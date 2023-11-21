library(cli)
library(corrplot)
library(datawizard)
library(dplyr)
library(ggplot2)
library(keyATM)
library(parallel)
library(pbapply)
library(philentropy)
library(plyr)
library(stringr)
library(tidyr)
library(topicdoc)
library(topicmodels)

keyATM_top_docs_texts <- function(
    model, corpus, dfm, n = 10, include_others = FALSE
) {
  #'
  #' Show the texts of the top documents of a keyATM model for each topics
  #'
  #' @param model the keyATM model
  #' @param corpus the quanteda corpus used to train the model
  #' @param dfm the dfm used to train the model
  #' @param n the number of texts to show, default is 10
  #' @param include_others if FALSE, only pre-defined topics are used, default is FALSE
  #' @return a n x k table with the texts of the top n documents for each topic
  #'
  match = ifelse(include_others, ".*", "\\d_.*")
  docs <- top_docs(model, n) %>%
    select(matches(match))
  for (name in colnames(docs)) {
    docs[,name] <- paste(
      rownames(dfm)[docs[,name]],
      corpus[rownames(dfm)[docs[,name]]],
      sep = ": "
    )
  }
  return(docs)
}

keyATM_topic_coherence <- function(
    model, dfm, n = 10, include_others = FALSE
) {
  #'
  #' Calculate the topic coherences of a keyATM model
  #'
  #' Taken from Luigi Curinis Big Data Analytics course
  #'
  #' @param model the keyATM model
  #' @param dfm the document feature matrix
  #' @param n the number of top words to consider, default is 10
  #' @param include_others if FALSE, only pre-defined topics are used, default is FALSE
  #' @return a named vector with the coherence of each topic
  #'
  match = ifelse(include_others, ".*", "\\d_.*")
  words <- top_words(model, n = n, show_keyword = FALSE) %>%
    select(matches(match))
  dtm <- convert(dfm, to = "topicmodels")
  result <- vector(length = ncol(words))
  for (i in 1:ncol(words)) {
    result[i] <- topicdoc:::coherence(dtm, words[,i], 1)
  }
  result <- setNames(result, colnames(words))
  return(result)
}

keyATM_topic_exclusivity <- function(
    model, n = 10, include_others = FALSE, weight = 0.7
) {
  #'
  #' Calculate the topic exclusivity of a keyATM model
  #'
  #' Taken from Luigi Curinis Big Data Analytics course
  #'
  #' @param model the keyATM model
  #' @param n the number of top words to consider, default is 10
  #' @param include_others if FALSE, only pre-defined topics are used, default is FALSE
  #' @param weight weight to apply for FREX calculation, default is 0.7
  #' @return a named vector with the exclusivity of each topic
  #'
  phi <- t(model$phi)
  mat <- phi / rowSums(phi)
  exclusivity <- apply(mat, 2, rank) / nrow(mat)
  frequency <- apply(phi, 2, rank) / nrow(mat)
  frex <- 1/(weight/exclusivity + (1 - weight)/frequency)
  index <- apply(phi, 2, order, decreasing = TRUE)[1:n, ]
  result <- vector(length = ncol(phi))
  for (i in 1:ncol(frex)) {
    result[i] <- sum(frex[index[, i], i])
  }
  result <- setNames(result, colnames(phi))
  match = ifelse(include_others, ".*", "\\d_.*")
  result <- subset(result, subset = grepl(match, names(result)))
  return(result)
}

keyATM_topic_ranksum <- function(model, keywords) {
  #'
  #' Calculate the sum of the ranks of the predefined keywords
  #'
  #' @param model the keyATM model
  #' @param keywords the predefined keywords
  #' @return a named vector with the ranksum of each topic normalized to 0...1,
  #'         the higher the value, the higher the ranks of the predefined keywords
  #'
  n <- nrow(model$theta)
  words <- top_words(model, n, show_keyword = FALSE) %>%
    select(matches("\\d_.*"))
  result <- vector(length = ncol(words))
  for (index in 1:length(keywords)) {
    name <- names(keywords[index])
    col <- sub(" ", "_", paste(index, name))
    indexes <- which(words[,index] %in% unlist(keywords[index]))
    result[index] <- sum(1:length(indexes)) / sum(indexes)
  }
  result <- setNames(result, colnames(words))
  return(result)
}

keyATM_topic_word_count <- function(model, threshold = 0.05, include_others = FALSE) {
  #'
  #' Calculate the number of words in a topic
  #'
  #' @param model the keyATM model
  #' @param threshold the theta value used for inclusion/exclusion, default is 0.05
  #' @param include_others if FALSE, only pre-defined topics are used, default is FALSE
  #' @return the number of words
  #'
  match = ifelse(include_others, ".*", "\\d_.*")
  result <- model$theta %>%
    as.data.frame() %>%
    pivot_longer(matches(match)) %>%
    select(name, value) %>%
    filter(value > threshold) %>%
    group_by(name) %>%
    dplyr::summarize(count = n())

  counts <- unlist(result["count"])
  counts <- setNames(counts, unlist(result["name"]))
  return(counts)
}


keyATM_save_model <- function(model, k, path) {
  #'
  #' Save a model to the file system
  #'
  #' @param model the model to save
  #' @param k the numbers of no-keyword topics
  #' @param path the path to safe the model
  saveRDS(model, file=paste0(path, "model_", k, ".Rds"))
}

keyATM_load_model <- function(k, path) {
  #'
  #' Load a model from file system
  #'
  #' @param k the numbers of no-keyword topics
  #' @param path the path to safe the model
  #' @return the modle
  return(readRDS(file=paste0(path, "model_", k, ".Rds")))
}

keyATM_fit_models <- function(
    docs, dfm, keywords, numbers, path, iterations = 1500, seed = NULL,
    parallel = TRUE
) {
  #'
  #' Try a range of no_keyword_topics and safe the models to the filesystem.
  #'
  #' @param docs the keyATM docs
  #' @param dfm the document feature matrix
  #' @param keywords the keyATM keywords
  #' @param numbers the numbers of no-keyword topics
  #' @param path the path to safe the model
  #' @param iterations the number of iterations, default is 1500
  #' @param parallel fit models using the given number of cores or all
  #'                 available cores (default)
  #' @param seed the random seed, default is chosing a seed randomly
  #' @return a data frame with the number of topics, coherence and exclusivity
  #'
  nCores <- ifelse(parallel == TRUE, detectCores(), parallel)
  cluster <- makeCluster(nCores)
  clusterExport(
    cluster,
    c("docs", "dfm", "keywords", "path", "iterations","seed"),
    envir = environment()
  )
  clusterExport(
    cluster,
    c("keyATM_save_model")
  )
  clusterEvalQ(cluster, library(dplyr))
  clusterEvalQ(cluster, library(keyATM))
  clusterEvalQ(cluster, library(quanteda))

  # Models with a lot of topics taking longer, we calculate them first so
  # that the progress bar estimation is pessimistic rather than optimistic
  # in the beginning. Note that shuffling the numbers won't work well, since
  # it seems that only batches of nCores are spawned at a time and then
  # waited for all cores to be finished before spanwning a new batch...
  result <- pblapply(
    cl = cluster,
    X = sort(numbers, decreasing = TRUE),
    FUN = function(no_keyword_topics) {
      tryCatch(
        {
          model <- keyATM(
            docs = docs,
            model = "base",
            no_keyword_topics = no_keyword_topics,
            keywords = keywords,
            options = list(
              seed = seed,
              iterations = iterations
            )
          )
          keyATM_save_model(model, no_keyword_topics, path)
        },
        error = function(e) {}
      )
    }
  )
  stopCluster(cluster)
}

keyATM_measure_models <- function(
    dfm, numbers, keywords, path, n = 10, threshold = 0.05
) {
  #'
  #' Measure the coherence, exclusivity and ranksum of different models.
  #'
  #' @param dfm the document feature matrix
  #' @param numbers the numbers of no-keyword topics
  #' @param keywords the keyATM keywords
  #' @param path the path where the models are saved
  #' @param n the number of top words to consider, default is 10
  #' @param threshold the theta value used for inclusion/exclusion of number of
  #'  words, default is 0.05
  #' @return a data frame with the number of topics, number of words, coherence,
  #'  exclusivity and ranksum
  #'

  result <- pblapply(
    numbers, function(topics) {
      model <- keyATM_load_model(topics, path)
      stopifnot(topics == model$no_keyword_topics)
      return(
        data.frame(
          topics=topics,
          word_count=mean(keyATM_topic_word_count(model, threshold = threshold)),
          coherence=mean(keyATM_topic_coherence(model, dfm, n = n)),
          exclusivity=mean(keyATM_topic_exclusivity(model, n = n)),
          ranksum=mean(keyATM_topic_ranksum(model, keywords))
        )
      )
    }
  )

  return(rbind.fill(result))
}

keyATM_compare_models_by_words <- function(x, y, m = 100, include_others = FALSE) {
  #'
  #' Compare two models by comparing their top words.
  #'
  #' @param x the first keyATM model
  #' @param y the second keyATM model
  #' @param m the maximum number of terms to include, default is 100
  #' @param include_others if FALSE, only pre-defined topics are used, default is FALSE
  #' @return An k vector with the proportion of common top words
  #'
  match = ifelse(include_others, ".*", "\\d_.*")
  words_x <- top_words(x, m, show_keyword = FALSE) %>%
    select(matches(match))
  words_y <- top_words(y, m, show_keyword = FALSE) %>%
    select(matches(match))

  result <- data.frame(matrix(NA, nrow = m-1, ncol = 1 + length(colnames(words_x))))
  colnames(result) <- c("n", colnames(words_x))
  result$n <- 2:m

  for (name in colnames(words_x)) {
    for (n in 2:m) {
      result[n-1, name] <- length(
        unlist(
          intersect(
            words_x[1:n, name],
            words_y[1:n, name])
        )
      ) / n
    }
  }

  return(result)
}

keyATM_compare_models_by_distribution <- function(x, y, include_others = FALSE) {
  #'
  #' Compare two models by comparing their distribution using the
  #' Jensen-Shannon divergence.
  #'
  #' Idea taken from https://datascience.stackexchange.com/a/87269
  #'
  #' @param x the first keyATM model
  #' @param y the second keyATM model
  #' @param include_others if FALSE, only pre-defined topics are used, default is FALSE
  #' @return An k vector with the distance value in the range 0 (similar) to 1 (distant)
  #'
  match = ifelse(include_others, ".*", "\\d_.*")

  # get the distributions and outer join them
  phi_x <- x$phi %>%
    t() %>%
    as.data.frame() %>%
    select(matches(match))
  colnames(phi_x) <- paste('x', colnames(phi_x), sep = '_')
  phi_y <- y$phi %>%
    t() %>%
    as.data.frame() %>%
    select(matches(match))
  colnames(phi_y) <- paste('y', colnames(phi_y), sep = '_')
  phi <- merge(phi_x, phi_y, by=0, all=TRUE)
  phi[is.na(phi)] <- 0

  result <- vector(length = ncol(phi_x))
  for (index in 1:length(colnames(phi_x))) {
    values <- rbind(
      phi[,colnames(phi_x)[index]],
      phi[,colnames(phi_y)[index]]
    )
    result[index] <- JSD(values, unit='log')
  }
  names(result) <- colnames(phi_x)
  return(result)
}

keyATM_keyword_search <- function(dfm, keywords) {
  #'
  #' Find the given keywords in the dfm
  #'
  #' @param dfm the DFM
  #' @param keywords the keyATM like keywords list
  #' @return A list with keyword occurrences for each topic
  #'
  df <- convert(dfm, to = "data.frame")
  rownames(df) <- rownames(dfm)

  result <- list()
  for (index in 1:length(keywords)) {
    terms <- keywords[index] %>%
      unlist() %>%
      as.vector()
    name <- names(keywords)[index]
    df_terms <- df %>%
      select(any_of(terms))
    result[[name]] <- df_terms %>%
      filter(if_any(colnames(df_terms), ~.x > 0))
  }
  return(result)
}

keyATM_plot_topic_occurrence <- function(model, dfm, topic)
{
  #'
  #' Plot the occurrence of a topic within the documents.
  #'
  #' @param model the keyATM model
  #' @param dfm the esocorpus based DFM used with the model
  #' @param topic the topic name
  #'
  model$theta %>%
    as.data.frame() %>%
    mutate(name = rownames(dfm)) %>%
    separate_wider_delim(
      name,
      delim = ".txt.",
      names = c("book", "paragraph")
    ) %>%
    mutate(
      paragraph = as.numeric(paragraph),
      book = as.factor(book),
    ) %>%
    ggplot(aes(x = paragraph, y = .data[[topic]])) +
    geom_line() +
    # geom_smooth(span = 0.1, se = FALSE) +
    facet_wrap(~ book, ncol = 1) +
    xlab("") +
    ylab("Theta") +
    labs(title = topic)
}

keyATM_plot_topic_occurrences <- function(model, dfm, include_others = FALSE) {
  #'
  #' Plot the occurrences of the topic within the documents.
  #'
  #' @param model the keyATM model
  #' @param dfm the esocorpus based DFM used with the model
  #'
  others = c(FALSE)
  if (include_others) {
    others = c(TRUE, FALSE)
  }
  model$theta %>%
    as.data.frame() %>%
    mutate(name = rownames(dfm)) %>%
    separate_wider_delim(
      name,
      delim = ".txt.",
      names = c("book", "paragraph")
    ) %>%
    gather(key = "topic", value = "value", c(-book, -paragraph)) %>%
    mutate(
      other = ifelse(startsWith(topic, "Other"), TRUE, FALSE),
      paragraph = as.numeric(paragraph),
      topic = as.factor(topic),
      book = as.factor(book),
    ) %>%
    filter(
      other %in% others
    ) %>%
    ggplot(
      aes(x = paragraph, y = value, color = topic, linetype = other)
    ) +
    geom_smooth(span = 0.01, se = FALSE) +
    scale_linetype(guide = "none") +
    facet_wrap(~ book, ncol = 1) +
    xlab("") +
    ylab("Theta")
}

keyATM_plot_keyword_occurrences <- function(dfm, keywords, topic) {
  #'
  #' Plot the occurrences of keywords within a DFM
  #'
  #' @param dfm the feature matrix
  #' @param keywords the keywords to search for
  #' @param topic the topic within the keywords to use
  #'
  occurrences <- keyATM_keyword_search(dfm, keywords)
  occurrences[[topic]] %>%
    mutate(name = rownames(.)) %>%
    separate_wider_delim(
      name,
      delim = ".txt.",
      names = c("book", "paragraph")
    ) %>%
    gather(key = "Keyword", value = "value", c(-book, -paragraph)) %>%
    mutate(
      paragraph = as.numeric(paragraph),
      Keyword = as.factor(Keyword)
    ) %>%
    filter(value > 0) %>%
    ggplot(aes(x = paragraph, y = value, color = Keyword)) +
    geom_col() +
    xlab("Paragraph") +
    ylab("Occurrence")
  # todo: scale x
}

keyATM_plot_topic_correlation <- function(model, dfm) {
  #'
  #' Plot the topic correlations within each current
  #'
  #' @param model the keyATM model
  #' @param dfm the esocorpus based DFM used with the model
  #'
  theta <- model$theta %>%
    as.data.frame() %>%
    mutate(current = docvars(dfm)$current)

  for (name in unique(theta$current)) {
    theta %>%
      filter(current == name) %>%
      select(-current) %>%
      cor() %>%
      corrplot(title = name, method = "color", type = "lower")
  }
}

keyATM_plot_topic_measure_scatter <- function(metrics, topic_range) {
  #'
  #' Plot the measures of a model as a scatter plot.
  #'
  #' @param metrics the keyATM model metrics
  #' @param topic_range the number of topics to show
  #'
  metrics %>%
    filter(topics %in% topic_range) %>%
    ggplot(aes(x=coherence, y=exclusivity, color=ranksum, size=word_count)) +
    geom_point() +
    geom_text_repel(mapping=aes(label=topics, size=500), vjust=-1.5) +
    scale_colour_gradient(
      high = "#132B43",
      low = "#56B1F7"
    ) +
    xlab("Coherence")  +
    ylab("Exclusivity") +
    labs(color = "Ranks (ISR)", size = "Words")
}

keyATM_plot_topic_measure_trend <- function(metrics, topic_range) {
  #'
  #' Plot the measures of a model as trend lines.
  #'
  #' @param metrics the keyATM model metrics
  #' @param topic_range the number of topics to show
  #'
  metrics %>%
    filter(topics %in% topic_range) %>%
    mutate(
      word_count=as.double(normalize(word_count)),
      coherence=as.double(normalize(coherence)),
      exclusivity=as.double(normalize(exclusivity)),
      ranksum=as.double(normalize(ranksum))
    ) %>%
    pivot_longer(
      c(word_count, coherence, exclusivity, ranksum),
      names_to = "measure"
    ) %>%
    ggplot(aes(x=topics, y=value, color=measure)) +
    geom_point() +
    geom_smooth(se = FALSE, method = "lm", formula= (y ~ log(x)), linewidth = 0.5, linetype = "dashed") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_discrete(labels = c("Coherence", "Exclusivity", "Ranks (ISR)", "Words")) +
    xlab("Topics") +
    ylab("Normalized Value") +
    labs(color = "Measure")
}

keyATM_plot_histogram <- function(model) {
  #'
  #' Plot the historgram of the words of a model
  #'
  #' @param model the keyATM model
  #'
  totals <- keyATM_topic_word_count(model)
  model$theta %>%
    as.data.frame() %>%
    pivot_longer(matches("\\d_.*")) %>%
    select(name, value) %>%
    mutate(
      name = name %>%
        str_replace("\\d_", "") %>%
        str_replace_all("_", " ") %>%
        str_to_title() %>%
        paste0(" (", totals[name], ")")
    ) %>%
    filter(value > 0.05) %>%
    ggplot(mapping=aes(x=value)) +
    geom_histogram() +
    facet_wrap(~name) +
    xlab("Theta") +
    ylab("Count")
}

