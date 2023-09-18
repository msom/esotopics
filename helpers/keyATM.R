library(cli)
library(corrplot)
library(keyATM)
library(dplyr)
library(ggplot2)
library(philentropy)
library(topicmodels)
library(topicdoc)

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
    docs[,name] <- corpus[rownames(dfm)[docs[,name]]]
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

keyATM_topic_exclusiveness <- function(
    model, n = 10, include_others = FALSE, weight = 0.7
) {
  #'
  #' Calculate the topic exclusiveness of a keyATM model
  #'
  #' Taken from Luigi Curinis Big Data Analytics course
  #'
  #' @param model the keyATM model
  #' @param n the number of top words to consider, default is 10
  #' @param include_others if FALSE, only pre-defined topics are used, default is FALSE
  #' @param weight weight to apply for FREX calculation, default is 0.7
  #' @return a named vector with the exclusiveness of each topic
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

keyATM_fit_and_measure_model <- function(
    docs, dfm, keywords, no_keyword_topics, n = 10, iterations = 1500, seed = NULL
) {
  #'
  #' Fit a model and measure the coherence and exclusiveness.
  #'
  #' @param docs the keyATM docs
  #' @param dfm the document feature matrix
  #' @param keywords the keyATM keywords
  #' @param no_keyword_topics the number of non-keyword topics
  #' @param n the number of top words to consider, default is 10
  #' @param iterations the number of iterations, default is 1500
  #' @param seed the random seed, default is chosing a seed randomly
  #' @return a data frame with the number of topics, coherence and exclusivity
  #'
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
  result <- data.frame(
    topics=no_keyword_topics,
    coherence=mean(keyATM_topic_coherence(model, dfm, n = n)),
    overall_coherence=mean(keyATM_topic_coherence(model, dfm, n = n, include_others = TRUE)),
    exclusiveness=mean(keyATM_topic_exclusiveness(model, n = n)),
    overall_exclusiveness=mean(keyATM_topic_exclusiveness(model, n = n, include_others = TRUE))
  )
  return(result)
}

keyATM_find_no_keyword_topics <- function(
    docs, dfm, keywords, numbers, n = 10, iterations = 1500, seed = NULL,
    parallel = TRUE
) {
  #'
  #' Try a range of no_keyword_topics and return their coherence and exclusivity
  #'
  #' Taken from Luigi Curinis Big Data Analytics course
  #'
  #' @param docs the keyATM docs
  #' @param dfm the document feature matrix
  #' @param keywords the keyATM keywords
  #' @param n the number of top words to consider, default is 10
  #' @param iterations the number of iterations, default is 1500
  #' @param parallel fit models using the given number of cores or all
  #'                 available cores (TRUE), default is TRUE
  #' @param seed the random seed, default is chosing a seed randomly
  #' @return a data frame with the number of topics, coherence and exclusivity
  #'

  if (parallel != FALSE) {
    stopifnot(require(parallel))
    stopifnot(require(plyr))
    stopifnot(require(pbapply))

    nCores <- ifelse(parallel == TRUE, detectCores(), parallel)
    cluster <- makeCluster(nCores)
    clusterExport(
      cluster,
      c("docs", "dfm", "keywords", "n", "iterations","seed"),
      envir = environment()
    )
    clusterExport(
      cluster,
      c("keyATM_fit_and_measure_model", "keyATM_topic_coherence",
        "keyATM_topic_exclusiveness")
    )
    clusterEvalQ(cluster, library(dplyr))
    clusterEvalQ(cluster, library(keyATM))
    clusterEvalQ(cluster, library(quanteda))

    # Models with a lot of topics taking longer, we calculate them first so
    # that the progress bar estimation is pessimistic rather than optimistic
    # in the beginning
    result <- pblapply(
      cl = cluster,
      X = sort(numbers, decreasing = TRUE),
      FUN = function(no_keyword_topics) {
        tryCatch(
          {
            return(
              keyATM_fit_and_measure_model(
                docs, dfm, keywords, no_keyword_topics,
                n = n, iterations = iterations, seed = seed
              )
            )
          },
          error = function(e) {
            return(data.frame())
          }
        )
      }
    )
    stopCluster(cluster)
  } else {
    result <- list()
    for (number in numbers) {
      values <- keyATM_fit_and_measure_model(
        docs, dfm, keywords, number,
        n = n, iterations = iterations, seed = seed
      )
      cli_alert_info(
        sprintf(
          "%i topics: coherence %f, exclusiveness %f",
          number,
          round(mean(values$coherence), digits = 1),
          round(mean(values$exclusiveness), digits = 1)
        )
      )
      result[[length(result)+1]] <- values
    }
  }

  result <- rbind.fill(result) %>%
    mutate(
      c_scaled = scale(-coherence, center=1),
      e_scaled = scale(exclusiveness, center=1),
      metric = sqrt(c_scaled^2 + e_scaled^2)
    ) %>%
    select(-c_scaled, -e_scaled) %>%
    arrange(-metric)
  return(result)
}

keyATM_compare_models_by_words <- function(x, y, n = 10, include_others = FALSE) {
  #'
  #' Compare two models by comparing their top words.
  #'
  #' @param x the first keyATM model
  #' @param y the second keyATM model
  #' @param n the number of terms to include, only for by_distribution = FALSE, default is 10
  #' @param include_others if FALSE, only pre-defined topics are used, default is FALSE
  #' @return An k vector with the proportion of common top words
  #'
  match = ifelse(include_others, ".*", "\\d_.*")
  words_x <- top_words(x, n, show_keyword = FALSE) %>%
    select(matches(match))
  words_y <- top_words(y, n, show_keyword = FALSE) %>%
    select(matches(match))

  result <- vector(length = ncol(words_x))
  for (index in 1:length(colnames(words_x))) {
    name <- colnames(words_x)[index]
    result[index] <- length(unlist(intersect(words_x[name], words_y[name]))) / n
  }
  names(result) <- colnames(words_x)
  return(result)
}

keyATM_compare_models_by_distribution <- function(x, y) {
  #'
  #' Compare two models by comparing their distribution using the
  #' Jensen-Shannon divergence.
  #'
  #' Idea taken from https://datascience.stackexchange.com/a/87269
  #'
  #' @param x the first keyATM model
  #' @param y the second keyATM model
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
  names(result) <- colnames(words_x)
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
    geom_smooth(span = 0.1, se = FALSE) +
    facet_wrap(~ book, ncol = 1) +
    xlab("") +
    ylab("Theta") +
    labs(title = topic)
}

keyATM_plot_topic_occurrences <- function(model, dfm, topic) {
  #'
  #' Plot the occurrences of a topic within the documents.
  #'
  #' @param model the keyATM model
  #' @param dfm the esocorpus based DFM used with the model
  #'
  model$theta %>%
  as.data.frame() %>%
    mutate(name = rownames(spiritualism_dfm)) %>%
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
    ggplot(
      aes(x = paragraph, y = value, color = topic, linetype = other)
    ) +
    geom_smooth(span = 0.1, se = FALSE) +
    scale_linetype(guide = "none") +
    facet_wrap(~ book, ncol = 1) +
    xlab("") +
    ylab("Theta")
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
    mutate(name = rownames(dfm)) %>%
    separate_wider_delim(
      name,
      delim = "_",
      names = c("current", "name", "surname", "year", "title")
    ) %>%
    select(-name, -surname, -year, -title)

  for (name in unique(theta$current)) {
    theta %>%
      filter(current == name) %>%
      select(-current) %>%
      cor() %>%
      corrplot(title = name, method = "color", type = "lower")
  }
}

