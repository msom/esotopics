library(cli)
library(corrplot)
library(datawizard)
library(dplyr)
library(ggplot2)
library(ggpointdensity)
library(ggrepel)
library(keyATM)
library(parallel)
library(pbapply)
library(philentropy)
library(quanteda)
library(simplermarkdown)
library(stringr)
library(tidyr)
library(topicdoc)
library(topicmodels)

source("helpers/predict.R")

keyatm_topic_names <- function(names) {
  #'
  #' Returns nice topic names
  #'
  #' @param names the topic names
  #' @return nicer topic names
  #'
  result <- names %>%
    str_replace("\\d_", "") %>%
    str_replace_all("_", " ") %>%
    str_to_title() %>%
    str_replace("Seance", "Séance")
  return(result)
}

keyatm_print_top_words_table <- function(model, n = 10, include_others = FALSE) {
  #'
  #' Show the texts of the top documents of a keyATM model for each topics
  #'
  #' @param model the keyATM model
  #' @param n the number of words to show, default is 10
  #' @param include_others if FALSE, only pre-defined topics are used, default is FALSE
  #'
  match <- ifelse(include_others, ".*", "\\d_.*")
  docs <- top_words(model, n) %>%
    select(matches(match)) %>%
    mutate_all(str_replace_all, "\\.", " ") %>%
    mutate_all(str_replace_all, "(.*) \\[✓\\]", "*\\1*") %>%
    mutate_all(str_replace_all, " \\[\\d\\]", "")
  names(docs) <- keyatm_topic_names(names(docs))
  docs %>%
    md_table()
}

keyatm_top_docs_texts <- function(model, corpus, dfm, n = 10, include_others = FALSE) {
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
  match <- ifelse(include_others, ".*", "\\d_.*")

  # we cannot use top_docs with our model-like lists
  measuref <- function(xcol) {
    order(xcol, decreasing = TRUE)[1:n]
  }
  docs <- as.data.frame(apply(model$theta, 2, measuref), stringsAsFactors = FALSE) %>%
    select(matches(match))

  for (name in colnames(docs)) {
    indices <- docs[, name]
    thetas <- format(round(model$theta[indices, name], 4), scientific = FALSE)
    texts <- corpus[rownames(dfm)[indices]]
    docs[, name] <- paste0(
      rownames(dfm)[indices] %>%
        str_replace("\\.txt\\.", " ") %>%
        str_replace("_(\\d{4})(_\\d)*", " (\\1)"),
      str_glue(" [{thetas}]: {texts}")
    )
  }
  return(docs)
}

keyatm_save_top_docs_texts <- function(texts, file) {
  #'
  #' Creates a nicely formatted markdown file with the given texts
  #'
  #' @param texts the result of keyatm_top_docs_texts
  #' @param file the file name
  #'
  cat(file = file)
  for (topic in 1:ncol(texts)) {
    cat(str_glue("\n\n# {keyatm_topic_names(names(texts))[topic]}\n\n"), file = file, append = TRUE)
    for (document in 1:nrow(texts)) {
      cat(str_glue("\n\n## {document}: "), file = file, append = TRUE)
      cat(
        texts[document, topic] %>%
          str_replace(": ", "\n\n>") %>%
          str_replace_all("[ ]+", " ")
        ,
        file = file, append = TRUE
      )
    }
  }
}

keyatm_topic_coherence <- function(model, dfm, n = 10, include_others = FALSE) {
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
  match <- ifelse(include_others, ".*", "\\d_.*")
  words <- top_words(model, n = n, show_keyword = FALSE) %>%
    select(matches(match))
  dtm <- convert(dfm, to = "topicmodels")
  result <- vector(length = ncol(words))
  for (i in 1:ncol(words)) {
    result[i] <- topicdoc:::coherence(dtm, words[, i], 1)
  }
  result <- setNames(result, colnames(words))
  return(result)
}

keyatm_topic_exclusivity <- function(model, n = 10, include_others = FALSE, weight = 0.7) {
  #'
  #' Calculate the topic exclusivity of a keyATM model
  #'
  #' Taken from Luigi Curinis Big Data Analytics course
  #'
  #' @param model the keyATM model
  #' @param n the number of top words to consider, default is 10
  #' @param include_others if FALSE, only pre-defined topics are used, defaultis FALSE
  #' @param weight weight to apply for FREX calculation, default is 0.7
  #' @return a named vector with the exclusivity of each topic
  #'
  phi <- t(model$phi)
  mat <- phi / rowSums(phi)
  exclusivity <- apply(mat, 2, rank) / nrow(mat)
  frequency <- apply(phi, 2, rank) / nrow(mat)
  frex <- 1 / (weight / exclusivity + (1 - weight) / frequency)
  index <- apply(phi, 2, order, decreasing = TRUE)[1:n, ]
  result <- vector(length = ncol(phi))
  for (i in 1:ncol(frex)) {
    result[i] <- sum(frex[index[, i], i])
  }
  result <- setNames(result, colnames(phi))
  match <- ifelse(include_others, ".*", "\\d_.*")
  result <- subset(result, subset = grepl(match, names(result)))
  return(result)
}

keyatm_topic_ranksum <- function(model, keywords) {
  #'
  #' Calculate the sum of the ranks of the predefined keywords
  #'
  #' @param model the keyATM model
  #' @param keywords the predefined keywords
  #' @return a named vector with the ranksum of each topic normalized to 0...1, the higher the
  #'  value, the higher the ranks of the predefined keywords
  #'
  n <- nrow(model$theta)
  words <- top_words(model, n, show_keyword = FALSE) %>%
    select(matches("\\d_.*"))
  result <- vector(length = ncol(words))
  for (index in 1:length(keywords)) {
    name <- names(keywords[index])
    col <- sub(" ", "_", paste(index, name))
    indexes <- which(words[, index] %in% unlist(keywords[index]))
    result[index] <- sum(1:length(indexes)) / sum(indexes)
  }
  result <- setNames(result, colnames(words))
  return(result)
}

keyatm_topic_feature_count <- function(model, threshold = 1, include_others = FALSE) {
  #'
  #' Calculate the number of words in a topic
  #'
  #' @param model the keyATM model
  #' @param threshold the phi value used for inclusion/exclusion as a multiple of the uniform
  #'  distribution value, default is 1
  #' @param include_others if FALSE, only pre-defined topics are used, default is FALSE
  #' @return the number of word
  #'
  threshold_phi <- threshold / ncol(model$phi)
  match <- ifelse(include_others, ".*", "\\d_.*")
  result <- model$phi %>%
    t() %>%
    as.data.frame() %>%
    pivot_longer(matches(match)) %>%
    select(name, value) %>%
    filter(value > threshold_phi) %>%
    group_by(name) %>%
    summarize(count = n())

  counts <- unlist(result["count"])
  counts <- setNames(counts, unlist(result["name"]))
  return(counts)
}

keyatm_topic_document_count <- function(model, threshold = 1, include_others = FALSE) {
  #'
  #' Calculate the number of documents relevant for a topic
  #'
  #' @param model the keyATM model
  #' @param threshold the theta value used for inclusion/exclusion as a multiple of the uniform
  #'  distribution value, default is 1
  #' @param include_others if FALSE, only pre-defined topics are used, default is FALSE
  #' @return the number of topics
  #'
  threshold_theta <- threshold / (model$no_keyword_topics + model$keyword_k)
  match <- ifelse(include_others, ".*", "\\d_.*")
  result <- model$theta %>%
    as.data.frame() %>%
    pivot_longer(matches(match)) %>%
    select(name, value) %>%
    filter(value > threshold_theta) %>%
    group_by(name) %>%
    summarize(count = n())

  counts <- unlist(result["count"])
  counts <- setNames(counts, unlist(result["name"]))
  return(counts)
}

keyatm_save_model <- function(model, k, seed, path) {
  #'
  #' Save a model to the file system
  #'
  #' @param model the model to save
  #' @param k the numbers of no-keyword topics
  #' @param seed the random seed
  #' @param path the path to safe the model
  saveRDS(model, file = paste0(path, "model_", k, "_", seed, ".Rds"))
}

keyatm_load_model <- function(k, seed, path) {
  #'
  #' Load a model from file system
  #'
  #' @param k the numbers of no-keyword topics
  #' @param seed the random seed
  #' @param path the path to safe the model
  #' @return the modle
  return(readRDS(file = paste0(path, "model_", k, "_", seed, ".Rds")))
}

keyatm_fit_models <- function(
  docs, dfm, keywords, numbers, path, iterations = 1500, seed = NULL, parallel = TRUE
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
  #' @param parallel fit models using the given number of cores or all available cores (default)
  #' @param seed the random seed, default is chosing a seed randomly
  #' @return a data frame with the number of topics, coherence and exclusivity
  #'
  n_cores <- ifelse(parallel == TRUE, detectCores(), parallel)
  cluster <- makeCluster(n_cores)
  clusterExport(
    cluster,
    c("docs", "dfm", "keywords", "path", "iterations", "seed"),
    envir = environment()
  )
  clusterExport(cluster, c("keyatm_save_model"))
  clusterEvalQ(cluster, library(dplyr))
  clusterEvalQ(cluster, library(keyATM))
  clusterEvalQ(cluster, library(quanteda))

  # Models with a lot of topics taking longer, we calculate them first so that the progress bar
  # estimation is pessimistic rather than optimistic in the beginning. Note that shuffling the
  # numbers won't work well, since it seems that only batches of n_cores are spawned at a time
  # and then waited for all cores to be finished before spanwning a new batch...
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
            options = list(seed = seed, iterations = iterations)
          )
          keyatm_save_model(model, no_keyword_topics, seed, path)
        },
        error = function(e) {}
      )
    }
  )
  stopCluster(cluster)
  return(result)
}

keyatm_measure_models <- function(
  dfm, numbers, keywords, seed, path, n = 10, threshold_docs = 1, threshold_features = 1
) {
  #'
  #' Measure the coherence, exclusivity and ranksum of different models.
  #'
  #' @param dfm the document feature matrix
  #' @param numbers the numbers of no-keyword topics
  #' @param keywords the keyATM keywords
  #' @param seed the random seed
  #' @param path the path where the models are saved
  #' @param n the number of top words to consider, default is 10
  #' @param threshold_docs the theta value used for inclusion/exclusion as a multiple of the
  #'  uniform distribution value, default is 1
  #' @param threshold_features the phi value used for inclusion/exclusion as a multiple of the
  #'  uniform distribution value, default is 1
  #' @return a data frame with the number of topics, number of words, coherence, exclusivity and
  #'  ranksum
  #'

  result <- pblapply(
    numbers, function(topics) {
      model <- keyatm_load_model(topics, seed, path)
      stopifnot(topics == model$no_keyword_topics)
      return(
        data.frame(
          topics = topics,
          document_count = mean(keyatm_topic_document_count(model, threshold = threshold_docs)),
          feature_count = mean(keyatm_topic_feature_count(model, threshold = threshold_features)),
          coherence = mean(keyatm_topic_coherence(model, dfm, n = n)),
          exclusivity = mean(keyatm_topic_exclusivity(model, n = n)),
          ranksum = mean(keyatm_topic_ranksum(model, keywords))
        )
      )
    }
  )

  return(plyr::rbind.fill(result))
}


keyatm_keyword_search <- function(
  dfm, keywords, drop_empty_rows = TRUE, drop_empty_columns = TRUE, summarized = FALSE,
  categorical = FALSE
) {
  #'
  #' Find the given keywords in the dfm
  #'
  #' @param dfm the DFM
  #' @param keywords the keyATM like keywords list
  #' @param drop_empty_rows if TRUE, drops rows with no hit, default is TRUE
  #' @param drop_empty_columns if TRUE, drops columns with no hit, default is TRUE
  #' @param summarized if TRUE, only returns the sum of keyword occurrences instead occurrences
  #'  by keywords, default is FALSE
  #' @param categorical if TRUE, only returns summarized occurrences as hit (1) / no hit (0),
  #'  default is FALSE
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
    if (drop_empty_rows) {
      df_terms <- df_terms %>%
        filter(if_any(colnames(df_terms), ~.x > 0))
    }
    if (drop_empty_columns) {
      df_terms <- df_terms %>%
        select(which(colSums(., na.rm = TRUE) > 0))
    }
    if (summarized) {
      df_terms <- df_terms %>%
        transmute(occurrences = rowSums(.))
      if (categorical) {
        df_terms <- df_terms %>%
          mutate(occurrences = ifelse(occurrences > 0, 1, 0))
      }
    }
    result[[name]] <- df_terms
  }
  return(result)
}

keyatm_plot_topic_occurrences <- function(model, dfm, include_others = FALSE, path = NA) {
  #'
  #' Plot the occurrence of the topics within the documents.
  #'
  #' @param model the keyATM model
  #' @param dfm the DFM used with the model
  #' @param include_others if FALSE, only pre-defined topics are used, default is FALSE
  #' @param path the path to safe the model
  #' @return a list of plots
  #'
  theta <- as.data.frame(model$theta)
  match <- ifelse(include_others, ".*", "\\d_.*")
  topics <-  theta %>%
    select(matches(match)) %>%
    names()

  result <- list()
  for (topic in topics) {
    figure <- theta %>%
      mutate(name = rownames(dfm)) %>%
      separate_wider_delim(name, delim = ".txt.", names = c("book", "paragraph")) %>%
      mutate(
        paragraph = as.numeric(paragraph),
        book = as.factor(
          book %>%
            str_replace(., "_(\\d{4})", " \\(\\1\\)") %>%
            str_replace("_1", "")
        )
      ) %>%
      ggplot(aes(x = paragraph, y = .data[[topic]])) +
      geom_line() +
      facet_wrap(~ book, ncol = 1) +
      xlab("Paragraph") +
      ylab("Theta") +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )
    if (!is.na(path)) {
      ggsave(paste0(path, "occurrence_", topic, ".pdf"), width = 4, height = 8)
    }
    result[[topic]] <- figure
  }
  return(result)
}

keyatm_plot_keyword_occurrences <- function(dfm, keywords, topic) {
  #'
  #' Plot the occurrences of keywords within a DFM
  #'
  #' @param dfm the feature matrix
  #' @param keywords the keywords to search for
  #' @param topic the topic within the keywords to use
  #'
  occurrences <- keyatm_keyword_search(dfm, keywords)
  occurrences[[topic]] %>%
    mutate(name = rownames(.)) %>%
    separate_wider_delim(name, delim = ".txt.", names = c("book", "paragraph")) %>%
    gather(key = "Keyword", value = "value", c(-book, -paragraph)) %>%
    mutate(paragraph = as.numeric(paragraph), Keyword = as.factor(Keyword)) %>%
    filter(value > 0) %>%
    ggplot(aes(x = paragraph, y = value, color = Keyword, fill = Keyword)) +
    geom_col() +
    xlim(0, nrow(dfm)) +
    xlab("Paragraph") +
    ylab("Occurrence")
}

keyatm_plot_topic_measure_scatter <- function(
  metrics, topic_range, highlight = NULL
) {
  #'
  #' Plot the measures of a model as a scatter plot.
  #'
  #' @param metrics the keyATM model metrics
  #' @param topic_range the number of topics to show
  #' @param hightlight option metrics to highlight using a different shape
  #'
  data <- metrics %>%
    mutate(shape = 17)
  if (!is.null(highlight)) {
    data[highlight, ]$shape <- 19
  }
  data %>%
    filter(topics %in% topic_range) %>%
    ggplot(
      aes(
        x = coherence,
        y = exclusivity,
        color = ranksum,
        size = feature_count,
        shape = as.factor(shape)
      )
    ) +
    geom_point() +
    geom_text_repel(mapping = aes(label = topics), size = 4) +
    scale_colour_gradient(high = "#132B43", low = "#56B1F7") +
    scale_shape_discrete(guide = "none") +
    xlab("Coherence")  +
    ylab("Exclusivity") +
    labs(color = "Ranks (ISR)", size = "Features")
}

keyatm_plot_topic_measure_trend <- function(metrics, topic_range) {
  #'
  #' Plot the measures of a model as trend lines.
  #'
  #' @param metrics the keyATM model metrics
  #' @param topic_range the number of topics to show
  #'
  metrics %>%
    filter(topics %in% topic_range) %>%
    mutate(
      document_count = as.double(normalize(document_count)),
      feature_count = as.double(normalize(feature_count)),
      coherence = as.double(normalize(coherence)),
      exclusivity = as.double(normalize(exclusivity)),
      ranksum = as.double(normalize(ranksum))
    ) %>%
    pivot_longer(
      c(document_count, feature_count, coherence, exclusivity, ranksum),
      names_to = "measure"
    ) %>%
    ggplot(aes(x = topics, y = value, color = measure, shape = measure)) +
    geom_point(position = position_jitter(height = 0, width = 10), size = 3) +
    geom_smooth(se = FALSE, linewidth = 0.5, span = 10, linetype = "dashed") +
    scale_y_continuous(limits = c(0, 1.01)) +
    scale_color_discrete(
      name = "Measure",
      labels = c("Coherence", "Documents", "Exclusivity", "Features", "Ranks (ISR)")
    ) +
    scale_shape_discrete(
      name = "Measure",
      labels = c("Coherence", "Documents", "Exclusivity", "Features", "Ranks (ISR)")
    ) +
    xlab("Topics") +
    ylab("Normalized Value")
}

keyatm_plot_document_histogram <- function(model, threshold = 1) {
  #'
  #' Plot the histogram of the documents of a model
  #'
  #' @param model the keyATM model
  #' @param threshold the theta value used for inclusion/exclusion as a multiple
  #'  of the uniform distribution value, default is 1
  #'
  totals <- keyatm_topic_document_count(model, threshold)
  threshold_theta <- threshold / (model$no_keyword_topics + model$keyword_k)
  model$theta %>%
    as.data.frame() %>%
    pivot_longer(matches("\\d_.*")) %>%
    select(name, value) %>%
    mutate(
      name = name %>%
        keyatm_topic_names() %>%
        paste0(" (", totals[name], ")")
    ) %>%
    filter(value > threshold_theta) %>%
    ggplot(mapping = aes(x = value)) +
    geom_histogram() +
    facet_wrap(~name) +
    xlab("Normalized Theta") +
    ylab("Count")
}

keyatm_plot_feature_histogram <- function(model, threshold = 1) {
  #'
  #' Plot the histogram of the words of a model
  #'
  #' @param model the keyATM model
  #' @param threshold the phi value used for inclusion/exclusion as a multiple
  #'  of the uniform distribution value, default is 1
  #'
  totals <- keyatm_topic_feature_count(model, threshold)
  threshold_phi <- threshold / ncol(model$phi)
  model$phi %>%
    t() %>%
    as.data.frame() %>%
    pivot_longer(matches("\\d_.*")) %>%
    select(name, value) %>%
    mutate(
      name = name %>%
        keyatm_topic_names() %>%
        paste0(" (", totals[name], ")")
    ) %>%
    filter(value > threshold_phi) %>%
    ggplot(mapping = aes(x = value)) +
    geom_histogram() +
    facet_wrap(~name) +
    xlab("Phi") +
    ylab("Count")
}

keyatm_plot_top_docs_length <- function(texts, ymax = NA) {
  #'
  #' Plot the length of the top documents of a model include SE
  #'
  #' @param texts the result of keyatm_top_docs_texts
  #' @param ymax optional ymax
  #'
  plot <- texts %>%
    pivot_longer(everything()) %>%
    mutate(name = keyatm_topic_names(name), value = nchar(value)) %>%
    group_by(name) %>%
    summarize(n = n(), mean = mean(value), sd = sd(value), se = sd / sqrt(n)) %>%
    ggplot(aes(x = name, y = mean)) +
    geom_bar(stat = "identity", fill = "#256a9e") +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.3) +
    xlab("Topic") +
    ylab("Length")
  if (!is.na(ymax)) {
    plot <- plot + ylim(c(0, ymax))
  }
  return(plot)
}


keyatm_print_occurrences_table <- function(
  model, dfm, threshold = 1, include_others = FALSE, number_of_rows = NA
) {
  #'
  #' Print a nicely formatted markdown table with occurrences
  #'
  #' @param model the keyATM model
  #' @param dfm the DFM used with the model
  #' @param threshold the phi value used for inclusion/exclusion as a multiple of the uniform
  #'  distribution value, default is 1
  #' @param include_others if FALSE, only pre-defined topics are used, default is FALSE
  #' @param number_of_rows an optional integer for considering a maximum number of rows to count
  #'
  match <- ifelse(include_others, ".*", "\\d_.*")
  threshold_theta <- threshold / (model$no_keyword_topics + model$keyword_k)
  occurrences <- model$theta %>%
    as.data.frame() %>%
    select(matches(match)) %>%
    mutate(name = rownames(dfm)) %>%
    separate_wider_delim(name, delim = ".txt.", names = c("book", "paragraph")) %>%
    mutate(
      paragraph = as.numeric(paragraph),
      book = as.factor(
        book %>%
          str_replace(., "_(\\d{4})", " \\(\\1\\)") %>%
          str_replace("_1", "")
      )
    ) %>%
    pivot_longer(-c("book", "paragraph"), names_to = "topic", values_to = "theta") %>%
    arrange(topic, -theta) %>%
    filter(theta > threshold_theta)

  if (!is.na(number_of_rows)) {
    occurrences <- occurrences %>%
      group_by(topic) %>%
      slice_head(n = number_of_rows) %>%
      ungroup()
  }

  occurrences %>%
    select(-paragraph, -theta) %>%
    group_by(book) %>%
    plyr::count() %>%
    ungroup() %>%
    mutate(topic = keyatm_topic_names(topic)) %>%
    pivot_wider(names_from = topic, values_from = freq) %>%
    replace(is.na(.), 0) %>%
    rename(Book = book) %>%
    relocate(
      Book,
      model$theta %>%
        as.data.frame() %>%
        select(matches(match)) %>%
        colnames() %>%
        keyatm_topic_names()
    ) %>%
    md_table()
}

keyatm_calculate_model_statistics <- function(
  model, dfm, keywords, intruder_features, intruder_documents
) {
  #'
  #' Calculate various statistics of a model
  #'
  #' @param model the keyATM model
  #' @param dfm the DFM
  #' @param keywords the keywords
  #' @param intruder_features the intruder features score
  #' @param intruder_documents the intruder documents score
  #' @return a data frame with model statistics.
  #'
  result <- data.frame(
    feature_count = keyatm_topic_feature_count(model),
    document_count = keyatm_topic_document_count(model),
    coherence = keyatm_topic_coherence(model, dfm),
    exclusivity = keyatm_topic_exclusivity(model),
    ranksum = keyatm_topic_ranksum(model, keywords),
    intruder_features = intruder_features,
    intruder_documents = intruder_documents
  )
  return(result)
}

keyatm_print_model_statistics_table <- function(
  statistics, total_features, total_documents, test_coherence = TRUE,
  hide = NA
) {
  #'
  #' Print a nicely formatted markdown table with model statistics.
  #'
  #' @param statistics the keyATM model statistics
  #' @param total_features the total number of features
  #' @param total_documents the total number of documents
  #' @param test_coherence test and output low coherences (using a two-sided 95% interval)
  #' @param hide a list of rows to hide
  #'
  df <- statistics %>%
    mutate(topic = keyatm_topic_names(row.names(statistics))) %>%
    add_row(
      topic = "Mean",
      feature_count = round(mean(.$feature_count)),
      document_count = round(mean(.$document_count)),
      coherence = mean(.$coherence),
      exclusivity = mean(.$exclusivity),
      ranksum = mean(.$ranksum),
      intruder_features = mean(.$intruder_features),
      intruder_documents = mean(.$intruder_documents)
    ) %>%
    mutate(
      feature_count = paste0(
        feature_count, " (",
        format(round(100 * feature_count / total_features, 1), nsmall = 1), " %)"
      ),
      document_count = paste0(
        document_count, " (",
        format(round(100 * document_count / total_documents, 1), nsmall = 1), " %)"
      ),
      coherence = format(round(coherence)),
      exclusivity = format(round(exclusivity, 1), nsmall = 1),
      ranksum = format(round(ranksum, 2), nsmall = 2),
      intruder_features = ifelse(
        is.na(intruder_features), "NA",
        format(round(intruder_features, 2), nsmall = 2)
      ),
      intruder_documents = ifelse(
        is.na(intruder_documents), "NA",
        format(round(intruder_documents, 2), nsmall = 2)
      )
    ) %>%
    add_row(
      topic = "---",
      feature_count = "---",
      document_count = "---",
      coherence = "---",
      exclusivity = "---",
      ranksum = "---",
      intruder_features = "---",
      intruder_documents = "---",
      .before = nrow(.)
    ) %>%
    rename(
      Topic = topic,
      Features = feature_count,
      Documents = document_count,
      Coherence = coherence,
      Exclusivity = exclusivity,
      ISR = ranksum,
      IFS = intruder_features,
      IDS = intruder_documents,
    ) %>%
    relocate(Topic)

  if (any(!is.na(hide))) {
    df <- df %>%
      select(-hide)
  }

  df %>%
    md_table()

  if (test_coherence) {
    lower <- t.test(statistics$coherence, mu = mean(statistics$coherence))$conf.int[1]
    names <- row.names(statistics[which(statistics$coherence < lower), ])
    print(str_glue("\n\nCoherence 95%: {lower} ({paste(names)})"))
  }
}

keyatm_search_to_model <- function(dfm, keywords) {
  #'
  #' Creates a keyATM model like list from a keyword search
  #'
  #' @param dfm the DFM
  #' @param keywords the keyATM like keywords list
  #' @return A list containing occurrences as thetas and some topic information
  #'
  theta <- keyatm_keyword_search(
    dfm, keywords,
    drop_empty_rows = FALSE,
    drop_empty_columns = FALSE,
    summarized = TRUE,
    categorical = FALSE
  )
  theta <- do.call(cbind, theta)
  colnames(theta) <- paste(1:length(keywords), names(keywords), sep = "_")
  rownames(theta) <- NULL

  result <- list(theta = as.matrix(theta), no_keyword_topics = 0, keyword_k = length(keywords))
  return(result)
}

keyatm_predict <- function(docs, model, n = 300) {
  #'
  #' Predicts topic proportions
  #'
  #' Taken from https://github.com/keyATM/keyATM/issues/187, but using mean instead of median.
  #'
  #' @param docs a keyATM documents object
  #' @param model a keyATM model
  #' @param n number of iterations
  #' @return A data frame containing the estimated theta values
  #'
  result <- pred_topicprop(model, docs, iter_num = n)
  result <- sapply(result, function(x) {apply(x, 2, mean)}) %>%
    t() %>%
    as.data.frame()
  return(result)
}

keyatm_predict_top_doc <- function(theta, include_others = FALSE) {
  #'
  #' Returns the top document per topic for predicted topics
  #'
  #' @param theta a data frame with thetas from keyatm_predict
  #' @param include_others if FALSE, only pre-defined topics are used, default is FALSE
  #' @return A data theta and document id
  #'
  match <- ifelse(include_others, ".*", "\\d_.*")
  result <- theta %>%
    select(matches(match)) %>%
    pivot_longer(everything()) %>%
    group_by(name) %>%
    summarise(theta=max(value), docid=which.max(value),) %>%
    ungroup()

  return(result)
}
