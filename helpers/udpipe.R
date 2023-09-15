library(cli)
library(dplyr)
library(parallel)
library(pbapply)
library(plyr)
library(quanteda)
library(udpipe)


udpipe_load_cached_model <- function() {
  #'
  #' Load the English udpipe model.
  #'
  #' Downloads the model, if not found in the "/cache" directory
  #'
  #' @return the English udpipe model
  #'
  filename <- udpipe_download_model(
    "english",
    model_dir = "cache",
    overwrite = FALSE
  )
  return(udpipe_load_model(filename))
}

udpipe_extract_phrases <- function(
    text, pattern, as_columns = FALSE, model = NULL, proper_nouns_only = TRUE
) {
  #'
  #' Extracts phrases with the given pattern from the given text.
  #'
  #' @param text the text
  #' @param pattern the (regex) pattern using POS tags:
  #'    A: adjective
  #'    C: coordinating conjuction
  #'    D: determiner
  #'    M: modifier of verb
  #'    N: (proper) noun
  #'    P: preposition
  #'    O: other elements
  #' @param as_columns return the phrases as colums instead of rows, default is FALSE
  #' @param model the udpipe model to be used
  #' @param proper_nouns_only if TRUE, pronouns are translated to O, else N
  #' @return as data frame with lemmatized phrases and frequency
  #'

  # load model if not provided
  if (is.null(model)) {
    model <- udpipe_load_cached_model()
  }

  # annotate parts of speech
  annotation <- udpipe_annotate(model, text) %>%
    as.data.frame() %>%
    mutate(phrase_tag = as_phrasemachine(upos, type = "upos"))

  # fix pronouns being categorized as nouns
  if (proper_nouns_only) {
    annotation <- annotation %>%
      mutate(phrase_tag = ifelse(upos == "PRON", "O", phrase_tag))
  }

  # extract phrases using lemmas
  result <- phrases(
    annotation$phrase_tag,
    term = annotation$lemma,
    pattern = pattern,
    is_regex = TRUE,
    detailed = FALSE
  ) %>%
    select(-ngram)

  # transform to data frame
  if (as_columns) {
    column_names <- result$keyword
    result <- data.frame(result$freq) %>% t() %>% as.data.frame()
    colnames(result) <- column_names
  }
  return(result)
}

udpipe_phrases <- function(corpus, pattern) {
  #'
  #' Create a DFM like data frame with phrases
  #'
  #' @param corpus the corpus
  #' @param pattern the (regex) pattern using POS tags:
  #'    A: adjective
  #'    C: coordinating conjuction
  #'    D: determiner
  #'    M: modifier of verb
  #'    N: noun or proper noun
  #'    P: preposition
  #'    O: other elements
  #' @return as DFM with phrases and frequency per document
  #'
  cluster <- makeCluster(detectCores())
  clusterExport(cluster, c("corpus", "pattern"), envir = environment())
  clusterExport(cluster, c("udpipe_extract_phrases","udpipe_load_cached_model"))
  clusterEvalQ(cluster, library(udpipe))
  clusterEvalQ(cluster, library(dplyr))
  clusterEvalQ(cluster, model <- udpipe_load_cached_model())

  cli_alert_info("Extracting phrases...")
  result <- pblapply(
    cl = cluster,
    X = names(corpus),
    FUN = function(name) {
      return(
        udpipe_extract_phrases(corpus[[name]], pattern, as_columns = TRUE, model = model)
      )
    }
  )
  stopCluster(cluster)

  cli_alert_info("Constructin a DFM")
  result <- rbind.fill(result)
  result[is.na(result)] <- 0
  rownames(result) <- names(corpus)
  names(result) <- make.names(names(result))
  return(as.dfm(result))
}
