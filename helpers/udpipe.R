library(udpipe)
library(plyr)
library(progress)

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

udpipe_extract_phrases <- function(text, pattern, as_columns = FALSE, model = NULL) {
  #'
  #' Extracts phrases with the given pattern from the given text.
  #'
  #' @param text the text
  #' @param pattern the (regex) pattern using POS tags:
  #'    A: adjective
  #'    C: coordinating conjuction
  #'    D: determiner
  #'    M: modifier of verb
  #'    N: noun or proper noun
  #'    P: preposition
  #'    O: other elements
  #' @param as_columns return the phrases as colums instead of rows, default is FALSE
  #' @param model the udpipe model to be used
  #' @return as data frame with phrases and frequency
  #'
  if (is.null(model)) {
    model <- udpipe_load_cached_model()
  }
  annotation <- udpipe_annotate(model, text) %>%
    as.data.frame() %>%
    mutate(phrase_tag = as_phrasemachine(upos, type = "upos"))
  result <- phrases(
    annotation$phrase_tag,
    term = annotation$token,
    pattern = pattern,
    is_regex = TRUE,
    detailed = FALSE
  ) %>%
    select(-ngram)
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
  #' @return as data frame with phrases and frequency per document
  #'

  # TODO: https://josephcrispell.github.io/2018/08/27/multi-threading-R.html
  bar <- progress_bar$new(total = length(corpus) / 10)
  model <- udpipe_load_cached_model()
  result <- data.frame()
  for (i in 1:length(corpus)) {
    if (i %% 10 == 0) {
      bar$tick()
    }
    row <- udpipe_extract_phrases(corpus[[i]], pattern, as_columns = TRUE, model = model)
    result <- rbind.fill(result, row)
  }
  result[is.na(result)] <- 0
  rownames(result) <- names(corpus)
  names(result) <- make.names(names(result))
  return(result)
}
