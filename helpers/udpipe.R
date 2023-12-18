library(cli)
library(dplyr)
library(hash)
library(parallel)
library(pbapply)
library(progress)
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
  text, pattern, as_columns = FALSE, model = NULL, proper_nouns_only = TRUE,
  adverbs_only = FALSE, nouns = NULL, noun_tuples = NULL, verbs = NULL
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
  #'    V: verb
  #'    O: other elements
  #' @param as_columns return the phrases as colums instead of rows,
  #'    default is FALSE
  #' @param model the udpipe model to be used
  #' @param proper_nouns_only if TRUE, pronouns are translated to O, else N
  #' @param adverbs_only if TRUE, particles categorized as O, else M
  #' @param nouns an optional list of nouns (lemmas) to include
  #' @param noun_tuples an optional list of noun tuples (lemmas) to include,
  #'    these are nouns co-occuring in a single document treated as a phrase
  #' @param verbs an optional list of verbs (lemmas) to include
  #' @return a data frame with lemmatized phrases and frequency
  #'

  # load model if not provided
  if (is.null(model)) {
    model <- udpipe_load_cached_model()
  }

  # annotate parts of speech
  annotations <- udpipe_annotate(model, tolower(text)) %>%
    as.data.frame() %>%
    mutate(phrase_tag = as_phrasemachine(upos, type = "upos"))

  # fix pronouns being categorized as nouns
  if (proper_nouns_only) {
    annotations <- annotations %>%
      mutate(phrase_tag = ifelse(upos == "PRON", "O", phrase_tag))
  }

  # fix particles being classified as verb
  if (adverbs_only) {
    annotations <- annotations %>%
      mutate(phrase_tag = ifelse(upos == "PART", "O", phrase_tag))
  }

  # extract phrases using lemmas
  result <- phrases(
    annotations$phrase_tag,
    term = annotations$lemma,
    pattern = pattern,
    is_regex = TRUE,
    detailed = FALSE
  ) %>%
    select(-ngram)

  if (!is.null(nouns) || !is.null(noun_tuples)) {
    noun_phrases <- phrases(
      annotations$phrase_tag,
      term = annotations$lemma,
      pattern = "N",
      is_regex = TRUE,
      detailed = FALSE
    )
    if (!is.null(nouns)) {
      # FIXME: rbind might create duplicate entries when pattern is "N"
      result <- result %>% rbind(
        noun_phrases %>%
          select(-ngram) %>%
          filter(keyword %in% nouns)
      )
    }
    if (!is.null(noun_tuples)) {
      for (noun_tuple in noun_tuples) {
        intersection <- intersect(noun_tuple, noun_phrases$keyword)
        if (length(intersection) == length(noun_tuple)) {
          name <- paste(noun_tuple, collapse = "-")
          # FIXME: this might create duplicate entries when pattern is "NN+"
          result[nrow(result) + 1, ] <- list(name, 1)
        }
      }
    }
  }

  if (!is.null(verbs)) {
    # FIXME: rbind might create duplicate entries when patterns is "V"
    result <- result %>%
      rbind(
        phrases(
          annotations$phrase_tag,
          term = annotations$lemma,
          pattern = "V",
          is_regex = TRUE,
          detailed = FALSE
        ) %>%
          select(-ngram) %>%
          filter(keyword %in% verbs)
      )
  }

  # transform to data frame
  if (as_columns) {
    column_names <- result$keyword
    result <- data.frame(result$freq) %>%
      t() %>%
      as.data.frame()
    colnames(result) <- column_names
  }
  return(result)
}

udpipe_phrases <- function(
  corpus, pattern, nouns = NULL, noun_tuples = NULL, verbs = NULL,
  adverbs_only = FALSE
) {
  #'
  #' Create a DFM with phrases.
  #'
  #' @param corpus the corpus
  #' @param pattern the (regex) pattern using POS tags:
  #'    A: adjective
  #'    C: coordinating conjuction
  #'    D: determiner
  #'    M: modifier of verb
  #'    N: noun or proper noun
  #'    P: preposition
  #'    V: verb
  #'    O: other elements
  #' @param nouns an optional list of nouns (lemmas) to include
  #' @param noun_tuples an optional list of noun tuples (lemmas) to include,
  #'    these are nouns co-occuring in a single document treated as a phrase
  #' @param verbs an optional list of verbs (lemmas) to include
  #' @param adverbs_only if TRUE, particles categorized as O, else M
  #' @return a DFM with phrases and frequency per document

  # Extract phrases using all cores for parallel processing.
  cli_alert_info("Extracting phrases...")
  cluster <- makeCluster(detectCores())
  clusterExport(
    cluster, c("corpus", "pattern", "nouns", "noun_tuples", "verbs",
               "adverbs_only"),
    envir = environment()
  )
  clusterExport(
    cluster, c("udpipe_extract_phrases", "udpipe_load_cached_model")
  )
  clusterEvalQ(cluster, library(udpipe))
  clusterEvalQ(cluster, library(dplyr))
  clusterEvalQ(cluster, model <- udpipe_load_cached_model())
  result <- pblapply(
    cl = cluster,
    X = names(corpus),
    FUN = function(name) {
      return(
        udpipe_extract_phrases(
          corpus[[name]], pattern, nouns = nouns, noun_tuples = noun_tuples,
          verbs = verbs, adverbs_only = adverbs_only, as_columns = TRUE,
          model = model
        )
      )
    }
  )
  stopCluster(cluster)

  # Create a DFM using a sparse matrix and hash table for memory and speed
  # reasons.
  # The sparse matrix is constructed using row (document) indexes, column
  # (phrase) indexes and values (phrase counts). The hash table stores the
  # phrase and index to the phrase names.
  # To minimize copying issues when growing indexes, lists instead of vectors
  # are used. To minimize long search times, a hash table instead of lists
  # or vectors are used for the phrase names.
  cli_alert_info("Creating a DFM")
  phrase_names <- hash()  # dimnames:columns
  document_indexes <- list()  # rows
  phrase_indexes <- list()  # columns
  phrase_counts <- list()  # values

  # Use a progress bar, although it might underestimate progress in the
  # beginning due to longer hash lookups later on.
  progress <- progress_bar$new(
    format = "  |:bar| :percent :elapsed",
    total = length(result),
    complete = "+",
    current = "-",
    incomplete = " ",
    clear = FALSE
  )

  # Add a triplet with document index, phrase index and phrase count for each
  # phrase in each document.
  for (document_index in seq(length.out = length(result))) {
    df <- result[[document_index]]

    for (phrase_index in seq(length.out = ncol(df))) {
      phrase_name <- make.names(names(df)[phrase_index])
      phrase_count <- df["result.freq", phrase_index]
      phrase_index <- phrase_names[[phrase_name]]
      if (is.null(phrase_index)) {
        phrase_index <- length(phrase_names) + 1
        phrase_names[[phrase_name]] <- phrase_index
      }

      document_indexes[[length(document_indexes) + 1]] <- document_index
      phrase_indexes[[length(phrase_indexes) + 1]] <- phrase_index
      phrase_counts[[length(phrase_counts) + 1]] <- phrase_count
    }
    progress$tick()
  }

  # Create the sparse matrix, convert to a DFM and add the docvars
  result <- Matrix::sparseMatrix(
    i = unlist(document_indexes),
    j = unlist(phrase_indexes),
    x = unlist(phrase_counts),
    dimnames = list(
      names(corpus),
      values(phrase_names) %>% sort() %>% names()
    )
  ) %>%
    as.dfm()
  docvars(result) <- docvars(corpus)

  return(result)
}
