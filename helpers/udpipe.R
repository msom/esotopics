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
    text, pattern, as_columns = FALSE, model = NULL, proper_nouns_only = TRUE,
    nouns = NULL, verbs = NULL
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
  #' @param nouns an optional list of nouns (lemmas) to include
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

  # extract phrases using lemmas
  result <- phrases(
    annotations$phrase_tag,
    term = annotations$lemma,
    pattern = pattern,
    is_regex = TRUE,
    detailed = FALSE
  ) %>%
    select(-ngram)

  if (!is.null(nouns)) {
    result <- result %>%
      rbind(
        phrases(
          annotations$phrase_tag,
          term = annotations$lemma,
          pattern = 'N',
          is_regex = TRUE,
          detailed = FALSE
        ) %>%
          select(-ngram) %>%
          filter(keyword %in% nouns)
      )
  }

  if (!is.null(verbs)) {
    result <- result %>%
      rbind(
        phrases(
          annotations$phrase_tag,
          term = annotations$lemma,
          pattern = 'V',
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
    result <- data.frame(result$freq) %>% t() %>% as.data.frame()
    colnames(result) <- column_names
  }
  return(result)
}

udpipe_phrases <- function(corpus, pattern, nouns = NULL, verbs = NULL) {
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
  #' @param nouns an optional list of nouns (lemmas) to include
  #' @param verbs an optional list of verbs (lemmas) to include
  #' @return a DFM with phrases and frequency per document
  #'
  cluster <- makeCluster(detectCores())
  clusterExport(cluster, c("corpus", "pattern", "nouns", "verbs"), envir = environment())
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
        udpipe_extract_phrases(
          corpus[[name]], pattern, nouns = nouns, verbs = verbs,
          as_columns = TRUE, model = model
        )
      )
    }
  )
  stopCluster(cluster)

  cli_alert_info("Creating a DFM")
  result <- rbind.fill(result)
  result[is.na(result)] <- 0
  rownames(result) <- names(corpus)
  names(result) <- make.names(names(result))
  return(as.dfm(result))
}

test_udpipe_extract_phrases <- function() {
  result <- udpipe_extract_phrases(
    "He believed in the immortality of the soul. All humans have immortal
    souls. Men's soul is immortal. He went into a magnetic sleep. Magnetic sleeps
    are sleeping with magents. After death, his spirit lived on. The form of these spirits.
    Of this, in this, magnetic he. Magnetic Sleep, magnetic Sleep, Magnetic Sleep.
    Allan said this and that. The was a true spiritist. Spiritist Doctrine.
    The law of progress is central. Law of progression, law of progress. Clairvoyance.
    The clairvoyants. The sleep-walker is a clairvoyant. A clairvoyante. Seance, séance.
    Occult science, occult sciences. Unveiling the veil. Should we unveil it or not.
    Saint-Germain was an ambassador.",
    # "AN|N(P+D*(A|N)*N)|NN",
    "AN|NPN",
    nouns = c("clairvoyance", "clairvoyant", "seance", "séance"),
    verbs = c("unveil")
  )
  stopifnot(nrow(result) == 13)
}


