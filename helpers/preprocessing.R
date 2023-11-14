source("helpers/udpipe.R")

preprocess_phrases <- function(x) {
  #'
  #' Extract (proper) noun phrases and some highly specific nouns.
  #'
  #' Streamlines some spelling.
  #'
  #' @param x a corpus
  #' @return a DFM

  cleaned <- gsub("[kq]u?abb?all?ah?", "kabbalah", x, ignore.case = TRUE)
  cleaned <- gsub("[kq]u?abb?all?i([a-z]*)", "kabbali\\1", cleaned, ignore.case = TRUE)
  cleaned <- gsub("thoth?", "thoth", cleaned, ignore.case = TRUE)
  cleaned <- gsub("séance?", "seance", cleaned, ignore.case = TRUE)

  result <- cleaned %>%
    udpipe_phrases(
      pattern = "A+N|NPN|NN|N(P+D*(A|N)*N)",
      nouns = c(
        # the astral
        "astral",
        # astral light
        "ether", "fohat",
        # seance
        "seance", "sitting", "sitter", "manifestation", "rapping", "materialization",
        # progress(ion)
        "progression", "progress", "incarnation", "reincarnation", "karma", "monad"
      ),
      noun_tuples = list(
        # kabbalistic tarot
        c("tarot", "kabbalah")
      )
    ) %>%
    dfm_subset(ntoken(.) > 0, drop_docid = FALSE)

  return(result)
}

preprocess_closed_words <- function(x) {
  #'
  #' Extract closed words (nouns, adjectives, adverbs, verbs).
  #'
  #' Streamlines some spelling.
  #'
  #' @param x a corpus
  #' @return a DFM

  cleaned <- gsub("[kq]u?abb?all?ah?", "kabbalah", x, ignore.case = TRUE)
  cleaned <- gsub("[kq]u?abb?all?i([a-z]*)", "kabbali\\1", cleaned, ignore.case = TRUE)
  cleaned <- gsub("thoth?", "thoth", cleaned, ignore.case = TRUE)
  cleaned <- gsub("séance?", "seance", cleaned, ignore.case = TRUE)

  result <- cleaned %>%
    udpipe_phrases(
      pattern = "N|A|M|V",
      adverbs_only = TRUE
    ) %>%
    dfm_subset(ntoken(.) > 0, drop_docid = FALSE)

  return(result)
}

