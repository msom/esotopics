source("helpers/udpipe.R")

preprocess <- function(x, drop_unique = TRUE) {
  #'
  #' Extract (proper) noun phrases and some highly specific nouns.
  #'
  #' Only keeps phrases occurring at least in two different documents.
  #'
  #' Also streamlines some spelling.
  #'
  #' @param x a corpus
  #' @param drop_unique if TRUE, drops phrases and nouns only occurring in a single document
  #' @return a DFM

  cleaned <- gsub("[kq]u?abb?all?ah?", "kabbalah", x, ignore.case = TRUE)
  cleaned <- gsub("[kq]u?abb?all?i([a-z]*)", "kabbali\\1", cleaned, ignore.case = TRUE)
  cleaned <- gsub("thoth?", "thoth", cleaned, ignore.case = TRUE)
  cleaned <- gsub("séance?", "seance", cleaned, ignore.case = TRUE)

  # todo: should we also extracts noun only but trim them to
  #       distinctive only nouns (= not frequent over all documents)?

  result <- cleaned %>%
    udpipe_phrases(
      pattern="AN|NPN|NN|N(P+D*(A|N)*N)", # alternatively "AN|NPN|NN"
      nouns=c(
        # spiritualism
        "spiritualism", "spiritualist",
        # spiritism
        "spiritism", "spiritist",
        # progress
        "reincarnation", "incarnation", "progress", "progression",
        # seance
        "seance", "sitting", "sitter", "rapping", "apparition", "tipping",
        "manifestation", "spectres", "materialization",
        # magnetic sleep
        "clairvoyance", "clairvoyant",
        # astral light
        "ether", "fohat",
        # occultism
        "occultism", "occultist", "tradition",
        "tarot", "kabbalah", "alchemy", "astrology",
        # master
        "master", "mahatma",
        # karma-nemesis
        "karma", "nirvana"
      )
    ) %>%
    dfm_subset(ntoken(.) > 0, drop_docid = FALSE)

  if (drop_unique) {
    result <- result %>%
      dfm_trim(min_docfreq = 2) %>%
      dfm_subset(ntoken(.) > 0, drop_docid = FALSE)
  }

  return(result)
}
