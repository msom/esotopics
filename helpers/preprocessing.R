source("helpers/udpipe.R")

preprocess <- function(x, drop_unique = TRUE) {
  #'
  #' Extract (proper) noun phrases and some highly specific nouns.
  #'
  #' Only keeps
  #'
  #' Also streamlines some spelling.
  #'
  #' @param x a corpus
  #' @param drop_unique if TRUE, drops phrases and nouns only occuring in a single document
  #' @return a DFM

  cleaned <- gsub("[kq]u?abb?all?ah?", "kabbalah", x, ignore.case = TRUE)
  cleaned <- gsub("[kq]u?abb?all?i([a-z]*)", "kabbali\\1", cleaned, ignore.case = TRUE)
  cleaned <- gsub("thoth?", "thoth", cleaned, ignore.case = TRUE)

  # todo: should we also extracts noun only but trim them to
  #       distinctive only nouns (= not frequent over all documents)?

  result <- cleaned %>%
    udpipe_phrases(
      "AN|NPN",
      nouns=c(
        # spiritualism, spiritism
        "spiritualism", "spiritualist",
        "spiritism", "spiritist",
        # progress
        "reincarnation", "incarnation",
        # magnetic sleep
        "clairvoyance", "clairvoyant",
        "seance", "séance","sitting",
        "ether",
        # occultism
        "occultism", "occultist", "tradition",
        "tarot", "kabbalah", "alchemy", "astrology"
      )
    ) %>%
    dfm_subset(ntoken(.) > 0, drop_docid = FALSE)

  if (drop_unique) {
    result <- result %>%
      dfm_trim(min_docfreq = 2)
  }

  result <- result %>%
    dfm_subset(ntoken(.) > 0, drop_docid = FALSE)

  return(result)
}
