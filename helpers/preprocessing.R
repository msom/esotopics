source("helpers/udpipe.R")

preprocess <- function(x) {
  #'
  #' Extract (proper) noun phrases and some highly specific nouns.
  #'
  #' Also streamlines some spelling.
  #'
  #' @param x a corpus
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

  return(result)
}
