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

  # todo: should we also extracts noun only but trim them to
  #       distinctive only nouns (= not frequent over all documents)?

  result <- cleaned %>%
    udpipe_phrases(
      "AN|NPN",
      nouns=c(
        "spiritualism", "spiritualist",
        "spiritism", "spiritist",
        "reincarnation", "incarnation",
        "clairvoyance", "clairvoyant",
        "seance", "séance","sitting",
        "ether",
        "tarot",
        "kabbalah",
        "occultism", "occultist",
        "veil",
        "cagliostro"
      )
    ) %>%
    dfm_subset(ntoken(.) > 0, drop_docid = FALSE)

  return(result)
}
