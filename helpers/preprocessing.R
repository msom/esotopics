source("helpers/udpipe.R")

preprocess <- function(x) {
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

  # todo: should we also extracts noun only but trim them to
  #       distinctive only nouns (= not frequent over all documents)?

  result <- cleaned %>%
    udpipe_phrases(
      pattern="A+N|NPN|NN|N(P+D*(A|N)*N)", # alternatively "A+N|NPN|NN"
      nouns=c(
        # the astral
        "astral",
        # astral light
        "ether", "fohat",
        # seance
        "seance", "sitting", "sitter", "manifestation", "rapping", "materialization",
        # progress(ion)
        "progression", "progress", "incarnation", "reincarnation", "karma", "monad"
      ),
      noun_tuples=list(
        # kabbalistic tarot
        c("tarot", "kabbalah")
      )
    ) %>%
    dfm_subset(ntoken(.) > 0, drop_docid = FALSE)

  return(result)
}
