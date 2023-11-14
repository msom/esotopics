source("helpers/udpipe.R")


test_udpipe_extract_phrases <- function() {
  text <- "All men had immortal souls. The immortal soul of great him."

  print("Test simple phrase")
  result <- udpipe_extract_phrases(text, "AN", proper_nouns_only = TRUE)
  print(result)
  stopifnot(nrow(result) == 1)

  print("Test additional nouns and verbs")
  result <- udpipe_extract_phrases(text, "AN", nouns = c("man"), noun_tuples = list(c("man", "soul")), verbs = c("have"))
  print(result)
  stopifnot(nrow(result) == 4)

  print("Test proper nouns")
  result <- udpipe_extract_phrases(text, "AN", proper_nouns_only = FALSE)
  print(result)

  print("Test adverbs/particles")
  result <- udpipe_extract_phrases("He didn't look up quickly", "M", adverbs_only = FALSE)
  print(result)
  stopifnot(nrow(result) == 2)
  result <- udpipe_extract_phrases("He didn't look up quickly", "M", adverbs_only = TRUE)
  print(result)
  stopifnot(nrow(result) == 1)

  print("Test special characters/French")
  result <- udpipe_extract_phrases("Seance, séance", "N")
  print(result)
  stopifnot(ncol(result) == 2)
  result <- udpipe_extract_phrases("Seance, séance", "A", nouns = c("seance", "séance"))
  print(result)
  stopifnot(ncol(result) == 2)

  print("Test as_columns")
  result <- udpipe_extract_phrases(text, "A", nouns = c("man"), as_columns = TRUE)
  print(result)
  stopifnot(ncol(result) == 3)
}

test_udpipe_phrases <- function() {
  corp <- corpus(
    "He believed in the immortality of the soul. All humans have immortal
    souls, indeed immortal souls. He was able to communicate in séances with
    immortal souls.
    He also was a great medium.",
    docvars = data.frame(author=c("Papus"))
  ) %>% corpus_reshape(to = "sentences")

  print("Test phrase extraction")
  result <- udpipe_phrases(
    corp,
    pattern = "AN",
    nouns = c("séance"),
    noun_tuples = list(
      c("immortality", "soul"),
      c("séance", "soul")
    ),
    verbs = c("communicate")
  )
  print(result)
  stopifnot(ncol(result)==6)
  stopifnot(nrow(result)==4)
  stopifnot(sum(result)==8)
}

test_udpipe_extract_phrases()
test_udpipe_phrases()
