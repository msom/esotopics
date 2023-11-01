source("helpers/udpipe.R")


test_udpipe_extract_phrases <- function() {
  text <- "All men had immortal souls. The immortal soul of great him."

  print("Test simple phrase")
  result <- udpipe_extract_phrases(text, "AN", proper_nouns_only = TRUE)
  print(result)
  stopifnot(nrow(result) == 1)

  print("Test additional nouns and verbs")
  result <- udpipe_extract_phrases(text, "AN", nouns = c("man"), verbs = c("have"))
  print(result)
  stopifnot(nrow(result) == 3)

  print("Test proper nouns")
  result <- udpipe_extract_phrases(text, "AN", proper_nouns_only = FALSE)
  print(result)
  stopifnot(nrow(result) == 2)

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

  print("Test phrase extraction with data frame")
  result <- udpipe_phrases(corp, "AN", nouns = c("séance"), verbs = c("communicate"), sparse = FALSE)
  print(result)
  stopifnot(ncol(result)==4)
  stopifnot(nrow(result)==4)
  stopifnot(sum(result)==6)

  print("Test phrase extraction with sparse matrix")
  result <- udpipe_phrases(corp, "AN", nouns = c("séance"), verbs = c("communicate"), sparse = TRUE)
  print(result)
  stopifnot(ncol(result)==4)
  stopifnot(nrow(result)==4)
  stopifnot(sum(result)==6)
}

test_udpipe_extract_phrases()
test_udpipe_phrases()
