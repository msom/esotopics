library(esocorpus)
library(simplermarkdown)
library(tidyverse)
library(quanteda)

# load data
data(esocorpus)

# create corpus
relation_corpus <- esocorpus %>%
  corpus() %>%
  corpus_subset(
    title %in% c(
      "An Introduction to the Study of Animal Magnetism",
      "The Celestial Telegraph",
      "The seeress of Prevorst",
      "The Spirits Book",
      "The Principles of Nature",
      "Transcendental magic, its doctrine and ritual",
      "The History of Magic",
      "The Tarot of the Bohemians",
      "The Secret Doctrine Vol 1",
      "The Key to Theosophy",
      "Astral Projection Ritual Magic and Alchemy"
    )
  ) %>%
  corpus_trim("paragraphs", min_ntoken = 30) %>%
  corpus_reshape(to = "paragraph")
docvars(relation_corpus)$file <- names(relation_corpus)
docvars(relation_corpus)$ntoken <- ntoken(relation_corpus)

# create a text file for each document
unlink("tmp", recursive = TRUE)
dir.create("tmp", showWarnings = FALSE)
relation_files <- list()
for (index in seq_along(relation_corpus)) {
  relation_files[index] <- paste0("tmp/", names(relation_corpus[index]))
  writeLines(relation_corpus[index], relation_files[[index]])
}

# extract relations using reverb from http://reverb.cs.washington.edu/
system2(
  "java",
  args = c("-Xmx512m", "-jar", "models/relation/reverb-latest.jar", "--files"),
  stdout = "models/relation/relations.csv",
  input = unlist(relation_files)
)
relation_data <- read_delim("models/relation/relations.csv", delim = "\t", col_names = FALSE) %>%
  rename(file = X1, sentence = X2, confidence = X12, left = X16, relation = X17, right = X18) %>%
  select(sentence, file, left, relation, right, confidence) %>%
  mutate(file = basename(file)) %>%
  left_join(docvars(relation_corpus))

# evaluate distribution of relations
relation_distribution <- relation_data %>%
  group_by(file) %>%
  summarize(f = n() / min(ntoken)) %>%
  pull()
fitdistrplus::descdist(relation_distribution)
plot(fitdistrplus::fitdist(relation_distribution, "norm"), breaks = 30)
pdf("models/relation/relation_distribution.pdf", width = 9, height = 6)
plot(fitdistrplus::fitdist(relation_distribution, "norm"), breaks = 30)
dev.off()

# extract correspondence relations
relation_pattern <- "correspond|analogous|resemble|echo(?! of)|reflect(?!ion| over)|equivalent|equal to"
relation_correspondences <- relation_data %>%
  filter(grepl(relation_pattern, relation, perl = TRUE))
nrow(relation_correspondences)

# evaluate distribution of correspondence relations
relation_correspondences_distribution <- relation_correspondences %>%
  group_by(file) %>%
  summarize(f = n() / max(ntoken)) %>%
  pull()
length(relation_correspondences_distribution)
fitdistrplus::descdist(relation_correspondences_distribution)
plot(fitdistrplus::fitdist(relation_correspondences_distribution, "gamma"), breaks = 30)
pdf("models/relation/correspondence_distribution.pdf", width = 9, height = 6)
plot(fitdistrplus::fitdist(relation_correspondences_distribution, "gamma"), breaks = 30)
dev.off()

# correspondence relations per document
relation_correspondences_per_document <- relation_correspondences %>%
  group_by(file) %>%
  summarize(relations = n()) %>%
  group_by(relations) %>%
  summarize(count = n()) %>%
  mutate(freq = round(count / sum(count), 3)) %>%
  md_table()
