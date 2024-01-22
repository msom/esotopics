library(esocorpus)

# load data
data(esocorpus)

# load helper functions
source("helpers/keyATM.R")
source("helpers/preprocessing.R")
source("helpers/vocabulary.R")

# Create corpus. Use text from esoteric animal magnetism, spiritualism,
# spiritsm, occultism, Theosophic Society and Order of the Golden Dawn.
# Only include paragraphs with at least 30 words. Reshape to paragraphs, since
# we assume to topic may change by paragraphs. Drop all features occurring
# only once.
search_corpus <- esocorpus %>%
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
  corpus_reshape(to = "paragraphs")
save(search_corpus, file = "models/search/corpus.RData")

# Create DFM
search_dfm <- preprocess_phrases(search_corpus)
save(search_dfm, file = "models/search/dfm.RData")
vocabulary_save(search_dfm, "models/search/features.txt", TRUE)

# Define extended keyword dictionary
search_keywords <- list(
  the_astral = c(
    "astral", "astral.body", "astral.plane", "astral.projection", "astral.realm",
    # additional
    "astral.sphere", "akashic.envelope", "sphere.of.sensation", "image.on.the.sphere",
    "picture.on.the.sphere", "spirit.vision"
  ),
  astral_light = c(
    "astral.light", "electric.vital.fluid", "fohat", "great.magnetic.agent", "primordial.light",
    "sidereal.force", "terrestrial.fluid", "universal.agent",
    # additional
    "sun..force", "occult.force", "astral.fluid"
  ),
  kabbalistic_tarot = c("kabbalistic.tarot"),
  magnetic_sleep = c(
    "magnetic.sleep", "magnetic.crisis", "magnetic.somnambulism",
    # additional
    "magnetic.somnambule", "magnetic.somnambulist"
  ),
  seance = c(
    "seance", "sitting", "sitter", "rapping", "table.tipping", "movement.of.furniture",
    "possessed.medium", "seized.medium"
    # no additional
  ),
  progression = c(
    "law.of.progress", "spiritual.growth",
    # additional
    "national.karma", "cycle.of.incarnation", "chain.of.progression", "human.progress",
    "progress.of.humanity", "eternal.progression"
  )
)

# Create model
search_model <- keyatm_search_to_model(search_dfm, search_keywords)

# Statistics
search_statistics <- data.frame(
  feature_count = lengths(search_keywords),
  document_count = keyatm_topic_document_count(search_model),
  coherence = c(NA, NA, NA, NA, NA, NA),
  exclusivity = c(NA, NA, NA, NA, NA, NA),
  ranksum = c(NA, NA, NA, NA, NA, NA),
  intruder_features = c(NA, NA, NA, NA, NA, NA),
  intruder_documents = c(18.5 / 20, 19 / 20, 1 / 1, 20 / 20, 14 / 20, 16 / 20)
)
keyatm_print_model_statistics_table(
  search_statistics,
  ncol(search_dfm),
  nrow(search_dfm),
  test_coherence = FALSE,
  hide = c("Coherence", "Exclusivity", "IFS", "ISR")
)

# Validate
# ... top documents
search_top_docs <- keyatm_top_docs_texts(search_model, search_corpus, search_dfm, n = 47)
keyatm_save_top_docs_texts(search_top_docs, "models/search/docs.md")

keyatm_plot_top_docs_length(search_top_docs)
ggsave("models/search/doc_length.pdf", width = 9, height = 6)

# ... occurrences
keyatm_print_occurrences_table(search_model, search_dfm)

# ... external
search_classify <- function(titles) {
  corp <- esocorpus %>%
    corpus() %>%
    corpus_subset(title %in% titles) %>%
    corpus_trim("paragraphs", min_ntoken = 30) %>%
    corpus_reshape(to = "paragraphs")
  dfm <- corp %>%
    preprocess_phrases()
  result <- keyatm_keyword_search(dfm, search_keywords)
  known <- vocabulary_compare(dfm, search_dfm)$known
  return(list(corp = corp, dfm = dfm, known = known, result = result))
}

search_classification <- list(
  search_classify(c("The Complete Golden Dawn System Of Magic")),
  search_classify(c("On the Origin of Species By Means of Natural Selection")),
  search_classify(c("Sane Occultism")),
  search_classify(c("The Mediums Book"))
)
save(search_classification, file = "models/search/classification.RData")

file <- "models/search/classification.md"
cat(file = file)
for (i in seq_len(length(search_classification))) {
  corp <- search_classification[[i]]$corp
  cat(str_glue("\n\n# {docvars(corp)$title[1]}"), file = file, append = TRUE)
  for (topic in names(search_classification[[i]]$result)) {
    df <- as.data.frame(search_classification[[i]]$result[topic])
    cat(
      str_glue(
        "\n\n\n## {keyatm_topic_names(topic)}\n\n",
        "{ncol(df)} feature(s) found in {nrow(df)} documents\n\n\n"
      ),
      file = file, append = TRUE
    )
    if (ncol(df) > 0) {
      df_h <- df %>%
        mutate(docid = row.names(.), sum = rowSums(.)) %>%
        separate_wider_delim(
          docid,
          delim = ".txt.",
          names = c("book", "paragraph")
        ) %>%
        relocate(c(book, paragraph, sum)) %>%
        arrange(-sum) %>%
        head(1)
      cat(md_table(df_h, as_character = TRUE), file = file, append = TRUE)
      text <- corp[[as.integer(df_h[1, "paragraph"])]]
      cat(str_glue("\n\n\n\n> {text}"), file = file, append = TRUE)
    }
  }
}

# Evaluate keywords
original_keywords <- list(
  the_astral = c(
    "astral", "astral.realm", "astral.plane", "astral.body", "astral.projection"
  ),
  astral_light = c(
    "magnetic.fluid", "ether", "astral.light", "universal.agent",
    "primordial.light", "terrestrial.fluid", "great.magnetic.agent",
    "sidereal.force", "electric.vital.fluid", "fohat"
  ),
  kabbalistic_tarot = c(
    "kabbalistic.tarot", "book.of.thoth", "tarot.kabbalah"
  ),
  magnetic_sleep = c(
    "magnetic.sleep", "magnetic.crisis", "peaceful.sleep", "magnetic.state",
    "state.of.somnambulism", "magnetic.somnambulism", "sixth.sense",
    "clairvoyant.healing"
  ),
  seance = c(
    "seance", "sitting", "sitter", "manifestation", "rapping", "table.tipping",
    "movement.of.furniture", "possessed.medium", "seized.medium",
    "materialization", "good.spirit", "evil.spirit", "haunted.by.spectre"
  ),
  progression = c(
    "progression", "progress", "incarnation", "reincarnation",
    "law.of.progress", "karma", "monad", "law.of.cause.and.effect",
    "law.of.retribution", "spiritual.growth"
  )
)
search_occurrences <- keyatm_keyword_search(search_dfm, search_keywords)
for (topic in names(search_occurrences)) {
  print(str_glue("\n\n{topic}"))
  common <- intersect(search_keywords[[topic]], original_keywords[[topic]])
  removed <- setdiff(original_keywords[[topic]], search_keywords[[topic]])
  added <- setdiff(search_keywords[[topic]], original_keywords[[topic]])
  not_found <- setdiff(common, names(search_occurrences[[topic]]))
  print(str_glue("  Removed: {paste(removed, collapse = ', ')}"))
  print(str_glue("  Added: {paste(added, collapse = ', ')}"))
  print(str_glue("  Common: {paste(common, collapse = ', ')}"))
  print(str_glue("  Not found: {paste(not_found, collapse = ', ')}"))
}
