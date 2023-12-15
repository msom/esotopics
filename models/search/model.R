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
    "astral",
    "astral.body",
    "astral.plane",
    "astral.projection",
    "astral.realm",
    # additional
    "astral.sphere",
    "akashic.envelope",
    "sphere.of.sensation",
    "image.on.the.sphere",
    "picture.on.the.sphere",
    "spirit.vision"
  ),
  astral_light = c(
    "astral.light",
    "electric.vital.fluid",
    "ether",
    "fohat",
    "great.magnetic.agent",
    "magnetic.fluid",
    "primordial.light",
    "sidereal.force",
    "terrestrial.fluid",
    "universal.agent"
    # additional
  ),
  kabbalistic_tarot = c(
    "kabbalistic.tarot"
  ),
  magnetic_sleep = c(
    "magnetic.sleep",
    "magnetic.crisis",
    "magnetic.somnambulism",
    # additional
    "magnetic.somnambule",
    "magnetic.somnambulist"
  ),
  seance = c(
    "seance",
    "sitting",
    "sitter",
    "rapping",
    "table.tipping",
    "movement.of.furniture",
    "possessed.medium",
    "seized.medium"
    # additional
  ),
  progression = c(
    # "progression",
    # "progress",
    # "incarnation",
    # "reincarnation",
    "law.of.progress",
    # "karma",
    # "monad",
    # "law.of.cause.and.effect",
    # "law.of.retribution",
    "spiritual.growth",
    # additional
    # "karmic.law",
    "national.karma",
    "cycle.of.incarnation",
    "chain.of.progression",
    "human.progress",
    "progress.of.humanity",
    "eternal.progression"
  )
)

# Create model
search_model <- keyatm_search_to_model(search_dfm, search_keywords)

# Statistics

# ... document histogram
keyatm_plot_document_histogram(search_model, threshold = 1)
ggsave("models/search/document_histogram.pdf", width = 9, height = 6)

# ... model statistics
search_statistics <- data.frame(
  feature_count = lengths(search_keywords),
  document_count = keyatm_topic_document_count(search_model),
  coherence = c(NA, NA, NA, NA, NA, NA),
  exclusivity = c(NA, NA, NA, NA, NA, NA),
  ranksum = c(NA, NA, NA, NA, NA, NA),
  intruder_features = c(NA, NA, NA, NA, NA, NA),
  intruder_documents = c(18.5 / 20, NA, 1 / 1, 20 / 20, 14 / 20, NA)
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
search_top_docs <- keyatm_top_docs_texts(
  search_model, search_corpus, search_dfm, n = 20
)
keyatm_save_top_docs_texts(search_top_docs, "models/search/docs.md")

keyatm_plot_top_docs_length(search_top_docs)
ggsave("models/search/doc_length.pdf", width = 9, height = 6)

# ... occurrences
keyatm_print_occurrences_table(search_model, search_dfm)

# TODO: clean up below

# ... The Astral <-> King


keyatm_plot_keyword_occurrences(
  search_dfm %>%
    dfm_subset(name == "King"),
  search_keywords,
  "the_astral"
)
