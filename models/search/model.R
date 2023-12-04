source("helpers/keyATM.R")

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
    "book.of.thoth",
    "kabbalistic.tarot",
    "tarot.kabbalah"
    # additional
  ),
  magnetic_sleep = c(
    "magnetic.sleep",
    "magnetic.crisis",
    "magnetic.state",
    "state.of.somnambulism",
    "magnetic.somnambulism",
    "sixth.sense",
    "clairvoyant.healing"
    # additional
  ),
  seance = c(
    "seance",
    "sitting",
    "sitter",
    "manifestation",
    "rapping",
    "table.tipping",
    "movement.of.furniture",
    "possessed.medium",
    "seized.medium",
    "materialization",
    "good.spirit",
    "evil.spirit",
    "haunted.by.spectre"
    # additional
  ),
  progression = c(
    "progression",
    "progress",
    "incarnation",
    "reincarnation",
    "law.of.progress",
    "karma",
    "monad",
    "law.of.cause.and.effect",
    "law.of.retribution",
    "spiritual.growth"
    # additional
  )
)

# Load reference model and DFM
phrases_model <- keyatm_load_model(
  106,
  "models/phrases/models/"
)
load("models/phrases/dfm.RData")
load("models/phrases/corpus.RData")

# Create model
search_model <- keyatm_search_to_model(
  phrases_model, phrases_dfm, search_keywords
)

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
  intruder_documents = c(18.5 / 20, NA, NA, NA, NA, NA)
)
keyatm_print_model_statistics_table(
  search_statistics,
  ncol(phrases_dfm),
  nrow(phrases_dfm),
  test_coherence = FALSE,
  hide = c("Coherence", "Exclusivity", "IFS", "ISR")
)

# Validate

# ... top documents
search_top_docs <- keyatm_top_docs_texts(
  search_model, phrases_corpus, phrases_dfm, n = 20
)
keyatm_save_top_docs_texts(search_top_docs, "models/search/docs.md")

# ... occurrences
keyatm_print_occurrences_table(search_model_categorical, phrases_dfm)

# TODO: clean up below

# ... The Astral <-> King
keyatm_compare_search_to_model(
  phrases_model, phrases_dfm, search_keywords, "1_the_astral", "King"
)
ggsave("models/search/confusion_the_astral_king.pdf", width = 5, height = 5)


keyatm_plot_keyword_occurrences(
  phrases_dfm %>%
    dfm_subset(name == "King"),
  search_keywords,
  "the_astral"
)
