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
phrases_corpus <- esocorpus %>%
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
save(phrases_corpus, file = "models/phrases/corpus.RData")
phrases_dfm_all <- preprocess_phrases(phrases_corpus)
vocabulary_save(phrases_dfm_all, "models/phrases/features_all.txt", TRUE)
phrases_dfm <- phrases_dfm_all %>%
  dfm_trim(min_docfreq = 2) %>%
  dfm_subset(ntoken(.) > 0, drop_docid = FALSE)
ncol(phrases_dfm)  # phrases
nrow(phrases_dfm)  # paragraphs
save(phrases_dfm, file = "models/phrases/dfm.RData")
vocabulary_save(phrases_dfm, "models/phrases/features.txt", TRUE)

# Read texts
phrases_docs <- keyatm_read(texts = phrases_dfm)

# Create keywords
phrases_keywords <- list(
  the_astral = c(
    "astral",
    # "astral.realm", pruned
    "astral.plane",
    "astral.body",
    "astral.projection"
  ),
  astral_light = c(
    "magnetic.fluid",
    "ether",
    "astral.light",
    "universal.agent",
    "primordial.light",
    # "terrestrial.fluid", pruned
    "great.magnetic.agent"
    # "sidereal.force", pruned
    # "electric.vital.fluid", pruned
    # "fohat" pruned
  ),
  kabbalistic_tarot = c(
    # "kabbalistic.tarot", pruned
    "book.of.thoth",
    "tarot.kabbalah"
  ),
  magnetic_sleep = c(
    "magnetic.sleep",
    "magnetic.crisis",
    # "peaceful.sleep", pruned
    "magnetic.state",
    "state.of.somnambulism",
    "magnetic.somnambulism",
    "sixth.sense"
    # "clairvoyant.healing" pruned
  ),
  seance = c(
    "seance",
    "sitting",
    # "sitter", pruned
    "manifestation",
    "rapping",
    # "table.tipping", pruned
    # "movement.of.furniture", pruned
    # "possessed.medium",
    # "seized.medium",
    "materialization",
    "good.spirit",
    "evil.spirit"
    # "haunted.by.spectre"
  ),
  progression = c(
    "progression",
    "progress",
    "incarnation",
    "reincarnation",
    "law.of.progress",
    "karma",
    "monad",
    # "law.of.cause.and.effect", pruned
    "law.of.retribution",
    "spiritual.growth"
  )
)

visualize_keywords(
  docs = phrases_docs,
  keywords = phrases_keywords
)$figure + scale_color_discrete(
  labels = names(phrases_keywords) %>%
    str_replace("\\d_", "") %>%
    str_replace_all("_", " ") %>%
    str_to_title()
)
ggsave("models/phrases/keywords.pdf", width = 9, height = 6)

# Calculate models
keyatm_fit_models(
  docs = phrases_docs,
  dfm = phrases_dfm,
  keywords = phrases_keywords,
  numbers = c(seq(1, 124), 125, 150, 200, 250, 300),
  path = "models/phrases/models/",
  seed = 123,
  parallel = 5
)
phrases_metrics <- keyatm_measure_models(
  phrases_dfm,
  numbers = c(seq(1, 124), 125, 150, 200, 250, 300),
  phrases_keywords,
  "models/phrases/models/"
)
save(phrases_metrics, file = "models/phrases/metrics.RData")

# Find number of topics
keyatm_plot_topic_measure_scatter(
  phrases_metrics,
  c(10, 25, 50, 75, 100, 125, 150, 200, 250, 300)
)
ggsave("models/phrases/metrics_scatter_overview.pdf", width = 9, height = 4)

keyatm_plot_topic_measure_trend(
  phrases_metrics,
  c(10, 25, 50, 75, 100, 125, 150, 200, 250, 300)
)
ggsave("models/phrases/metrics_trend.pdf", width = 9, height = 5)

keyatm_plot_topic_measure_scatter(
  phrases_metrics,
  c(seq(75, 125), 300),
  highlight = c(106)
)
ggsave("models/phrases/metrics_scatter.pdf", width = 9, height = 5)

# Load model
phrases_model <- keyatm_load_model(
  106,
  "models/phrases/models/"
)

# Statistics

# ... document histogram
keyatm_plot_document_histogram(phrases_model, threshold = 1)
ggsave("models/phrases/document_histogram.pdf", width = 9, height = 6)

# ... feature histogram
keyatm_plot_feature_histogram(phrases_model)
ggsave("models/phrases/feature_histogram.pdf", width = 9, height = 6)

# ... model statistics
phrases_statistics <- keyatm_calculate_model_statistics(
  phrases_model, phrases_dfm, phrases_keywords,
  intruder_features = c(NA, NA, NA, NA, NA, NA),
  intruder_documents = c(13 / 20, NA, 7 / 20, NA, NA, NA)
)
keyatm_print_model_statistics_table(
  phrases_statistics,
  ncol(phrases_dfm),
  nrow(phrases_dfm)
)

# Validate

# ... convergence
plot_modelfit(phrases_model)
ggsave("models/phrases/model_fit.pdf", width = 9, height = 4)

plot_alpha(phrases_model)

# ... topic proportion
plot_pi(phrases_model)

plot_topicprop(phrases_model, show_topic = seq(1, 6))

# ... top features
keyatm_print_top_words_table(phrases_model)
top_words(phrases_model, n = 200, show_keyword = FALSE) %>%
  write.csv("models/phrases/topics.csv")

# ... top documents
phrases_top_docs <- keyatm_top_docs_texts(
  phrases_model, phrases_corpus, phrases_dfm, n = 20
)
keyatm_save_top_docs_texts(phrases_top_docs, "models/phrases/docs.md")

# ... occurrences
keyatm_print_occurrences_table(phrases_model, phrases_dfm)

keyatm_plot_topic_occurrences(
  phrases_model, phrases_dfm, path = "models/phrases/"
)
