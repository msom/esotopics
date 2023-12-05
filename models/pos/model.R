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
pos_corpus <- esocorpus %>%
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
save(pos_corpus, file = "models/pos/corpus.RData")

# Create DFM
pos_dfm_all <- preprocess_closed_words(pos_corpus)
vocabulary_save(pos_dfm_all, "models/pos/features_all.txt", TRUE)
pos_dfm <- pos_dfm_all %>%
  dfm_trim(min_docfreq = 2) %>%
  dfm_subset(ntoken(.) > 0, drop_docid = FALSE)
ncol(pos_dfm)  # words
nrow(pos_dfm)  # paragraphs
save(pos_dfm, file = "models/pos/dfm.RData")
vocabulary_save(pos_dfm, "models/pos/features.txt", TRUE)

# Read texts
pos_docs <- keyatm_read(texts = pos_dfm)

# Create keywords
pos_keywords <- list(
  the_astral = c(
    "astral",
    "realm",
    "plane",
    # "body",
    "projection"
  ),
  astral_light = c(
    "magnetic", "fluid",
    "ether",
    "astral", "light",
    "universal", "agent",
    "primordial",
    "terrestrial",
    "sidereal", "force",
    "electric", "vital",
    "fohat"
  ),
  kabbalistic_tarot = c(
    "kabbalistic",
    "book", "thoth",
    "tarot", "kabbalah"
  ),
  magnetic_sleep = c(
    "magnetic", "sleep",
    "crisis",
    "peaceful",
    "state",
    "somnambulism",
    "sixth", "sense",
    "clairvoyant", "healing"
  ),
  seance = c(
    "seance",
    "sitting",
    # "sitter",
    "manifestation",
    "rapping",
    "table", # "tipping",
    "movement", "furniture",
    "possessed", "medium",
    # "seized",
    "materialization",
    "good",
    "spirit",
    "evil",
    # "haunted",
    "spectre"
  ),
  progression = c(
    "progression",
    "progress",
    "incarnation",
    "reincarnation",
    "law",
    "karma",
    "monad",
    "cause", "effect",
    "retribution",
    "spiritual", "growth"
  )
)

visualize_keywords(
  docs = pos_docs,
  keywords = pos_keywords
)$figure + scale_color_discrete(
  labels = names(pos_keywords) %>%
    str_replace("\\d_", "") %>%
    str_replace_all("_", " ") %>%
    str_to_title()
)
ggsave("models/pos/keywords.pdf", width = 9, height = 6)

# Calculate models
keyatm_fit_models(
  docs = pos_docs,
  dfm = pos_dfm,
  keywords = pos_keywords,
  numbers = c(seq(1, 125), 150, 200, 250, 300),
  path = "models/pos/models/",
  seed = 123,
  parallel = 2
)
pos_metrics <- keyatm_measure_models(
  pos_dfm,
  numbers = c(seq(1, 125), 150, 200, 250, 300),
  pos_keywords,
  "models/pos/models/"
)
save(pos_metrics, file = "models/pos/metrics.RData")

# Find number of topics
keyatm_plot_topic_measure_scatter(
  pos_metrics,
  c(10, 25, 50, 75, 100, 125, 150, 200, 250, 300)
)
ggsave("models/pos/metrics_scatter_overview.pdf", width = 9, height = 4)

keyatm_plot_topic_measure_trend(
  pos_metrics,
  c(10, 25, 50, 75, 100, 125, 150, 200, 250, 300)
)
ggsave("models/pos/metrics_trend.pdf", width = 9, height = 5)

keyatm_plot_topic_measure_scatter(
  pos_metrics,
  c(seq(75, 125), 300),
  highlight = c(109)
)
ggsave("models/pos/metrics_scatter.pdf", width = 9, height = 5)

# Load model
pos_model <- keyatm_load_model(
  109,
  "models/pos/models/"
)

# Statistics

# ... document histogram
keyatm_plot_document_histogram(pos_model)
ggsave("models/pos/document_histogram.pdf", width = 9, height = 6)

# ... feature histogram
keyatm_plot_feature_histogram(pos_model)
ggsave("models/pos/feature_histogram.pdf", width = 9, height = 6)

# ... model statistics
pos_statistics <- keyatm_calculate_model_statistics(
  pos_model, pos_dfm, pos_keywords,
  intruder_features = c(NA, NA, NA, NA, NA, NA),
  intruder_documents = c(2 / 20, NA, 4 / 20, NA, NA, NA)
)
keyatm_print_model_statistics_table(
  pos_statistics,
  ncol(pos_dfm),
  nrow(pos_dfm)
)

# Validate

# ... convergence
plot_modelfit(pos_model)
ggsave("models/pos/model_fit.pdf", width = 9, height = 4)

plot_alpha(pos_model)

# ... topic proportion
plot_pi(pos_model)

plot_topicprop(pos_model, show_topic = seq(1, 6))

# ... top features
keyatm_print_top_words_table(pos_model)
top_words(pos_model, n = 200, show_keyword = FALSE) %>%
  write.csv("models/pos/topics.csv")

# ... top documents
pos_top_docs <- keyatm_top_docs_texts(pos_model, pos_corpus, pos_dfm, n = 20)
keyatm_save_top_docs_texts(pos_top_docs, "models/pos/docs.md")

# ... occurrences
keyatm_print_occurrences_table(pos_model, pos_dfm)

keyatm_plot_topic_occurrences(
  pos_model, pos_dfm, path = "models/pos/"
)
