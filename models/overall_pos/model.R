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
overall_pos_corpus <- esocorpus %>%
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

overall_pos_dfm_all <- preprocess_closed_words(overall_pos_corpus)
vocabulary_save(overall_pos_dfm_all, "models/overall_pos/features_all.txt", TRUE)
overall_pos_dfm <- overall_pos_dfm_all %>%
  dfm_trim(min_docfreq = 2) %>%
  dfm_subset(ntoken(.) > 0, drop_docid = FALSE)
ncol(overall_pos_dfm)  # words
nrow(overall_pos_dfm)  # paragraphs
save(overall_pos_dfm, file="models/overall_pos/dfm.RData")
vocabulary_save(overall_pos_dfm, "models/overall_pos/features.txt", TRUE)

# Read texts
overall_pos_docs <- keyATM_read(texts = overall_pos_dfm)

# Create keywords
overall_pos_keywords <- list(
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
  docs = overall_pos_docs,
  keywords = overall_pos_keywords
)$figure + scale_color_discrete(
  labels = names(overall_pos_keywords) %>%
    str_replace("\\d_", "") %>%
    str_replace_all("_", " ") %>%
    str_to_title()
)
ggsave("models/overall_pos/keywords.pdf")

# Calculate models
# todo: calculate models 115-124
keyATM_fit_models(
  docs = overall_pos_docs,
  dfm = overall_pos_dfm,
  keywords = overall_pos_keywords,
  # numbers = c(111, 112, 113, 114),
  numbers = c(seq(1, 114), 125, 150, 200, 250, 300),
  path = "models/overall_pos/models/",
  seed = 123,
  parallel = 2
)
overall_pos_metrics <- keyATM_measure_models(
  overall_pos_dfm,
  numbers = c(seq(1, 114), 125, 150, 200, 250, 300),
  overall_pos_keywords,
  "models/overall_pos/models/"
)
save(overall_pos_metrics, file="models/overall_pos/metrics.RData")

# Find number of topics
keyATM_plot_topic_measure_scatter(
  overall_pos_metrics,
  c(1, 10, 25, 50, 75, 100, 125, 150, 200, 250, 300)
)
ggsave("models/overall_pos/metrics_scatter_overview.pdf")

keyATM_plot_topic_measure_trend(
  overall_pos_metrics,
  c(1, 10, 25, 50, 75, 100, 125, 150, 200, 250, 300)
)
ggsave("models/overall_pos/metrics_trend.pdf")

keyATM_plot_topic_measure_scatter(
  overall_pos_metrics,
  c(seq(75, 114), 300),
  # c(seq(50, 125), 300),
  highlight = c(109)
)
ggsave("models/overall_pos/metrics_scatter.pdf")

# Load model
overall_pos_model <- keyATM_load_model(
  109,
  "models/overall_pos/models/"
)

# Statistics
keyATM_plot_document_histogram(overall_pos_model)
ggsave("models/overall_pos/document_histogram.pdf")

keyATM_plot_feature_histogram(overall_pos_model)
ggsave("models/overall_pos/feature_histogram.pdf")

overall_pos_statistics = data.frame(
  feature_count=keyATM_topic_feature_count(overall_pos_model),
  coherence=keyATM_topic_coherence(overall_pos_model, overall_pos_dfm, n = 15),
  exclusivity=keyATM_topic_exclusivity(overall_pos_model, n = 15),
  ranksum=keyATM_topic_ranksum(overall_pos_model, overall_pos_keywords),
  probability=plot_pi(overall_pos_model)$values$Probability,
  proportion=plot_topicprop(overall_pos_model, show_topic = seq(1, 6), order = "topicid")$values$Topicprop
)
save(overall_pos_statistics, file="models/overall_pos/statistics.RData")
View(overall_pos_statistics)

# Validate
plot_modelfit(overall_pos_model)
ggsave("models/overall_pos/model_fit.pdf")

plot_alpha(overall_pos_model)

plot_pi(overall_pos_model)

plot_topicprop(overall_pos_model, show_topic = seq(1, 6))

overall_pos_words <- top_words(overall_pos_model, 200)
top_words(overall_pos_model, n = 200, show_keyword = FALSE) %>%
  write.csv("models/overall_pos/topics.csv")
View(overall_pos_words)

overall_pos_top_docs <- keyATM_top_docs_texts(
  overall_pos_model, overall_pos_corpus, overall_pos_dfm, n = 200
)
save(overall_pos_top_docs, file="models/overall_pos/docs.RData")
View(overall_pos_top_docs)

# Show topic in texts
keyATM_plot_topic_occurrence(overall_pos_model, overall_pos_dfm, "1_the_astral")
keyATM_plot_topic_occurrence(overall_pos_model, overall_pos_dfm, "2_astral_light")
keyATM_plot_topic_occurrence(overall_pos_model, overall_pos_dfm, "3_kabbalistic_tarot")
keyATM_plot_topic_occurrence(overall_pos_model, overall_pos_dfm, "4_magnetic_sleep")
keyATM_plot_topic_occurrence(overall_pos_model, overall_pos_dfm, "5_seance")
keyATM_plot_topic_occurrence(overall_pos_model, overall_pos_dfm, "6_progression")
keyATM_plot_topic_occurrences(overall_pos_model, overall_pos_dfm)
keyATM_plot_topic_correlation(overall_pos_model, overall_pos_dfm)
