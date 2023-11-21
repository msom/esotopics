library(esocorpus)
library(ggrepel)

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
overall_phrases_corpus <- esocorpus %>%
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
overall_phrases_dfm_all <- preprocess_phrases(overall_phrases_corpus)
vocabulary_save(overall_phrases_dfm_all, "models/overall_phrases/features_all.txt", TRUE)
overall_phrases_dfm <- overall_phrases_dfm_all %>%
  dfm_trim(min_docfreq = 2) %>%
  dfm_subset(ntoken(.) > 0, drop_docid = FALSE)
ncol(overall_phrases_dfm)  # phrases
nrow(overall_phrases_dfm)  # paragraphs
save(overall_phrases_dfm, file="models/overall_phrases/dfm.RData")
vocabulary_save(overall_phrases_dfm, "models/overall_phrases/features.txt", TRUE)

# Read texts
overall_phrases_docs <- keyATM_read(texts = overall_phrases_dfm)

# Create keywords
overall_phrases_keywords <- list(
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
  docs = overall_phrases_docs,
  keywords = overall_phrases_keywords
)

# Calculate models in rough range
keyATM_fit_models(
  docs = overall_phrases_docs,
  dfm = overall_phrases_dfm,
  keywords = overall_phrases_keywords,
  numbers = c(seq(1, 124), 125, 150, 200, 250, 300),
  path = "models/overall_phrases/models/",
  seed = 123,
  parallel = 5
)
overall_phrases_metrics <- keyATM_measure_models(
  overall_phrases_dfm,
  numbers = c(seq(1, 124), 125, 150, 200, 250, 300),
  overall_phrases_keywords,
  "models/overall_phrases/models/",
  n = 15
)
save(overall_phrases_metrics, file="models/overall_phrases/metrics.RData")

# Find number of topics
keyATM_plot_topic_measure_scatter(
  overall_phrases_metrics,
  c(1, 10, 25, 50, 75, 100, 125, 150, 200, 250, 300)
)
ggsave("models/overall_phrases/metrics_scatter_overview.pdf")
keyATM_plot_topic_measure_trend(
  overall_phrases_metrics,
  c(1, 10, 25, 50, 75, 100, 125, 150, 200, 250, 300)
)
ggsave("models/overall_phrases/metrics_trend.pdf")
keyATM_plot_topic_measure_scatter(
  overall_phrases_metrics,
  c(seq(50, 125), 300)
)
ggsave("models/overall_phrases/metrics_scatter.pdf")
overall_phrases_topics <- 106

# Load model
overall_phrases_model <- keyATM_load_model(overall_phrases_topics, "models/overall_phrases/models/")

# Statistics
keyATM_plot_histogram(overall_phrases_model)
ggsave("models/overall_phrases/histogram.pdf")
overall_phrases_statistics = data.frame(
  word_count=keyATM_topic_word_count(overall_phrases_model),
  coherence=keyATM_topic_coherence(overall_phrases_model, overall_phrases_dfm, n = 15),
  exclusivity=keyATM_topic_exclusivity(overall_phrases_model, n = 15),
  ranksum=keyATM_topic_ranksum(overall_phrases_model, overall_phrases_keywords)
)
save(overall_phrases_statistics, file="models/overall_phrases/statistics.RData")
View(overall_phrases_statistics)

# Validate
plot_modelfit(overall_phrases_model)
plot_alpha(overall_phrases_model)
# plot_topicprop(overall_phrases_model)
overall_phrases_words <- top_words(overall_phrases_model, 200)
View(overall_phrases_words)
top_words(overall_phrases_model, n = 200, show_keyword = FALSE) %>%
  write.csv("models/overall_phrases/topics.csv")
overall_phrases_top_docs <- keyATM_top_docs_texts(overall_phrases_model, overall_phrases_corpus, overall_phrases_dfm, n = 200)
save(overall_phrases_top_docs, file="models/overall_phrases/docs.RData")
View(overall_phrases_top_docs)

# Show topic in texts
keyATM_plot_topic_occurrence(overall_phrases_model, overall_phrases_dfm, "1_the_astral")
keyATM_plot_topic_occurrence(overall_phrases_model, overall_phrases_dfm, "2_astral_light")
keyATM_plot_topic_occurrence(overall_phrases_model, overall_phrases_dfm, "3_kabbalistic_tarot")
keyATM_plot_topic_occurrence(overall_phrases_model, overall_phrases_dfm, "4_magnetic_sleep")
keyATM_plot_topic_occurrence(overall_phrases_model, overall_phrases_dfm, "5_seance")
keyATM_plot_topic_occurrence(overall_phrases_model, overall_phrases_dfm, "6_progression")
keyATM_plot_topic_occurrences(overall_phrases_model, overall_phrases_dfm)
keyATM_plot_topic_correlation(overall_phrases_model, overall_phrases_dfm)

# covariate model TODO: cleanup
vars <- docvars(overall_phrases_corpus) %>%
  select(current)
overall_phrases_model_covariate <- keyATM(
  docs = overall_phrases_docs,
  model = "covariates",
  model_settings = list(
    covariates_data    = vars,
    covariates_formula = ~ current
  ),
  no_keyword_topics = overall_phrases_topics,
  keywords = overall_phrases_keywords,
  options = list(seed = 123)
)
covariates_info(overall_phrases_model_covariate)
strata_topic <- by_strata_DocTopic(
  overall_phrases_model_covariate,
  by_var = "currentFrench Occultism",
  labels = c(0, 1)
  # labels = c("18_19c", "20_21c")
)
plot(
  strata_topic,
  var_name = "currentFrench Occultism",
  show_topic = 1:6,
  # by = "covariate"
)
