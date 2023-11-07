library(esocorpus)

# load data
data(esocorpus)

# load helper functions
source("helpers/keyATM.R")
source("helpers/preprocessing.R")
source("helpers/vocabulary.R")

# Create corpus. Use text from esoteric animal magnetism, spiritualism and
# Kardecs main text. Only include paragraphs with at least 30 words. Reshape to
# paragraphs, since we assume to topic may change by paragraphs. Drop all
# features occurring only once.
spiritualism_corpus <- esocorpus %>%
  corpus() %>%
  corpus_subset(
    title %in% c(
      "An Introduction to the Study of Animal Magnetism",
      "The Celestial Telegraph",
      "The seeress of Prevorst",
      "The Spirits Book",
      "The Principles of Nature"
    )
  ) %>%
  corpus_trim("paragraphs", min_ntoken = 30) %>%
  corpus_reshape(to = "paragraphs")
spiritualism_dfm_all <- preprocess(spiritualism_corpus)
vocabulary_save(spiritualism_dfm_all, "models/spiritualism/features_all.txt", TRUE)
spiritualism_dfm <- spiritualism_dfm_all %>%
  dfm_trim(min_docfreq = 2) %>%
  dfm_subset(ntoken(.) > 0, drop_docid = FALSE)
ncol(spiritualism_dfm)  # phrases
nrow(spiritualism_dfm)  # paragraphs
save(spiritualism_dfm, file="models/spiritualism/dfm.RData")
vocabulary_save(spiritualism_dfm, "models/spiritualism/features.txt", TRUE)

# Read texts
spiritualism_docs <- keyATM_read(texts = spiritualism_dfm)

# Create keywords
spiritualism_keywords <- list(
  # the_astral pruned
  astral_light = c(
    # "great.magnetic.agent", pruned
    "magnetic.fluid",
    "ether"
    # "astral.light", pruned
    # "universal.agent", pruned
    # "primordial.light", pruned
    # "terrestrial.fluid", pruned
    # "sidereal.force", pruned
    # "electric.vital.fluid", pruned
    # "fohat" pruned
  ),
  # kabbalistic_tarot pruned
  magnetic_sleep = c(
    "magnetic.sleep",
    "magnetic.crisis",
    # "peaceful.sleep", pruned
    "magnetic.state",
    "state.of.somnambulism",
    "magnetic.somnambulism"
    # "sixth.sense", pruned
    # "clairvoyant.healing" pruned
  ),
  seance = c(
    "seance",
    "sitting",
    # "sitter", pruned
    "manifestation",
    # "rapping", pruned
    # "table.tipping", pruned
    # "movement.of.furniture", pruned
    # "possessed.medium", pruned
    # "seized.medium", pruned
    "materialization",
    "good.spirit",
    "evil.spirit"
    # "haunted.by.spectre" pruned
  ),
  progression = c(
    "progression",
    "progress",
    "incarnation",
    "reincarnation",
    "law.of.progress",
    # "karma", pruned
    "monad"
    # "law.of.cause.and.effect", pruned
    # "law.of.retribution", pruned
    # "spiritual.growth" pruned
  )
)
visualize_keywords(
  docs = spiritualism_docs,
  keywords = spiritualism_keywords
)

# Calculate models
spiritualism_topics_range <- seq(1, 120)  # TODO: 120 might be to few
keyATM_fit_models(
  spiritualism_docs,
  spiritualism_dfm,
  spiritualism_keywords,
  numbers = spiritualism_topics_range,
  path = "models/spiritualism/",
  seed = 123,
  parallel = 6
)

# Find number of topics (we are looking for the top left)
spiritualism_metrics <- keyATM_measure_models(
  spiritualism_dfm,
  spiritualism_topics_range,
  spiritualism_keywords,
  "models/spiritualism/"
)
save(spiritualism_metrics, file="models/spiritualism/metrics.RData")
spiritualism_metrics %>%
  ggplot(aes(x=coherence, y=exclusiveness)) +
  geom_point() +
  geom_text(aes(label=topics), vjust=1.5) +
  xlab("Coherence")  +
  ylab(label="Exclusiveness")
spiritualism_topics <- spiritualism_metrics[1:5,] %>%
  arrange(-ranksum) %>%
  first() %>%
  select(topics) %>%
  unlist()

# Load model
spiritualism_model <- keyATM_load_model(spiritualism_topics, "models/spiritualism/")

# Validate
plot_modelfit(spiritualism_model)
plot_alpha(spiritualism_model)
plot_topicprop(spiritualism_model)
spiritualism_words <- top_words(spiritualism_model, n = 200)
View(spiritualism_words)
top_words(spiritualism_model, n = 200, show_keyword = FALSE) %>%
  write.csv("models/spiritualism/topics.csv")
spiritualism_top_docs <- keyATM_top_docs_texts(spiritualism_model, spiritualism_corpus, spiritualism_dfm)
save(spiritualism_top_docs, file="models/spiritualism/docs.RData")
View(spiritualism_top_docs)

# Show topic in texts
keyATM_plot_topic_occurrence(spiritualism_model, spiritualism_dfm, "1_astral_light")
keyATM_plot_topic_occurrence(spiritualism_model, spiritualism_dfm, "2_magnetic_sleep")
keyATM_plot_topic_occurrence(spiritualism_model, spiritualism_dfm, "3_seance")
keyATM_plot_topic_occurrence(spiritualism_model, spiritualism_dfm, "4_progression")
keyATM_plot_topic_occurrences(spiritualism_model, spiritualism_dfm)

# Compare to one of Kardecs other texts
kardec_corpus <- esocorpus %>%
  corpus() %>%
  corpus_subset(title == "The Mediums Book") %>%
  corpus_trim("paragraphs", min_ntoken = 30) %>%
  corpus_reshape(to = "paragraphs")
kardec_dfm <- preprocess(kardec_corpus)
kardec_docs <- keyATM_read(texts = kardec_dfm)
visualize_keywords(kardec_docs, spiritualism_keywords)
vocabulary_compare(kardec_dfm, spiritualism_dfm_all)$known
keyATM_plot_keyword_occurrences(kardec_dfm, spiritualism_keywords, "progression")
