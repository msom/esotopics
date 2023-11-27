library(esocorpus)

# load data
data(esocorpus)

# load helper functions
source("helpers/keyATM.R")
source("helpers/preprocessing.R")
source("helpers/vocabulary.R")

# Create corpus. Use text from Lévi, Papus and Blavatsky. Only include
# paragraphs with at least 30 words. Reshape to paragraphs, since we assume
# to topic may change by paragraphs.
occultism_corpus <- esocorpus %>%
  corpus() %>%
  corpus_subset(
    title %in% c(
      "Transcendental magic, its doctrine and ritual",
      "The History of Magic",
      "The Key of the Mysteries",
      "The Tarot of the Bohemians",
      "The Secret Doctrine Vol 1",
      "The Key to Theosophy"
    )
  ) %>%
  corpus_trim("paragraphs", min_ntoken = 30) %>%
  corpus_reshape(to = "paragraphs")
occultism_dfm_all <- preprocess_phrases(occultism_corpus)
vocabulary_save(occultism_dfm_all, "models/occultism/features_all.txt", TRUE)
occultism_dfm <- occultism_dfm_all %>%
  dfm_trim(min_docfreq = 2) %>%
  dfm_subset(ntoken(.) > 0, drop_docid = FALSE)
ncol(occultism_dfm)  # phrases
nrow(occultism_dfm)  # paragraphs
save(occultism_dfm, file="models/occultism/dfm.RData")
vocabulary_save(occultism_dfm, "models/occultism/features.txt", TRUE)

# Read texts
occultism_docs <- keyATM_read(texts = occultism_dfm)

# Create keywords
occultism_keywords <- list(
  the_astral = c(
    "astral",
    # "astral.realm", pruned
    "astral.plane",
    "astral.body"
    # "astral.projection" pruned
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
    # "magnetic.crisis", pruned
    # "peaceful.sleep", pruned
    # "magnetic.state", pruned
    "state.of.somnambulism",
    # "magnetic.somnambulism", pruned
    "sixth.sense"
    # "clairvoyant.healing" pruned
  ),
  seance = c(
    "seance",
    # "sitting", pruned
    # "sitter", pruned
    "manifestation",
    "rapping",
    # "table.tipping", pruned
    # "movement.of.furniture", pruned
    # "possessed.medium", pruned
    # "seized.medium", pruned
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
    # "law.of.progress",
    "karma",
    "monad",
    # "law.of.cause.and.effect",
    "law.of.retribution"
    # "spiritual.growth"
  )
)
visualize_keywords(
  docs = occultism_docs,
  keywords = occultism_keywords
)

# Calculate models
occultism_topics_range <- seq(1, 120)  # TODO: 120 might be to few
keyATM_fit_models(
  occultism_docs,
  occultism_dfm,
  occultism_keywords,
  numbers = occultism_topics_range,
  path = "models/occultism/models",
  seed = 123,
  parallel = 6
)

# Find number of topics (we are looking for the top left)
occultism_metrics <- keyATM_measure_models(
  occultism_dfm,
  occultism_topics_range,
  occultism_keywords,
  "models/occultism/models"
)
save(occultism_metrics, file="models/occultism/metrics.RData")
occultism_metrics %>%
  ggplot(aes(x=coherence, y=exclusivity)) +
  geom_point() +
  geom_text(aes(label=topics), vjust=1.5) +
  xlab("Coherence")  +
  ylab(label="Exclusivity")
occultism_topics <- occultism_metrics[1:5,] %>%
  arrange(-ranksum) %>%
  first() %>%
  select(topics) %>%
  unlist()

# Load model
occultism_model <- keyATM_load_model(occultism_topics, "models/occultism/models")

# Validate
plot_modelfit(occultism_model)
plot_alpha(occultism_model)
plot_topicprop(occultism_model)
occultism_words <- top_words(occultism_model, 200)
View(occultism_words)
top_words(occultism_model, n = 200, show_keyword = FALSE) %>%
  write.csv("models/occultism/topics.csv")
occultism_top_docs <- keyATM_top_docs_texts(occultism_model, occultism_corpus, occultism_dfm)
View(occultism_top_docs)
save(occultism_top_docs, file="models/occultism/docs.RData")

keyATM_plot_topic_occurrences(occultism_model, occultism_dfm)

# Test with one of levis other texts
levi_corpus <- esocorpus %>%
  corpus() %>%
  corpus_subset(title == "The Great Secret") %>%
  corpus_trim("paragraphs", min_ntoken = 30) %>%
  corpus_reshape(to = "paragraphs")
levi_dfm <- preprocess_phrases(levi_corpus)
levi_docs <- keyATM_read(texts = levi_dfm)
visualize_keywords(levi_docs, occultism_keywords)
vocabulary_compare(levi_dfm, occultism_dfm_all)$known
keyATM_plot_keyword_occurrences(levi_dfm, occultism_keywords, "occultism")
keyATM_plot_keyword_occurrences(levi_dfm, occultism_keywords, "astral_light")
keyATM_plot_keyword_occurrences(levi_dfm, occultism_keywords, "tarot")
