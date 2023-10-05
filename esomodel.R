library(esocorpus)

# load data
data(esocorpus)

# load helper functions
source("helpers/keyATM.R")
source("helpers/preprocessing.R")
source("helpers/vocabulary.R")

# Create corpus. Use text from esoteric animal magnetism, occultism and
# Kardecs main text. Only include paragraphs with at least 30 words. Reshape to
# paragraphs, since we assume to topic may change by paragraphs.
eso_corpus <- esocorpus %>%
  corpus() %>%
  corpus_subset(
    title %in% c(
      "An Introduction to the Study of Animal Magnetism",
      "The Celestial Telegraph",
      "The seeress of Prevorst",
      "The Spirits Book",
      "Transcendental magic, its doctrine and ritual",
      "The History of Magic",
      "The Key of the Mysteries",
      "The Tarot of the Bohemians"
    )
  ) %>%
  corpus_trim("paragraphs", min_ntoken = 30) %>%
  corpus_reshape(to = "paragraphs")
eso_dfm <- preprocess(eso_corpus)
ncol(eso_dfm)
save(eso_dfm, file="out/eso_dfm.RData")
vocabulary_save(eso_dfm, "out/eso_features.txt", TRUE)

# Read texts
eso_docs <- keyATM_read(texts = eso_dfm)

# Create keywords
eso_keywords <- list(
  occultism = c(
    "occultism",
    "occult.science",
    "occultist",
    "alchemy",
    "astrology",
    "kabbalah"
  ),
  tarot = c(
    "tarot",
    "book.of.thoth",
    "kabbalistic.tarot",
    "kabbalah"
  ),
  astral_light = c(
    "astral.light",
    "universal.agent",
    "primordial.light",
    "terrestrial.fluid",
    "magnetic.agent"
  ),
  magnetic_sleep = c(
    "magnetic.sleep",
    # "magnetic.crisis", not found often?
    # "peaceful.sleep",  not found?
    "magnetic.state",
    "state.of.somnambulism",
    "magnetic.somnambulism"
  ),
  spiritualism = c(
    "spiritualist",
    "spiritualism",
    "spiritism",
    "spiritist"
  ),
  progress = c(
    "law.of.progress",
    "law.of.progression",
    "reincarnation",
    "incarnation"
  )
)

visualize_keywords(
  docs = eso_docs,
  keywords = eso_keywords
)

# Find number of topics (we are looking for the top left, but also consider rank)
eso_metrics <- keyATM_find_no_keyword_topics(
  eso_docs,
  eso_dfm,
  eso_keywords,
  seq(5, 50),  # TODO: 50 topics might be too less
  iterations=200,  # TODO: 200 iterations might be too less
  seed = 123,
  parallel = 6
)
save(eso_metrics, file="out/eso_metrics.RData")
eso_metrics %>%
  ggplot(aes(x=coherence, y=exclusiveness)) +
  geom_point() +
  geom_text(aes(label=topics), vjust=1.5) +
  xlab("Coherence")  +
  ylab(label="Exclusiveness")

eso_topics <- eso_metrics[1:4,] %>%
  # arrange(-ranksum) %>%
  first() %>%
  select(topics) %>%
  unlist()

# Create model
eso_model <- keyATM(
  docs = eso_docs,
  model = "base",
  no_keyword_topics = eso_topics,
  keywords = eso_keywords,
  options = list(
    seed = 123,
    iterations = 2000
  ),
)
save(eso_model, file="out/eso_model.RData")

# Validate
plot_modelfit(eso_model)
plot_alpha(eso_model)
plot_topicprop(eso_model)
top_words(eso_model, 30) %>%
  View()
top_words(eso_model, n = 100, show_keyword = FALSE) %>%
  write.csv("out/eso_topics.csv")
keyATM_top_docs_texts(eso_model, eso_corpus, eso_dfm) %>%
  View()

# Show topic in texts
keyATM_plot_topic_occurrence(eso_model, eso_dfm, "1_occultism")
keyATM_plot_topic_occurrence(eso_model, eso_dfm, "2_tarot")
keyATM_plot_topic_occurrence(eso_model, eso_dfm, "3_astral_light")
keyATM_plot_topic_occurrence(eso_model, eso_dfm, "4_magnetic_sleep")
keyATM_plot_topic_occurrence(eso_model, eso_dfm, "5_spiritualism")
keyATM_plot_topic_occurrence(eso_model, eso_dfm, "6_progress")
keyATM_plot_topic_occurrences(eso_model, eso_dfm)
keyATM_plot_topic_correlation(eso_model, eso_dfm)

# covariate model
vars <- docvars(eso_corpus) %>%
  select(current)
eso_model_covariate <- keyATM(
  docs = eso_docs,
  model = "covariates",
  model_settings = list(
    covariates_data    = vars,
    covariates_formula = ~ current
  ),
  no_keyword_topics = eso_topics,
  keywords = eso_keywords,
  options = list(
    seed = 123,
    iterations = 2000
  )
)
covariates_info(eso_model_covariate)
strata_topic <- by_strata_DocTopic(
  eso_model_covariate,
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
