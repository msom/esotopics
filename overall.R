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
eso_corpus <- esocorpus %>%
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
eso_dfm_all <- preprocess(eso_corpus)
vocabulary_save(eso_dfm_all, "models/overall/features_all.txt", TRUE)
eso_dfm <- eso_dfm_all %>%
  dfm_trim(min_docfreq = 2) %>%
  dfm_subset(ntoken(.) > 0, drop_docid = FALSE)
ncol(eso_dfm)  # phrases
nrow(eso_dfm)  # paragraphs
save(eso_dfm, file="models/overall/dfm.RData")
vocabulary_save(eso_dfm, "models/overall/features.txt", TRUE)

# Read texts
eso_docs <- keyATM_read(texts = eso_dfm)

# Create keywords
eso_keywords <- list(
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
  docs = eso_docs,
  keywords = eso_keywords
)

# Calculate models
eso_topics_range <- seq(1, 120)  # TODO: 120 might be to few
keyATM_fit_models(
  docs = eso_docs,
  dfm = eso_dfm,
  keywords = eso_keywords,
  numbers = eso_topics_range,
  path = "models/overall/",
  seed = 123,
  parallel = 4
)

# Find number of topics (we are looking for the top left)
eso_metrics <- keyATM_measure_models(
  eso_dfm,
  eso_topics_range,
  eso_keywords,
  "models/overall/"
)
save(eso_metrics, file="models/overall/metrics.RData")
eso_metrics %>%
  ggplot(aes(x=coherence, y=exclusiveness, color=ranksum)) +
  geom_point() +
  geom_text(aes(label=topics), vjust=1.5) +
  scale_colour_gradient(
    high = "#132B43",
    low = "#56B1F7"
  ) +
  xlab("Coherence")  +
  ylab(label="Exclusiveness")
eso_topics <- eso_metrics[1:5,] %>%
  arrange(-ranksum) %>%
  first() %>%
  select(topics) %>%
  unlist()

# Load model
eso_model <- keyATM_load_model(eso_topics, "models/overall/")

# Validate
plot_modelfit(eso_model)
plot_alpha(eso_model)
plot_topicprop(eso_model)
eso_words <- top_words(eso_model, 200)
View(eso_words)
top_words(eso_model, n = 200, show_keyword = FALSE) %>%
  write.csv("models/overall/topics.csv")
eso_top_docs <- keyATM_top_docs_texts(eso_model, eso_corpus, eso_dfm, n = 200)
save(eso_top_docs, file="models/overall/docs.RData")
View(eso_top_docs)

# Show topic in texts
keyATM_plot_topic_occurrence(eso_model, eso_dfm, "1_the_astral")
keyATM_plot_topic_occurrence(eso_model, eso_dfm, "2_astral_light")
keyATM_plot_topic_occurrence(eso_model, eso_dfm, "3_kabbalistic_tarot")
keyATM_plot_topic_occurrence(eso_model, eso_dfm, "4_magnetic_sleep")
keyATM_plot_topic_occurrence(eso_model, eso_dfm, "5_seance")
keyATM_plot_topic_occurrence(eso_model, eso_dfm, "6_progression")
keyATM_plot_topic_occurrences(eso_model, eso_dfm)
keyATM_plot_topic_correlation(eso_model, eso_dfm)

# covariate model TODO: cleanup
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
  options = list(seed = 123)
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
