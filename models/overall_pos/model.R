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
ncol(overall_pos_dfm)  # phrases
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
    "good", "spirit",
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
)

# Calculate models
overall_pos_topics_range <- seq(1, 120)  # TODO: 120 might be to few
keyATM_fit_models(
  docs = overall_pos_docs,
  dfm = overall_pos_dfm,
  keywords = overall_pos_keywords,
  numbers = overall_pos_topics_range,
  path = "models/overall_pos/models/",
  seed = 123,
  parallel = 4
)

# Find number of topics (we are looking for the top left)
overall_pos_metrics <- keyATM_measure_models(
  overall_pos_dfm,
  overall_pos_topics_range,
  overall_pos_keywords,
  "models/overall_pos/models/"
)
save(overall_pos_metrics, file="models/overall_pos/metrics.RData")
overall_pos_metrics %>%
  ggplot(aes(x=coherence, y=exclusiveness, color=ranksum)) +
  geom_point() +
  geom_text(aes(label=topics), vjust=1.5) +
  scale_colour_gradient(
    high = "#132B43",
    low = "#56B1F7"
  ) +
  xlab("Coherence")  +
  ylab(label="Exclusiveness")
overall_pos_topics <- overall_pos_metrics[1:5,] %>%
  arrange(-ranksum) %>%
  first() %>%
  select(topics) %>%
  unlist()

# Load model
overall_pos_model <- keyATM_load_model(overall_pos_topics, "models/overall_pos/models/")

# Validate
plot_modelfit(overall_pos_model)
plot_alpha(overall_pos_model)
plot_topicprop(overall_pos_model)
overall_pos_words <- top_words(overall_pos_model, 200)
View(overall_pos_words)
top_words(overall_pos_model, n = 200, show_keyword = FALSE) %>%
  write.csv("models/overall_pos/topics.csv")
overall_pos_top_docs <- keyATM_top_docs_texts(overall_pos_model, overall_pos_corpus, overall_pos_dfm, n = 200)
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

# covariate model TODO: cleanup
vars <- docvars(overall_pos_corpus) %>%
  select(current)
overall_pos_model_covariate <- keyATM(
  docs = overall_pos_docs,
  model = "covariates",
  model_settings = list(
    covariates_data    = vars,
    covariates_formula = ~ current
  ),
  no_keyword_topics = overall_pos_topics,
  keywords = overall_pos_keywords,
  options = list(seed = 123)
)
covariates_info(overall_pos_model_covariate)
strata_topic <- by_strata_DocTopic(
  overall_pos_model_covariate,
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
