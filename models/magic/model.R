library(esocorpus)

# load data
data(esocorpus)

# load helper functions
source("helpers/keyATM.R")
source("helpers/preprocessing.R")
source("helpers/vocabulary.R")

# Create corpus. Use text from Golden Dawn. Only include paragraphs with at
# least 30 words. Reshape to paragraphs, since we assume to topic may change by
# paragraphs. Drop all features occurring only once.
magic_corpus <- esocorpus %>%
  corpus() %>%
  corpus_subset(
    title %in% c(
      "Astral Projection Ritual Magic and Alchemy"
    )
  ) %>%
  corpus_trim("paragraphs", min_ntoken = 30) %>%
  corpus_reshape(to = "paragraphs")
magic_dfm_all <- preprocess(magic_corpus)
vocabulary_save(magic_dfm_all, "models/magic/features_all.txt", TRUE)
magic_dfm <- magic_dfm_all %>%
    dfm_trim(min_docfreq = 2) %>%
    dfm_subset(ntoken(.) > 0, drop_docid = FALSE)
ncol(magic_dfm)  # phrases
nrow(magic_dfm)  # paragraphs
save(magic_dfm, file="models/magic/dfm.RData")
vocabulary_save(magic_dfm, "models/magic/features.txt", TRUE)

# Read texts
magic_docs <- keyATM_read(texts = magic_dfm)

# Create keywords
magic_keywords <- list(
  the_astral = c(
    "astral",
    # "astral.realm",
    "astral.plane",
    # "astral.body",
    "astral.projection"
  ),
  astral_light = c(
    # "magnetic.fluid",
    "ether",
    "astral.light"
    # "universal.agent",
    # "primordial.light",
    # "terrestrial.fluid",
    # "great.magnetic.agent",
    # "sidereal.force",
    # "electric.vital.fluid",
    # "fohat"
  ),
  # kabbalistic_tarot pruned
  # magnetic_sleep pruned
  seance = c(
    # "seance",
    # "sitting",
    # "sitter",
    "manifestation",
    # "rapping",
    # "table.tipping",
    # "movement.of.furniture",
    # "possessed.medium",
    # "seized.medium",
    # "materialization",
    # "good.spirit",
    "evil.spirit"
    # "haunted.by.spectre"
  ),
  progression = c(
    "progression",
    "progress",
    "incarnation",
    # "reincarnation",
    # "law.of.progress",
    # "karma",
    # "monad",
    # "law.of.cause.and.effect",
    # "law.of.retribution",
    "spiritual.growth"
  )
)
visualize_keywords(
  docs = magic_docs,
  keywords = magic_keywords
)

# Calculate models
magic_topics_range <- seq(1, 120)
keyATM_fit_models(
  magic_docs,
  magic_dfm,
  magic_keywords,
  numbers = magic_topics_range,
  path = "models/magic/models",
  seed = 123,
  parallel = 6
)

# Find number of topics (we are looking for the top left)
magic_metrics <- keyATM_measure_models(
  magic_dfm,
  magic_topics_range,
  magic_keywords,
  "models/magic/models"
)
save(magic_metrics, file="models/magic/metrics.RData")
magic_metrics %>%
  ggplot(aes(x=coherence, y=exclusiveness)) +
  geom_point() +
  geom_text(aes(label=topics), vjust=1.5) +
  xlab("Coherence")  +
  ylab(label="Exclusiveness")
magic_topics <- magic_metrics[1:5,] %>%
  arrange(-ranksum) %>%
  first() %>%
  select(topics) %>%
  unlist()

# Load model
magic_model <- keyATM_load_model(magic_topics, "models/magic/models")

# Validate
plot_modelfit(magic_model)
plot_alpha(magic_model)
plot_topicprop(magic_model)
magic_words <- top_words(magic_model, n = 200)
View(magic_words)
top_words(magic_model, n = 200, show_keyword = FALSE) %>%
  write.csv("models/magic/topics.csv")
magic_top_docs <- keyATM_top_docs_texts(magic_model, magic_corpus, magic_dfm)
save(magic_top_docs, file="models/magic/docs.RData")
View(magic_top_docs)

# Show topic in texts
keyATM_plot_topic_occurrence(magic_model, magic_dfm, "1_the_astral")
keyATM_plot_topic_occurrences(magic_model, magic_dfm)
