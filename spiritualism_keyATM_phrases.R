library(esocorpus)

# load data
data(esocorpus)

# load helper functions
source("helpers/keyATM.R")
source("helpers/preprocessing.R")
source("helpers/vocabulary.R")

# Create corpus. Use text from esoteric animal magnetism, spiritualism and
# Kardecs main text. Only include paragraphs with at least 30 words. Reshape to
# paragraphs, since we assume to topic may change by paragraphs.
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
spiritualism_dfm <- preprocess(spiritualism_corpus)
ncol(spiritualism_dfm)
save(spiritualism_dfm, file="out/spiritualism_dfm.RData")
vocabulary_save(spiritualism_dfm, "out/spiritualism_features.txt", TRUE)

# Read texts
spiritualism_docs <- keyATM_read(texts = spiritualism_dfm)

# Create keywords
spiritualism_keywords <- list(
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
    "incarnation",
    "progress",
    "progression"
  )
)

visualize_keywords(
  docs = spiritualism_docs,
  keywords = spiritualism_keywords
)

# Find number of topics (we are looking for the top left, but also consider rank)
spiritualism_metrics <- keyATM_find_no_keyword_topics(
  spiritualism_docs,
  spiritualism_dfm,
  spiritualism_keywords,
  seq(5, 50),  # TODO: 50 topics might be too less
  iterations=200,  # TODO: 200 iterations might be too less
  seed = 123,
  parallel = 6
)
save(spiritualism_metrics, file="out/spiritualism_metrics.RData")
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

# Create model
spiritualism_model <- keyATM(
  docs = spiritualism_docs,
  model = "base",
  no_keyword_topics = spiritualism_topics,
  keywords = spiritualism_keywords,
  options = list(
    seed = 123,
    iterations = 2000
  ),
)
save(spiritualism_model, file="out/spiritualism_model.RData")

# Validate
plot_modelfit(spiritualism_model)
plot_alpha(spiritualism_model)
plot_topicprop(spiritualism_model)
top_words(spiritualism_model, n = 30) %>%
  View()
top_words(spiritualism_model, n = 100, show_keyword = FALSE) %>%
  write.csv("out/spiritualism_topics.csv")
keyATM_top_docs_texts(spiritualism_model, spiritualism_corpus, spiritualism_dfm) %>%
  View()

# Show topic in texts
keyATM_plot_topic_occurrence(spiritualism_model, spiritualism_dfm, "1_magnetic_sleep")
keyATM_plot_topic_occurrence(spiritualism_model, spiritualism_dfm, "2_spiritualism")
keyATM_plot_topic_occurrence(spiritualism_model, spiritualism_dfm, "3_progress")
keyATM_plot_topic_occurrences(spiritualism_model, spiritualism_dfm)

# Compare to one of Kardecs other texts
kardec_corpus <- esocorpus %>%
  corpus() %>%
  corpus_subset(title == "The Mediums Book") %>%
  corpus_trim("paragraphs", min_ntoken = 30) %>%
  corpus_reshape(to = "paragraphs")
kardec_dfm <- preprocess(kardec_corpus, FALSE)
kardec_docs <- keyATM_read(texts = kardec_dfm)
visualize_keywords(kardec_docs, spiritualism_keywords)
kardec_metrics <- keyATM_find_no_keyword_topics(
  kardec_docs,
  kardec_dfm,
  spiritualism_keywords,
  seq(1, 50, 1),  # TODO: 50 topics might be too less
  iterations=100,  # TODO: 100 iterations are too less, models converge typically around 500
  seed = 123,
  parallel = 4
)
kardec_topics <- kardec_metrics[1:5,] %>%
  arrange(-ranksum) %>%
  first() %>%
  select(topics) %>%
  unlist()
kardec_metrics %>%
  ggplot(aes(x=coherence, y=exclusiveness)) +
  geom_point() +
  geom_text(aes(label=topics), vjust=1.5) +
  xlab("Coherence")  +
  ylab(label="Exclusiveness")
kardec_model <- keyATM(
  docs = kardec_docs,
  model = "base",
  no_keyword_topics = kardec_topics,
  keywords = spiritualism_keywords,
  options = list(
    seed = 123,
    iterations = 2000
  ),
)
vocabulary_compare(kardec_dfm, spiritualism_dfm)$known
keyATM_compare_models_by_words(spiritualism_model, kardec_model, m = 200) %>%
  pivot_longer(-n) %>%
  ggplot(aes(x = n, y = value, color = name)) +
  geom_line()
keyATM_compare_models_by_distribution(spiritualism_model, kardec_model)
keyATM_plot_keyword_occurrences(kardec_dfm, spiritualism_keywords, "spiritualism")
