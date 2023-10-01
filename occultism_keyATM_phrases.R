library(esocorpus)

# load data
data(esocorpus)

# load helper functions
source("helpers/keyATM.R")
source("helpers/udpipe.R")
source("helpers/preprocessing.R")
source("helpers/vocabulary.R")

# Create corpus. Use text from esoteric animal magnetism, occultism and
# Kardecs main text. Only include paragraphs with at least 30 words. Reshape to
# paragraphs, since we assume to topic may change by paragraphs.
occultism_corpus <- esocorpus %>%
  corpus() %>%
  corpus_subset(
    title %in% c(
      "Transcendental magic, its doctrine and ritual",
      "The History of Magic",
      "The Key of the Mysteries",
      "The Tarot of the Bohemians"
    )
  ) %>%
  corpus_trim("paragraphs", min_ntoken = 30) %>%
  corpus_reshape(to = "paragraphs")
occultism_dfm <- preprocess(occultism_corpus)
ncol(occultism_dfm)
save(occultism_dfm, file="out/occultism_dfm.RData")
vocabulary_save(occultism_dfm, "out/occultism_features.txt", TRUE)

# Read texts
occultism_docs <- keyATM_read(texts = occultism_dfm)

# Create keywords
occultism_keywords <- list(
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
  )
)

visualize_keywords(
  docs = occultism_docs,
  keywords = occultism_keywords
)

# Find number of topics (we are looking for the top left, but also consider rank)
occultism_metrics <- keyATM_find_no_keyword_topics(
  occultism_docs,
  occultism_dfm,
  occultism_keywords,
  seq(5, 50),  # TODO: 50 topics might be too less
  iterations=200,  # TODO: 200 iterations might be too less
  seed = 123,
  parallel = 4
)
save(occultism_metrics, file="out/occultism_metrics.RData")
occultism_metrics %>%
  ggplot(aes(x=coherence, y=exclusiveness)) +
  geom_point() +
  geom_text(aes(label=topics), vjust=1.5) +
  xlab("Coherence")  +
  ylab(label="Exclusiveness")

occultism_topics <- occultism_metrics[1:4,] %>%
  arrange(-ranksum) %>%
  first() %>%
  select(topics) %>%
  unlist()

# Create model
occultism_model <- keyATM(
  docs = occultism_docs,
  model = "base",
  no_keyword_topics = occultism_topics,
  keywords = occultism_keywords,
  options = list(
    seed = 123,
    iterations = 2000
  ),
)
save(occultism_model, file="out/occultism_model.RData")

# Validate
plot_modelfit(occultism_model)
plot_alpha(occultism_model)
plot_topicprop(occultism_model)
top_words(occultism_model, 30) %>%
  View()
top_words(occultism_model, n = 100, show_keyword = FALSE) %>%
  write.csv("out/occultism_topics.csv")
keyATM_top_docs_texts(occultism_model, occultism_corpus, occultism_dfm) %>%
  View()

# Show topic in texts
keyATM_plot_topic_occurrence(occultism_model, occultism_dfm, "1_occultism")
keyATM_plot_topic_occurrence(occultism_model, occultism_dfm, "2_tarot")
keyATM_plot_topic_occurrence(occultism_model, occultism_dfm, "3_astral_light")
keyATM_plot_topic_occurrences(occultism_model, occultism_dfm)

# Test with one of levis other texts
levi_corpus <- esocorpus %>%
  corpus() %>%
  corpus_subset(title == "The Great Secret") %>%
  corpus_trim("paragraphs", min_ntoken = 30) %>%
  corpus_reshape(to = "paragraphs")
levi_dfm <- preprocess(levi_corpus)
levi_docs <- keyATM_read(texts = levi_dfm)
visualize_keywords(levi_docs, occultism_keywords)
levi_metrics <- keyATM_find_no_keyword_topics(
  levi_docs,
  levi_dfm,
  occultism_keywords,
  seq(1, 50, 1),  # TODO: 50 topics might be too less
  iterations=100,  # TODO: 100 iterations are too less, models converge typically around 500
  seed = 123,
  parallel = 4
)
levi_topics <- levi_metrics[1:2,] %>%
  arrange(-ranksum) %>%
  first() %>%
  select(topics) %>%
  unlist()
levi_metrics %>%
  ggplot(aes(x=coherence, y=exclusiveness)) +
  geom_point() +
  geom_text(aes(label=topics), vjust=1.5) +
  xlab("Coherence")  +
  ylab(label="Exclusiveness")
levi_model <- keyATM(
  docs = levi_docs,
  model = "base",
  no_keyword_topics = levi_topics,
  keywords = occultism_keywords,
  options = list(
    seed = 123,
    iterations = 2000
  ),
)
vocabulary_compare(levi_dfm, occultism_dfm)$known
keyATM_compare_models_by_words(occultism_model, levi_model, m = 200) %>%
  pivot_longer(-n) %>%
  ggplot(aes(x = n, y = value, color = name)) +
  geom_line()
keyATM_compare_models_by_distribution(occultism_model, levi_model)
keyATM_plot_keyword_occurrences(levi_dfm, occultism_keywords, "occultism")
keyATM_plot_keyword_occurrences(levi_dfm, occultism_keywords, "astral_light")
keyATM_plot_keyword_occurrences(levi_dfm, occultism_keywords, "tarot")

