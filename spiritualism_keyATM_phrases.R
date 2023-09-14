library(dplyr)
library(esocorpus)
library(ggplot2)
library(keyATM)
library(quanteda)
library(tidyr)

# load data
data(esocorpus)

# load helper functions
source("helpers/keyATM.R")
source("helpers/udpipe.R")

# Create corpus. Exclude Davis, since his books is too general about knowledge
# of that time. Only include paragraphs with at least 30 words. Reshape to
# paragraphs, since we assume to topic may change by paragraphs.
spiritualism_corpus <- esocorpus %>%
  corpus() %>%
  corpus_subset(name %in% c("Kerner", "Cahagnet")) %>%
  corpus_trim("paragraphs", min_ntoken = 30) %>%
  corpus_reshape(to = "paragraphs")

# Extract adjective-noun phrases
spiritualism_dfm <- udpipe_phrases(spiritualism_corpus, 'AN') %>%
  as.dfm() %>%
  dfm_subset(
    ntoken(.) > 0,
    drop_docid = FALSE
  )

# Read texts
spiritualism_docs <- keyATM_read(texts = spiritualism_dfm)

# Create keywords
spiritualism_keywords <- list(
  # nightly_encounter = c("night", "bed", "sleep", "spectre", "apparition"),
  # souls_of_the_deceased = c("material.body", "spiritual.world"),
  magnetic_sleep = c("magnetic.sleep", "magnetic.state", "somnambulic.state")
)

visualize_keywords(
  docs = spiritualism_docs,
  keywords = spiritualism_keywords
)

# Find number of topics (we are looking for the top left)
spiritualism_metrics <- keyATM_find_no_keyword_topics(
  spiritualism_docs,
  spiritualism_dfm,
  spiritualism_keywords,
  seq(1, 100, 1),
  iterations=100,
  seed = 123
)
spiritualism_topics <- spiritualism_metrics[1, "topics"]
spiritualism_metrics %>%
  ggplot(aes(x=coherence, y=exclusiveness)) +
  geom_point() +
  geom_text(aes(label=topics), vjust=1.5) +
  xlab("Coherence")  +
  ylab(label="Exclusiveness")

# Create model
model <- keyATM(
  docs = spiritualism_docs,
  model = "base",
  no_keyword_topics = spiritualism_topics,
  keywords = spiritualism_keywords,
  options = list(
    seed = 123,
    iterations = 1500
  ),
)

# Validate
plot_modelfit(model)
plot_alpha(model)
plot_topicprop(model)
View(top_words(model))
top_words(model, n = 50)[1]
keyATM_top_docs_texts(model, spiritualism_corpus, spiritualism_dfm)

# Show topic in texts
model$theta %>%
  as.data.frame() %>%
  mutate(name = rownames(spiritualism_dfm)) %>%
  separate_wider_delim(
    name,
    delim = ".txt.",
    names = c("book", "paragraph")
  ) %>%
  mutate(
    paragraph = as.numeric(paragraph),
    book = as.factor(book),
  ) %>%
  ggplot(
    aes(x = paragraph, y = `1_magnetic_sleep`)
  ) +
  geom_line() +
  geom_smooth(span = 0.1, se = FALSE) +
  facet_wrap(~ book, ncol = 1) +
  xlab("") +
  ylab("Magnetic Sleep")


model$theta %>%
  as.data.frame() %>%
  mutate(name = rownames(spiritualism_dfm)) %>%
  separate_wider_delim(
    name,
    delim = ".txt.",
    names = c("book", "paragraph")
  ) %>%
  gather(key = "topic", value = "value", c(-book, -paragraph)) %>%
  mutate(
    other = ifelse(startsWith(topic, "Other"), TRUE, FALSE),
    paragraph = as.numeric(paragraph),
    topic = as.factor(topic),
    book = as.factor(book),
  ) %>%
  ggplot(
    aes(x = paragraph, y = value, color = topic, linetype = other)
  ) +
  geom_smooth(span = 0.1, se = FALSE) +
  facet_wrap(~ book, ncol = 1) +
  xlab("") +
  ylab("Topic")


# plot_pi(model)
# keyATM_topic_coherence(model, spiritualism_dfm)
# keyATM_topic_exclusiveness(model)
