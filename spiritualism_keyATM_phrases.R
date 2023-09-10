library(dplyr)
library(ggplot2)
library(esocorpus)
library(quanteda)
library(keyATM)

# load data
data(esocorpus)

# load helper functions
source("helpers/keyATM.R")
source("helpers/udpipe.R")

# Create corpus. Exclude Davis, since his books is too general about knowledge
# of that time. Only include paragraphs with at least 30 words. Reshape to
# paragraphs, since we assume to topic may change by paragraphs.
spritualism_corpus <- esocorpus %>%
  corpus() %>%
  corpus_subset(name %in% c("Kerner", "Cahagnet")) %>%
  corpus_trim("paragraphs", min_ntoken = 30) %>%
  corpus_reshape(to = "paragraphs")

# Extract adjective-noun phrases
spritualism_dfm <- udpipe_phrases(spritualism_corpus, 'A{1}N{1}') %>%
  as.dfm() %>%
  dfm_subset(
    ntoken(.) > 0,
    drop_docid = FALSE
  )

# Read texts
spiritualism_docs <- keyATM_read(texts = spritualism_dfm)

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
  spritualism_dfm,
  spiritualism_keywords,
  seq(0, 10, 1),
  iterations=100
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
top_words(model)
keyATM_top_docs_texts(model, spritualism_corpus)

# plot_pi(model)
# keyATM_topic_coherence(model, spritualism_dfm)
# keyATM_topic_exclusiveness(model)


