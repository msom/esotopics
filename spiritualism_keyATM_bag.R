library(dplyr)
library(esocorpus)
library(ggplot2)
library(keyATM)
library(quanteda)

# load data
data(esocorpus)

# load helper functions
source("helpers/keyatm.R")

# Create corpus. Exclude Davis, since his books is too general about knowledge
# of that time. Only include paragraphs with at least 30 words. Reshape to
# paragraphs, since we assume to topic may change by paragraphs.
spiritualism_corpus <- esocorpus %>%
  corpus() %>%
  corpus_subset(name %in% c("Kerner", "Cahagnet")) %>%
  corpus_trim("paragraphs", min_ntoken = 30) %>%
  corpus_reshape(to = "paragraphs")

# Create tokens. Remove numbers, punctuation, symbols, separators, stopwords
# and short words.
spritualism_tokens <- spiritualism_corpus %>%
  tokens(
    remove_numbers = TRUE,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_separators = TRUE,
  ) %>%
  tokens_remove(stopwords('en')) %>%
  tokens_select(min_nchar = 3)

# Create dfm.
spritualism_dfm <- spritualism_tokens %>%
  dfm() %>%
  # dfm_trim(min_termfreq = 1) %>%
  dfm_subset(
    ntoken(.) > 0,
    drop_docid = FALSE
  )

# Read texts
spiritualism_docs <- keyATM_read(texts = spritualism_dfm)

# Create keywords
spiritualism_keywords <- list(
  nightly_encounter = c("night", "bed", "sleep", "spectre", "apparition"),
  souls_of_the_deceased = c("dead", "soul", "spirit", "body"),
  magnetic_sleep = c("magnetic", "somnambulic", "somnambule")
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
  iterations=500
)
spiritualism_topics <- spiritualism_metrics[1, "topics"]
spiritualism_metrics %>%
  ggplot(aes(x=coherence, y=exclusiveness)) +
  geom_point() +
  geom_text(aes(label=topics), vjust=1.5) +
  xlab("Coherence")  +
  ylab(label="Exclusiveness")
metrics

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
plot_topicprop(model)
keyATM_topic_coherence(model, spritualism_dfm)
keyATM_topic_exclusiveness(model)

top_words(model, n = 30)
keyATM_top_docs_texts(model, spiritualism_corpus)


# Old
# model$theta  # Document-topic distribution
# model$phi    # Topic-word distribution
plot_modelfit(model)
plot_alpha(model)
plot_pi(model)

