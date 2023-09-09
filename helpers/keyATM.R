library(keyATM)
library(dplyr)
library(topicmodels)
library(topicdoc)

keyATM_top_docs_texts <- function(
    model, corpus,n = 10, include_others = FALSE
) {
  #'
  #' Show the texts of the top documents of a keyATM model for each topics
  #'
  #' @param model the keyATM model
  #' @param corpus the quanteda corpus used to train the model
  #' @param n the number of texts to show, default is 10
  #' @param include_others if FALSE, only pre-defined topics are used, default is FALSE
  #' @return a n x k table with the texts of the top n documents for each topic
  #'
  match = ifelse(include_others, ".*", "\\d_.*")
  docs <- top_docs(model, n) %>%
    select(matches(match))
  for (name in colnames(docs)) {
    docs[,name] <- corpus[docs[,name]]
  }
  return(docs)
}


keyATM_topic_coherence <- function(
    model, dfm, n = 10, include_others = FALSE
) {
  #'
  #' Calculate the topic coherences of a keyATM model
  #'
  #' Taken from Luigi Curinis Big Data Analytics course
  #'
  #' @param model the keyATM model
  #' @param dfm the document feature matrix
  #' @param n the number of top words to consider, default is 10
  #' @param include_others if FALSE, only pre-defined topics are used, default is FALSE
  #' @return a named vector with the coherence of each topic
  #'
  match = ifelse(include_others, ".*", "\\d_.*")
  words <- top_words(model, n = n, show_keyword = FALSE) %>%
    select(matches(match))
  dtm <- convert(dfm, to = "topicmodels")
  result <- vector(length = ncol(words))
  for (i in 1:ncol(words)) {
    result[i] <- topicdoc:::coherence(dtm, words[,i], 1)
  }
  result <- setNames(result, colnames(words))
  return(result)
}

keyATM_topic_exclusiveness <- function(
    model, n = 10, include_others = FALSE, weight = 0.7
) {
  #'
  #' Calculate the topic exclusiveness of a keyATM model
  #'
  #' Taken from Luigi Curinis Big Data Analytics course
  #'
  #' @param model the keyATM model
  #' @param n the number of top words to consider, default is 10
  #' @param include_others if FALSE, only pre-defined topics are used, default is FALSE
  #' @param weight weight to apply for FREX calculation, default is 0.7
  #' @return a named vector with the exclusiveness of each topic
  #'
  phi <- t(model$phi)
  mat <- phi / rowSums(phi)
  exclusivity <- apply(mat, 2, rank) / nrow(mat)
  frequency <- apply(phi, 2, rank) / nrow(mat)
  frex <- 1/(weight/exclusivity + (1 - weight)/frequency)
  index <- apply(phi, 2, order, decreasing = TRUE)[1:n, ]
  result <- vector(length = ncol(phi))
  for (i in 1:ncol(frex)) {
    result[i] <- sum(frex[index[, i], i])
  }
  result <- setNames(result, colnames(phi))
  match = ifelse(include_others, ".*", "\\d_.*")
  result <- subset(result, subset = grepl(match, names(result)))
  return(result)
}

keyATM_find_no_keyword_topics <- function(
    docs, dfm, keywords, numbers, n = 10, iterations = 1500, seed = NULL
) {
  #'
  #' Try a range
  #'
  #' Taken from Luigi Curinis Big Data Analytics course
  #'
  #' @param docs the keyATM docs
  #' @param dfm the document feature matrix
  #' @param keywords the keyATM keywords
  #' @param n the number of top words to consider, default is 10
  #' @param iterations the number of iterations, default is 1500
  #' @param seed the random seed, default is chosing a seed randomly
  #' @return a data frame with the number of topics, coherence and exclusivity
  #'
  result <- data.frame(
    topics=integer(),
    coherence=double(),
    exclusiveness=double()
  )
  for (index in 1:length(numbers)) {
    number <- numbers[index]
    result[index, "topics"]  <- number
    model <- keyATM(
      docs = docs,
      model = "base",
      no_keyword_topics = 5,
      keywords = keywords,
      options = list(
        seed = seed,
        iterations = iterations
      )
    )
    result[index, "coherence"] <- mean(keyATM_topic_coherence(model, dfm, n = n))
    result[index, "exclusiveness"] <- mean(keyATM_topic_exclusiveness(model, n = n))
  }
  result <- result %>%
    mutate(
      coherence_scaled=abs(coherence),
      exclusiveness_scaled=exclusiveness,
      across(c("coherence_scaled","exclusiveness_scaled"), ~ scale(.)),
      coherence_scaled = coherence_scaled - min(coherence_scaled),
      exclusiveness_scaled = exclusiveness_scaled - min(exclusiveness_scaled),
      value = sqrt(coherence_scaled^2 + exclusiveness_scaled^2)
    ) %>%
    arrange(-value)
  return(result)
}
