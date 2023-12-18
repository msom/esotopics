library(keyATM)
library(quanteda)
library(tidyverse)

#
# Functions for the prediction
# See https://github.com/keyATM/keyATM/issues/187
#
rand_dir <- function(alpha) {
  x <- sapply(alpha, function(a) {rgamma(n = 1, shape = a, rate = 1)})
  x <- x / sum(x)
  return(x)
}

pred_logprob <- function(k, word_id, log_theta_iter, log_phi) {
  logprob <- log_theta_iter[k] + log_phi[k, word_id]
  return(logprob)
}

pred_topicprop <- function(fitted, docs_predict, iter_num = 300) {
  vocab <- fitted$vocab
  topics <- colnames(fitted$theta)

  # Check if it has new words
  if (sum(! unique(unlist(docs_predict$W_raw)) %in% vocab) > 0) {
    warning("New documents contain words not in the fitted object. They will be dropped.")
  }

  # Convert raw words to word ids
  wd_map <- keyATM:::myhashmap(vocab, 1:length(vocab) - 1L)
  W_pred <- lapply(docs_predict$W_raw, function(x) {
    ids <- keyATM:::myhashmap_getvec(wd_map, x)
    ids <- ids[!is.na(ids)]
    return(ids)
  })

  # alpha
  use_iter <- max(fitted$values_iter$used_iter) * 0.7
  fitted$values_iter$alpha_iter %>%
    filter(Iteration > use_iter) %>%
    group_by(Topic) %>%
    summarise(alpha = median(alpha)) %>%
    arrange(Topic) %>%
    pull(alpha) -> alpha_est

  # Other info
  D <- length(W_pred)
  K <- fitted$keyword_k + fitted$no_keyword_topics
  log_phi <- log(fitted$phi)
  doclen <- sapply(W_pred, length)
  pred_theta <- lapply(1:D, function(i) {matrix(-1, nrow = iter_num, ncol = K)})

  for (docid in seq_len(D)) {
    for (iter in 1:iter_num) {
      log_theta_iter <- log(rand_dir(alpha_est))

      z_d <- rep(NA, doclen[docid])
      for (docpos in seq_len(doclen[docid])) {
        word_id <- W_pred[[docid]][docpos] + 1  # adding 1 for R version
        log_prob <- sapply(1:K, function(k) {pred_logprob(k, word_id, log_theta_iter, log_phi)})
        prob <- exp(log_prob - matrixStats::logSumExp(log_prob))
        z_d[docpos] <- which.max(prob)
      }
      theta <- table(z_d) / doclen[docid]
      theta_use <- rep(0, K)
      theta_use[as.integer(names(theta))] <- theta
      pred_theta[[docid]][iter, ] <- theta_use
    }
    colnames(pred_theta[[docid]]) <- topics
  }
  return(pred_theta)
}
