library(dplyr)

vocabulary_extract <- function(x) {
  #'
  #' Extract the vocabulary of a dfm
  #'
  #' @param x the document feature matrix
  #' @param filename the filename
  #'
  if ("dfm" %in% class(x)) {
    return(colnames((x)))
  }
  return(x)
}

vocabulary_save <- function(x, filename, sort_alphabetically = FALSE) {
  #'
  #' Saves the vocabulary to a text file.
  #'
  #' @param x the document feature matrix
  #' @param filename the filename
  #'
  result <- vocabulary_extract(x)
  if (sort_alphabetically) {
    result <- sort(result)
  }
  write(result, filename)
}

vocabulary_compare <- function(x, base) {
  #'
  #' Compare one vocabulary to another.
  #'
  #' @param x the vocabulary to compare, a vector or a dfm
  #' @param base the vocabulary to compare to, a vector or a dfm
  #' @return a the proportion of known terms and all the unknown terms
  return(
    list(
      "known" = length(intersect(x, base)) / length(x),
      "unknown" = setdiff(vocabulary_extract(x), vocabulary_extract(base))
    )
  )
}
