library(dplyr)

vocabulary_extract <- function(x) {
  #'
  #' Extract the vocabulary of a dfm
  #'
  #' @param x the document feature matrix
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

vocabulary_compare <- function(x, y) {
  #'
  #' Compare one vocabulary to another.
  #'
  #' @param x the vocabulary to compare, a vector or a dfm
  #' @param y the vocabulary to compare to, a vector or a dfm
  #' @return a the proportion of known terms and all the unknown terms
  voc_x <- vocabulary_extract(x)
  voc_y <- vocabulary_extract(y)
  return(
    list(
      "known" = length(intersect(voc_x, voc_y)) / length(voc_x),
      "unknown" = setdiff(voc_x, voc_y)
    )
  )
}
