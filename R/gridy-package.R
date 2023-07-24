## usethis namespace: start
#' @useDynLib gridy, .registration = TRUE
## usethis namespace: end
NULL
## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @import data.table
## usethis namespace: end
NULL

## quiets concerns of R CMD check : the .'s that appear in data.table
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
