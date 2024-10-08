#' Find Variables With Near-Zero Variance
#'
#' Find variables With near-zero variance.
#'
#' \code{find_nzv} diagnoses predictors that have one unique value (i.e. are
#' zero variance predictors) or predictors that are have both of the following
#' characteristics: they have very few unique values relative to the number of
#' samples and the ratio of the frequency of the most common value to the
#' frequency of the second most common value is large.
#'
#' For example, an example of near zero variance predictor is one that, for
#' 1000 samples, has two distinct values and 999 of them are a single value.
#'
#' To be flagged the both following conditions must be met:
#' \enumerate{
#'    \item first the frequency of the most prevalent value over the
#' second most frequent value (called the ``frequency ratio'') must be above
#' \code{freqCut}.
#'    \item Secondly, the ``percent of unique values,'' the number of
#' unique values divided by the total number of samples (times 100), must also
#' be below \code{uniqueCut}.
#' }
#' In the above example, the frequency ratio is 999 and the unique value
#' percentage is 0.0001.
#'
#'
#' @param x a numeric vector or matrix, or a data frame with all numeric data.
#' @param freqCut the cutoff for the ratio of the most common value to the
#' second most common value.
#' @param uniqueCut the cutoff for the percentage of distinct values out of the
#' number of total samples.
#' @param saveMetrics a logical. If false, the positions of the zero- or
#' near-zero predictors is returned. If true, a data frame with predictor
#' information is returned.
#'
#' @return For \code{nearZeroVar}: if \code{saveMetrics = FALSE}, a vector of
#' integers corresponding to the column positions of the problematic
#' predictors. If \code{saveMetrics = TRUE}, a data frame with columns:
#' \item{freqRatio }{the ratio of frequencies for the most common value over
#' the second most common value} \item{percentUnique }{the percentage of unique
#' data points out of the total number of data points} \item{zeroVar }{a vector
#' of logicals for whether the predictor has only one distinct value} \item{nzv
#' }{a vector of logicals for whether the predictor is a near zero variance
#' predictor}
#'
#' @author Max Kuhn
#' @source \pkg{caret} package.
#' @examples
#' df <- iris[, -5]
#' n <- 5
#' df$test_nzv <- c(rep.int(1, times = n), rep.int(2, times = nrow(df) - n))
#' df$test_zv <- rep.int(1, times = nrow(df))
#' out <- clean_num_nzv(df, freqCut = 95 / 5, uniqueCut = 10, saveMetrics = FALSE)
#' identical(x = out, y = c("test_nzv" = 5L, "test_zv" = 6L))
#'
#' @export
clean_num_nzv <- function(x, freqCut = 95 / 5, uniqueCut = 10, saveMetrics = FALSE) {
  if (is.null(dim(x))) x <- matrix(x, ncol = 1)
  freqRatio <- apply(x, 2, function(data) {
    t <- table(data[!is.na(data)])
    if (length(t) <= 1) {
      return(0)
    }
    w <- which.max(t)
    return(max(t, na.rm = TRUE) / max(t[-w], na.rm = TRUE))
  })
  lunique <- apply(x, 2, function(data) length(unique(data[!is.na(data)])))
  percentUnique <- 100 * lunique / apply(x, 2, length)
  zeroVar <- (lunique == 1) | apply(x, 2, function(data) all(is.na(data)))
  if (saveMetrics) {
    out <- data.frame(
      freqRatio = freqRatio,
      percentUnique = percentUnique,
      zeroVar = zeroVar,
      nzv = (freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar
    )
  } else {
    out <- which((freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
  }
  out
}
