#' Find Variables With a constant Value
#'
#' Find Variables with a constant value.
#'
#' A named integer() will be returned with
#' the names and positions of the columns having a constant value.
#'
#' @param data Data frame.
#'
#' @return Integer() with positions and names.
#' @export
#'
#' @examples
#' n <- 10L
#' df <- data.frame(
#'   ran_int = rpois(n, lambda = 5),
#'   ran_text = sample(letters[1:10], size = n, replace = TRUE),
#'   test_text = "abc",
#'   test_num = 1
#' )
#' out <- clean_cols_constant(df)
#' stopifnot(identical(x = out, y = c("test_text" = 3L, "test_num" = 4L)))
clean_cols_constant <- function(data) {
  nms <- sapply(data, FUN = \(x) {
    (sum(!duplicated(x)) == 1L)
  })
  nms <- nms[nms] # remove FALSE items
  pos <- match(names(nms), names(data)) # get position
  names(pos) <- names(nms) # add names to position vector
  pos
}



#' Find Variables With Near-Zero Variance
#'
#' Find variables with near-zero variance.
#'
#' \code{clean_cols_nzv} diagnoses predictors that have one unique value (i.e. are
#' zero variance predictors) or predictors that are have both of the following
#' characteristics: they have very few unique values relative to the number of
#' samples and the ratio of the frequency of the most common value to the
#' frequency of the second most common value is large.
#'
#'
#' @section Near-zero variance:
#' To be flagged as near-zero variance, both following conditions must be met:
#' \describe{
#'    \item{frequency ratio}{The frequency of the most prevalent value over the
#' frequency of the second most frequent value, called the \emph{frequency ratio}
#' must be above \code{freq_tol}.}
#'    \item{percent of unique values}{The number of unique values divided by
#'    the total number of samples, must be below \code{uniq_tol}.}
#' }
#'
#'
#' @param data Data frame..
#' @param freq_tol The cutoff for the ratio of the most common value to the
#' second most common value. Must be between (51 / 49) and (99 / 1). Default
#' is 95 / 5. Many people use 90 / 10.
#' @param uniq_tol The cutoff for the percentage of distinct values out of the
#' number of total samples.
#' @param info Logical. If \code{FALSE}, the positions near-zero variance
#' variables is returned in a named integer(). If \code{TRUE}, a data frame
#' with variables and all information is returned. Default is \code{FALSE}
#'
#' @return For \code{nearZeroVar}: if \code{info = FALSE}, a vector of
#' integers corresponding to the column positions of the problematic
#' variables. If \code{info = TRUE}, a data frame with details by variable.
#' @export
#'
#' @author Max Kuhn
#' @source \pkg{caret} package.
#' @examples
#' n <- 50L
#' df <- data.frame(
#'   ran_int = rpois(n, lambda = 5),
#'   ran_num = rnorm(n),
#'   test_nzv = c(rep.int(1, times = 1), rep.int(2, times = n - 1)),
#'   test_zv = 1
#' )
#' out <- clean_cols_nzv(df, freq_tol = 95 / 5, uniq_tol = 0.1, info = FALSE)
#' stopifnot(identical(x = out, y = c("test_nzv" = 3L, "test_zv" = 4L)))
clean_cols_nzv <- function(data, freq_tol = 95 / 5, uniq_tol = 0.1, info = FALSE) {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_double(freq_tol, lower = 51 / 49, upper = 99 / 1)
  checkmate::assert_double(uniq_tol, lower = 0.01, upper = 0.99)

  out <- data |>
    dplyr::select(tidyselect::where(is.numeric)) |>
    purrr::map(.f = \(x) {
      freq <- table(x[!is.na(x)])
      freq_not_max <- freq[-which.max(freq)]
      if (length(freq_not_max)) {
        freq_ratio <- max(freq, na.rm = TRUE) / max(freq_not_max, na.rm = TRUE)
      } else {
        freq_ratio <- 0L
      }
      uniq_nb <- length(freq)
      uniq_pct <- uniq_nb / length(x)
      zero_var <- (uniq_nb == 1L) | all(is.na(x))
      nzv <- ((freq_ratio > freq_tol) & (uniq_pct <= uniq_tol)) | zero_var
      data.frame(
        "freq_ratio" = freq_ratio, "uniq_nb" = uniq_nb, "uniq_pct" = uniq_pct,
        "zero_var" = zero_var, "near_zero_var" = nzv
      )
    }) |>
    purrr::list_rbind(names_to = "id")

  if (!info) {
    nms <- out$id[out$near_zero_var]
    out <- which(names(data) %in% nms)
    names(out) <- nms
  }

  out
}
