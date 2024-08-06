#' Get the range using a factor of the median
#'
#' Get the range using a factor of the median.
#'
#' This will compute ranges where the lower boundary is \code{median / tol} and
#' the upper boundary is \code{median * tol}. The tolerance \code{tol} is
#' loosely (\emph{very loosely}) based on hidiroglou-berthelot which is why the
#' function is called \code{hbRules}.
#'
#' @details hidiroglou-berthelot:
#' Normally, multiplying then dividing the median to obtain boundaries is more
#' appropriate for skewed data. In a business case, the data is very often
#' skewed which is fine in that case. However in a business context, when
#' applied to more centrally distributed data,  such as as log-transformed sales
#' amount per invoice for example, it is unlikely that high
#' amounts are mistakes and should be selected. However, very large amounts
#' should be. In such a practical business context, where the upper bound is
#' relatively larger than the lower bound, experience has shown this
#' method to be quite relevant, robust and easy to understand.
#'
#' @param data Data.frame .
#' @param cols Character() with the names of the columns to compute.
#' @param tol Tolerance number. Must be between 51/49 and 99. Default is 80/20.
#' @param na.rm Flag used by \code{median(x, na.rm)} and \code{mad(x, na.rm)}.
#' Default value is \code{TRUE}.
#'
#' @return List of ranges with one element per column.
#'
#' @importFrom stats median
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
valid_range_hb <- function(data, cols, tol = 80 / 20, na.rm = TRUE) {
  checkmate::assert_data_frame(data, min.rows = 2, min.cols = 1)
  checkmate::assert_names(cols, subset.of = names(data))
  checkmate::assert_number(tol, lower = 51 / 49, upper = 99, finite = TRUE)
  data |>
    dplyr::select(tidyselect::all_of(cols)) |>
    purrr::map(.f = \(x) {
      med <- median(x, na.rm = na.rm)
      c("min" = med / tol, "max" = med * tol)
    })
}

#' Get the range using the normalized MAD
#'
#' Get the range using the normalized MAD.
#'
#' This method is applicable when the data is more or less centralized.
#' It is robust to deviations. It uses the \emph{median absolute deviation, MAD}.
#' It is described in many books.  For example, it could also be computed using
#' the \pkg{performance} package with \code{performance::check_outliers()}.
#'
#' \code{tol} is a percentage representing the probabilities, or proportion of data,
#' to keep. It is the \code{p} in \code{stats::qchisq(prob, df = 1)}. The
#' outliers are selected from the left and right tails. That is, for example,
#' with \code{tol = 0.95} 2.5% will be selected from the left and 2.5% will be
#' selected from the right.
#'
#' @section \pkg{Performance}:
#' If more outlier detection methods are required, please see the
#' \pkg{performance} at
#' \href{https://easystats.github.io/performance/index.html}{performance}. This
#' package is packed with very useful utilities.
#'
#' @param data Data.frame .
#' @param cols Character() with the names of the columns to compute.
#' @param tol A percentage representing the probabilities, or proportion of data,
#' to keep. It is the \code{p} in \code{stats::qchisq(prob, df = 1)}. Default
#' is 0.95. Must be between 0.51 and 0.9999.
#' @param na.rm logical. Should missing values (including \code{NaN}) be removed?
#'
#' @return List of ranges with one element per column.
#'
#' @importFrom stats median mad na.omit qchisq
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
valid_range_madn <- function(data, cols, tol = 0.95, na.rm = TRUE) {
  checkmate::assert_data_frame(data, min.rows = 2, min.cols = 1)
  checkmate::assert_names(cols, subset.of = names(data))
  checkmate::assert_number(tol, lower = 0.51, upper = 0.9999, finite = TRUE)
  data |>
    dplyr::select(tidyselect::all_of(cols)) |>
    purrr::map(.f = \(x) {
      if (na.rm) x <- na.omit(x)
      alpha <- (1 - tol) / 2
      prob <- 1 - alpha
      chi <- sqrt(qchisq(prob, df = 1))
      base <- chi * mad(x, na.rm = na.rm)
      med <- median(x, na.rm = na.rm)
      c("min" = med - base, "max" = med + base)
    })
}
