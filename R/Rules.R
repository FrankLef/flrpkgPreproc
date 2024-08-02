#' Create a \code{Rules} Object
#'
#' Create a \code{Rules} object.
#'
#' Create a \code{Rules} object used to to managed validation rules as they are
#' typically used by the \pkg{validate} package.
#'
#'
#' @return Object of class \code{Rules}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
Rules <- S7::new_class("Rules",
  package = "flrpkgPreproc",
  properties = list(
    specs = S7::new_property(class = S7::class_list),
    rules = S7::new_property(class = S7::class_data.frame)
  ),
  validator = function(self) {
    vars <- c("name" = "character", "label" = "character", "rule" = "character")
    check <- checkmate::check_data_frame(
      self@rules,
      types = vars,
      ncols = length(vars)
    )
    if (is.character(check)) {
      rlang::abort(
        message = check,
        class = "ValueError"
      )
    }
  },
  constructor = function() {
    S7::new_object(
      Rules,
      specs = list(),
      rules = data.frame(
        name = character(),
        label = character(),
        rule = character()
      )
    )
  }
)

#' Get the Range Using a Factor of the Median
#'
#' Get the range using a factor of the median.
#'
#' This will compute ranges where the lower boundary is \code{median / tol} and
#' the upper boundary is \code{median * tol}. The tolerance \code{tol} is
#' loosely (very loosely) based on hidiroglou-berthelot which is why the
#' function is called \code{hbRules}.
#'
#' @details info:
#' Normally, multiplying then dividing the median to obtain boundaries is more
#' appropriate for skewed data. In a business case, the data is very often
#' skewed which is fine in that case. However in a business context, when
#' applied to more centrally distributed data,  such as as log-transformed sales
#' amount per invoice for example, it is unlikely that high
#' amounts are mistakes and should be selected. However, very large amounts
#' should. In such a practical business context, experience has shown this
#' method to be quite relevant.
#'
#'
#' @name hbRules
#'
#' @param object Object of class \code{Rules}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{data}{Data.frame to process.}
#'    \item{cols}{Character vector of column names to process.}
#'    \item{tol}{Tolerance number. Must be between 1 and 100. Default value
#'    is 80/20.}
#'    \item{na.rm}{Flag used by \code{median(x, na.rm)}. Default value is
#'    \code{TRUE}.}
#' }
#'
#'
#' @return List of ranges.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
hbRules <- S7::new_generic("hbRules", dispatch_args = "object")

S7::method(hbRules, Rules) <- function(
    object, data, cols, tol = 80 / 20, na.rm = TRUE) {
  checkmate::assert_data_frame(data, min.cols = 2, min.rows = 1)
  checkmate::assert_names(cols, subset.of = names(data))
  checkmate::assert_number(tol, lower = 1, upper = 99, finite = TRUE)

  data
}
