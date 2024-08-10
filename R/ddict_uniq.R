#' Validate that columns have unique values
#'
#' Validate that columns have unique values.
#'
#' The columns are selected from the \code{DDict} when their role match
#' the regular expression in \code{uniq_rgx}. If \code{uniq_rgx} does not find a
#' match, and error will be thrown.
#'
#'
#' @name ddict_uniq
#'
#' @param object Object of class \code{DDict}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{data}{Data.frame with variables to test for uniqueness.}
#'    \item{role_rgx}{Reguular expression to select variables with this role.
#'    Default value is \code{r"(\buniq\b)"}.}
#'    \item{table_nm}{Name of the table.}
#' }
#'
#' @return Logical named vector with the results. TRUE is when the variable has
#' unique values. FALSE otherwise.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
ddict_uniq <- S7::new_generic("DDict", dispatch_args = "object")

S7::method(ddict_uniq, DDict) <- function(
    object, data, role_rgx = r"(\buniq\b)", table_nm = deparse1(substitute(data))) {
  checkmate::assert_data_frame(data, min.cols = 1)
  checkmate::assert_string(role_rgx, min.chars = 1)
  checkmate::assert_string(table_nm, min.chars = 1)


  cols <- object@data |>
    dplyr::filter(table == table_nm, grepl(pattern = role_rgx, x = role)) |>
    dplyr::pull(name)


  if (!length(cols)) {
    msg_head <- cli::col_red("There is no column identified as unique.")
    msg_body <- c(
      "x" = sprintf("Table: %s", table_nm),
      "x" = sprintf("Role rgx : %s", role_rgx),
      "!" = "Verify the uniq regex in argument and the `role` column in data."
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }

  df <- data[, cols]
  sapply(df, FUN = \(x) !any(duplicated(x)))
}
