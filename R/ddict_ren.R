#' Rename columns using a \code{DDict}
#'
#' Rename columns using a \code{DDict}.
#'
#' The information is stored in an object of class \code{DDict}.
#'
#' @name ddict_ren
#'
#' @param object Object of class \code{DDict}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{data}{Data.frame with variables to rename.}
#'    \item{table_nm}{Name of table. Used when doing loop or when \code{data}
#'    is from a function argument.}
#' }
#'
#' @return \code{data} with renamed columns.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
ddict_ren <- S7::new_generic("DDict", dispatch_args = "object")

S7::method(ddict_ren, DDict) <- function(
    object, data, table_nm = deparse1(substitute(data))) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(table_nm, min.chars = 1)

  ddict <- ddict_table(object, table_nm = table_nm)

  ddict <- ddict |>
    dplyr::mutate(pos = match(raw_name, names(data))) |>
    dplyr::filter(!is.na(pos))

  if (!nrow(ddict)) {
    msg_head <- cli::col_red("No `raw_name` found in the data names.")
    msg_body <- c(
      "i" = "Maybe the names have already been changed?",
      "x" = sprintf("Table: %s", table_nm)
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }

  ddict <- ddict |>
    dplyr::filter(raw_name != name)

  if (!nrow(ddict)) {
    msg_head <- cli::col_yellow("There is no column to rename.")
    msg_body <- c(
      "!" = sprintf("Table: %s", table_nm),
      "i" = "The `raw_name` and `name` must be doifferent to trigger renaming."
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::warn(
      message = msg,
      class = "ValueWarning"
    )
    return(data)
  }

  names(data)[ddict$pos] <- ddict$name

  data
}
