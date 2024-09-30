ddict_prune_cols <- S7::new_generic(
  "DDict",
  dispatch_args = "object",
  fun = function(
      object, data, ..., is_raw_nm = FALSE, table_nm = deparse1(substitute(data))) {
    checkmate::assert_data_frame(data)
    checkmate::assert_flag(is_raw_nm)
    checkmate::assert_string(table_nm, min.chars = 1, null.ok = FALSE)
    S7::S7_dispatch()
  }
)


#' Remove columns without a role in \code{DDict}
#'
#' Remove columns without a role in \code{DDict}.
#'
#' The information about the role of a variable is stored in the \strong{role}
#' column an object of class \code{DDict}.
#'
#' @name ddict_prune_cols
#'
#' @param object Object of class \code{DDict}.
#' @param data Data.frame with variables to rename.
#' @param is_raw_nm \code{FALSE} = use the \code{name} from
#'   \code{DDict}; \code{TRUE} = use \code{raw_name} from \code{DDict}. Default
#'  is \code{FALSE}.
#' @param table_nm Name of table.
#'
#' @return \code{data} with removed columns.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
S7::method(ddict_prune_cols, DDict) <- function(
    object, data, is_raw_nm = FALSE, table_nm = deparse1(substitute(data))) {
  ddict <- ddict_table(object, table_nm = table_nm)

  if (is_raw_nm) {
    ddict <- ddict |>
      dplyr::select(-name) |>
      dplyr::rename(name = raw_name)
  }

  no_role <- ddict |>
    dplyr::filter(is.na(role) | (nchar(role) == 0L)) |>
    dplyr::pull(name)

  the_names <- names(data)
  the_names_n <- length(the_names)

  to_remove <- the_names[is.element(the_names, no_role)]
  to_remove_n <- length(to_remove)

  check <- (the_names_n - to_remove_n) >= 1L
  if (check) {
    msg_head <- cli::col_yellow("Removing columns without a role.")
    msg_body <- c(
      "i" = sprintf("Table: %s.", table_nm),
      "i" = sprintf("Nb of columns removed: %d.", to_remove_n)
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::warn(
      message = msg,
      class = "ValueWarning"
    )
  } else {
    msg_head <- "At least 1 column must be left in the data."
    msg_head <- cli::col_red(msg_head)
    msg_body <- c(
      "!" = "Verify the settings of `is_raw_nm`.",
      "x" = sprintf("Table: %s", table_nm),
      "x" = sprintf("Nb of columns in the data: %d", the_names_n),
      "x" = sprintf("Nb of columns to be removed: %d", to_remove_n)
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }

  data |>
    dplyr::select(!tidyselect::all_of(to_remove))
}
