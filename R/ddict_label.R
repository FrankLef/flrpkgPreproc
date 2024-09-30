ddict_label <- S7::new_generic(
  "DDict",
  dispatch_args = "object",
  fun = function(
    object, data, ..., is_raw_nm = FALSE, table_nm = deparse1(substitute(data))) {
    checkmate::assert_data_frame(data)
    checkmate::assert_flag(is_raw_nm)
    checkmate::assert_string(table_nm, min.chars = 1, null.ok = FALSE)
    S7::S7_dispatch()
  })


#' Set labels to columns using a \code{DDict}
#'
#' Set labels to columns using a \code{DDict}.
#'
#' The labels, stored in an object of class \code{DDict}, are used by
#' to set labels with \code{labelled::var_label()}. If the label in
#' \code{DDict} is empty or \code{NA}, an error is returned.
#'
#' @name ddict_label
#'
#' @param object Object of class \code{DDict}.
#' @param data Data.frame with variables to label.
#' @param is_raw_nm \code{FALSE} = use the \code{name} from
#' \code{DDict}; \code{TRUE} = use \code{raw_name} from \code{DDict}.
#' Default is \code{FALSE}.
#' @param table_nm Table name used to filter the dictionary.
#'
#' @importFrom labelled var_label
#'
#' @return \code{data} with labels added to the columns.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
S7::method(ddict_label, DDict) <- function(
    object, data, is_raw_nm = FALSE, table_nm = deparse1(substitute(data))) {

  # cat("\n", "ddict_label: table_nm", "\n")
  # print(table_nm)

  ddict <- ddict_table(object, table_nm = table_nm)
  # cat("\n", "ddict_label: ddict", "\n")
  # print(ddict)

  if (!is_raw_nm) {
    ddict <- ddict |>
      dplyr::select(name, label)
  } else {
    ddict <- ddict |>
      dplyr::select(raw_name, label) |>
      dplyr::rename(name = raw_name)
  }

  ddict <- ddict |>
    dplyr::filter(name %in% names(data))

  if (!nrow(ddict)) {
    msg_head <- cli::col_red("The variables to label where not found in the data.")
    msg_body <- c(
      "x" = sprintf("Table: %s", table_nm),
      "!" = "Verify the argument `is_raw_nm`.",
      "i" = "Verify the raw_name/name column in the data dictionary."
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }

  ddict <- ddict |>
    dplyr::filter(nchar(label) >= 1L, !is.na(label))
  # cat("\n", "ddict_label: ddict", "\n")
  # print(ddict)

  if (!nrow(ddict)) {
    msg_head <- cli::col_yellow("There is no label to apply.")
    msg_body <- c(
      "!" = sprintf("Table: %s", table_nm),
      "i" = "Verify the `label` is not empty in the data dictionary."
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::warn(
      message = msg,
      class = "ValueWarning"
    )
    return(data)
  }

  the_labels <- as.list(ddict$label)
  names(the_labels) <- ddict$name
  labelled::var_label(data) <- the_labels

  data
}
