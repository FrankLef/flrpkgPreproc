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
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{data}{Data.frame with variables to label.}
#'    \item{is_raw_nm}{\code{FALSE} (default) = use the \code{name} from
#' \code{DDict}; \code{TRUE} = use \code{raw_name} from \code{DDict}.}
#'    \item{table_nm}{Name of table. Used when doing loop or when \code{data}
#'    is from a function argument.}
#' }
#'
#' @return \code{data} with labels added to the columns.
#'
#' @importFrom labelled var_label
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
ddict_label <- S7::new_generic("DDict", dispatch_args = "object")

S7::method(ddict_label, DDict) <- function(
    object, data, is_raw_nm = FALSE, table_nm = deparse1(substitute(data))) {
  checkmate::assert_data_frame(data)
  checkmate::assert_flag(is_raw_nm)
  checkmate::assert_string(table_nm, min.chars = 1)

  # cat("\n", "ddict_label: table_nm", "\n")
  # print(table_nm)

  ddict <- ddict_table(object, table_nm = table_nm)
  # cat("\n", "ddict_label: ddict", "\n")
  # print(ddict)

  if (!is_raw_nm) {
    lbl <- ddict |>
      dplyr::select(name, label)
  } else {
    lbl <- ddict |>
      dplyr::select(raw_name, label) |>
      dplyr::rename(name = raw_name)
  }

  lbl <- lbl |>
    dplyr::filter(name %in% names(data))

  if (!nrow(lbl)) {
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

  lbl <- lbl |>
    dplyr::filter(nchar(label) >= 1L, !is.na(label))
  # cat("\n", "ddict_label: lbl", "\n")
  # print(lbl)

  if (!nrow(lbl)) {
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

  the_labels <- as.list(lbl$label)
  names(the_labels) <- lbl$name
  labelled::var_label(data) <- the_labels

  data
}
