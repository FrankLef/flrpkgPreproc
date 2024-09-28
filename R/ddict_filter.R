#' Filter from a \code{DDict}
#'
#' Filter from a \code{DDict}.
#'
#' The records are filtered using regular expressions. If no criteria is
#' provided for **role**, **process** and **rule**, the full data is returned.
#'
#' @name ddict_filter
#'
#' @param object Object of class \code{DDict}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{table_nm}{Compulsory name of the table.}
#'    \item{role_rgx}{Regular expression to filter **role**.}
#'    \item{process_rgx}{Regular expression to filter **process**.}
#'    \item{rule_rgx}{Regular expression to filter **rule**.}
#' }
#'
#' @return \code{data} from \code{DDict} object.
#'
#' @importFrom dplyr filter
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
ddict_filter <- S7::new_generic("DDict", dispatch_args = "object")

S7::method(ddict_filter, DDict) <- function(
    object, table_nm = "", role_rgx = NULL, process_rgx = NULL, rule_rgx = NULL) {
  checkmate::assert_string(table_nm, min.chars = 1)
  checkmate::assert_string(role_rgx, na.ok = TRUE, null.ok = TRUE)
  checkmate::assert_string(process_rgx, na.ok = TRUE, null.ok = TRUE)
  checkmate::assert_string(rule_rgx, na.ok = TRUE, null.ok = TRUE)

  ddict <- ddict_table(object, table_nm = table_nm)

  params <- c("role" = role_rgx, "process" = process_rgx, "rule" = rule_rgx)
  for (nm in names(params)) {
    rgx <- params[nm]
    if (!is.na(rgx)) {
      ddict <- dplyr::filter(ddict, grepl(pattern = rgx, x = .data[[nm]]))
    } else {
      ddict <- dplyr::filter(ddict, is.na(.data[[nm]]))
    }
  }


  if (!nrow(ddict)) {
    msg_head <- cli::col_red("No records returned from the data dictionary.")
    msg_body <- c(
      "i" = "Verify the regular expressions used to filter the data.",
      "x" = sprintf("table name: %s", table_nm),
      "x" = sprintf("role regex: %s", role_rgx),
      "x" = sprintf("process regex: %s", process_rgx),
      "x" = sprintf("rule regex: %s", rule_rgx)
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }

  ddict
}
