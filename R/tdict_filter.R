#' Filter from a \code{TDict}
#'
#' Filter from a \code{TDict}.
#'
#' The records are filtered using regular expressions. If no criteria is
#' provided for **role**, **process** and **rule**, the full data is returned.
#'
#' @name tdict_filter
#'
#' @param object Object of class \code{DDict}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{role_rgx}{Regular expression to filter **role**.}
#'    \item{process_rgx}{Regular expression to filter **process**.}
#'    \item{rule_rgx}{Regular expression to filter **rule**.}
#' }
#'
#' @return \code{data} from \code{TDict} object.
#'
#' @importFrom dplyr filter
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
tdict_filter <- S7::new_generic("TDict", dispatch_args = "object")

S7::method(tdict_filter, TDict) <- function(
    object, role_rgx = NULL, process_rgx = NULL, rule_rgx = NULL) {
  checkmate::assert_string(role_rgx, na.ok = TRUE, null.ok = TRUE)
  checkmate::assert_string(process_rgx, na.ok = TRUE, null.ok = TRUE)
  checkmate::assert_string(rule_rgx, na.ok = TRUE, null.ok = TRUE)

  tdict <- object@data

  if (!nrow(tdict)) {
    msg_head <- cli::col_red("The table dictionary is empty.")
    msg_body <- c(
      "x" = "There is nothing in the data dictionary.."
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }


  params <- c("role" = role_rgx, "process" = process_rgx, "rule" = rule_rgx)
  for (nm in names(params)) {
    rgx <- params[nm]
    if (!is.na(rgx)) {
      tdict <- dplyr::filter(tdict, grepl(pattern = rgx, x = .data[[nm]]))
    } else {
      tdict <- dplyr::filter(tdict, is.na(.data[[nm]]))
    }
  }


  if (!nrow(tdict)) {
    msg_head <- cli::col_red("No records returned from the table dictionary.")
    msg_body <- c(
      "i" = "Verify the regular expressions used to filter the data.",
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

  tdict
}
