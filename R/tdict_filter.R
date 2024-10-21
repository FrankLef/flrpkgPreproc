tdict_filter <- S7::new_generic(
  "TDict",
  dispatch_args = "object",
  fun = function(object, ..., type_rgx = NULL,
                 role_rgx = NULL, process_rgx = NULL, rule_rgx = NULL, pull_var = NULL) {
    checkmate::assert_string(type_rgx, na.ok = TRUE, null.ok = TRUE)
    checkmate::assert_string(role_rgx, na.ok = TRUE, null.ok = TRUE)
    checkmate::assert_string(process_rgx, na.ok = TRUE, null.ok = TRUE)
    checkmate::assert_string(rule_rgx, na.ok = TRUE, null.ok = TRUE)
    checkmate::assert_string(pull_var, na.ok = FALSE, null.ok = TRUE)
    S7::S7_dispatch()
  }
)

#' Filter from a \code{TDict}
#'
#' Filter from a \code{TDict}.
#'
#' The records are filtered using regular expressions. If no criteria is
#' provided for **type**,  **role**, **process** or **rule**, the full data
#' is returned.
#'
#' @name tdict_filter
#'
#' @param object Object of class \code{DDict}.
#' @param type_rgx Regular expression to filter **type**.
#' @param role_rgx Regular expression to filter **role**.
#' @param process_rgx Regular expression to filter **process**.
#' @param rule_rgx Regular expression to filter **rule**.
#' @param pull_var String to identify variable to pull with \code{dplyr::pull}.
#'   If \code{NULL}, the entire table is returned. Default value is \code{NULL}.
#'
#' @return \code{data} from \code{TDict} object.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
S7::method(tdict_filter, TDict) <- function(
    object, type_rgx = NULL,
    role_rgx = NULL, process_rgx = NULL, rule_rgx = NULL, pull_var = NULL) {
  tdict <- tdict_table(object)

  params <- c(
    "type" = type_rgx, "role" = role_rgx, "process" = process_rgx, "rule" = rule_rgx
  )
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
      "x" = sprintf("type regex: %s", type_rgx),
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

  if (is.null(pull_var)) {
    out <- tdict
  } else {
    out <- dplyr::pull(tdict, var = pull_var)
  }
  out
}
