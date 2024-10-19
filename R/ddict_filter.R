ddict_filter <- S7::new_generic(
  "DDict",
  dispatch_args = "object",
  fun = function(
    object, ..., table_nm = NULL,
    role_rgx = NULL, process_rgx = NULL, rule_rgx = NULL, pull_var = NULL) {
    checkmate::assert_string(table_nm, min.chars = 1, null.ok = TRUE)
    checkmate::assert_string(role_rgx, na.ok = TRUE, null.ok = TRUE)
    checkmate::assert_string(process_rgx, na.ok = TRUE, null.ok = TRUE)
    checkmate::assert_string(rule_rgx, na.ok = TRUE, null.ok = TRUE)
    checkmate::assert_string(pull_var, na.ok = FALSE, null.ok = TRUE)
    S7::S7_dispatch()
  }
)

#' Filter \code{data} from a \code{DDict}
#'
#' Filter \code{data} from a \code{DDict}.
#'
#' The records are filtered using regular expressions. If no criteria is
#' provided for **table_nm**, **role**, **process** and **rule**, the full
#' data is returned. When the filter is \code{NA}, rows with \code{NA} are
#' returned.
#'
#' @name ddict_filter
#'
#' @param object Object of class \code{DDict}.
#' @param table_nm Table name used to filter the dictionary. See details for
#'   more info.
#' @param role_rgx Regular expression to filter **role**. See details for more
#'   info.
#' @param process_rgx Regular expression to filter **process**. See details for
#'   more info.
#' @param rule_rgx Regular expression to filter **rule**. See details for more
#'   info.
#' @param pull_var String to identify variable to pull with \code{dplyr::pull}.
#'   If \code{NULL}, the entire table is returned. Default value is \code{NULL}.
#'
#' @return Filtered \code{data} from \code{DDict} object.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
S7::method(ddict_filter, DDict) <- function(
    object, table_nm = NULL,
    role_rgx = NULL, process_rgx = NULL, rule_rgx = NULL, pull_var = NULL) {
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
    msg_head <- cli::col_red("No records returned from the filtered data dictionary.")
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

  if (is.null(pull_var)) {
    out <- ddict
  } else {
    out <- dplyr::pull(ddict, pull_var)
  }
  out
}
