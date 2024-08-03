#' Create data.frame of out-of-bound rules
#'
#' Create data.frame of out-of-bound rules used by \pkg{validate}.
#'
#' The data.frame includes the name of the rule, its label and the rule itself.
#' All rule names end with the \code{suffx} '_oob'.
#'
#' @param ranges List of variable names with their respective range.
#' @param suffix Suffix to add to create the rules name. Default to 'oob'.
#'
#' @return Data frame of rules usable by \code{validate::validator()}.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
valid_rules_rng <- function(ranges, suffix = "oob") {
  checkmate::assert_list(ranges, min.len = 1)
  checkmate::assert_string(suffix, min.chars = 1)
  purrr::map_df(.x = names(ranges), .f = \(nm) {
    a_range <- ranges[[nm]]
    checkmate::assert_numeric(a_range, len = 2, unique = TRUE)
    checkmate::assert_names(names(a_range), identical.to = c("min", "max"))
    nm_suffix <- paste(nm, suffix, sep = "_")
    a_min <- a_range["min"]
    a_min <- paste("min", "=", a_min)
    a_max <- a_range["max"]
    a_max <- paste("max", "=", a_max)
    a_rule <- paste0("in_range(", nm, ", ", a_min, ", ", a_max, ")")
    c("name" = nm_suffix, "label" = nm_suffix, "rule" = a_rule)
  })
}
