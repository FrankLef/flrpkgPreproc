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
valid_rules <- function(ranges, suffix = c("oob", "madn")) {
  checkmate::assert_list(ranges, min.len = 1)

  suffix <- match.arg(suffix)

  purrr::map_df(.x = names(ranges), .f = \(nm) {
    a_range <- ranges[[nm]]
    nm_suffix <- paste(nm, suffix, sep = "_")
    a_rule <- valid_rule_rng(nm, rng = a_range)
    c("name" = nm_suffix, "label" = nm_suffix, "rule" = a_rule)
  }) |>
    as.data.frame()
}


#' Write a string for a rule With range
#'
#' Write a string for a rule with range.
#'
#' The rule is used by the \pkg{validate} package and is in the form of
#' \bold{in_range(column name, minimum, maximum)}.
#'
#'
#' @param x String representing the column name.
#' @param rng Numeric(2) with the range's boundaries.
#'
#' @return String with the rule usueable by \pkg{validate}.
#' @export
#'
#' @examples
#' rule <- valid_rule_rng("var", rng = c("min" = 0, "max" = 1))
#' stopifnot(identical(rule, "in_range(var, min = 0, max = 1)"))
valid_rule_rng <- function(x, rng) {
  checkmate::assert_numeric(rng, len = 2, unique = TRUE)
  checkmate::assert_names(names(rng), identical.to = c("min", "max"))
  a_min <- paste("min", "=", rng["min"])
  a_max <- paste("max", "=", rng["max"])
  paste0("in_range(", x, ", ", a_min, ", ", a_max, ")")
}
