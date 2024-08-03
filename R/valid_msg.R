#' Write information message on valid/invalid pct
#'
#' Write information message on valid/invalid pct.
#'
#' Write information message on valid/invalid pct. Mainly use with \pkg{logging}.
#'
#' @param validation Object of class \code{validation} created with
#' \code{validate::validator}.
#' @param suffix Suffix representing the type of validation.
#'
#' @return String used for message.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
valid_msg_pct <- function(validation, suffix) {
  checkmate::assert_class(validation, classes = "validation")
  checkmate::assert_names(suffix)

  invalid_nb <- sum(!validate::values(validation))
  valid_nb <- sum(validate::values(validation))
  tot_nb <- invalid_nb + valid_nb
  assertthat::assert_that(tot_nb >= 1L)

  invalid_pct <- invalid_nb / tot_nb
  valid_pct <- valid_nb / tot_nb

  sprintf(
    "%s: Validation completed. %.1f%% invalid, %.1f%% valid.", toupper(suffix),
    100 * invalid_pct, 100 * valid_pct
  )
}
