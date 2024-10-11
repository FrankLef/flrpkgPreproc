#' Replace empty string and \code{NaN, -Inf, Inf} with \code{NA}
#'
#' Replace empty string and \code{NaN, -Inf, Inf} with \code{NA}.
#'
#' This function calls \code{clean_inf} and \code{clean_empty}.
#'
#' @seealso [clean_inf()] [clean_empty()]
#'
#' @param data Data frame.
#' @param na_patterns Character vector of regular expressions to run with
#' \code{stringr::str_replace(x, pattern = stringr::regex(pattern, ignore_case = TRUE),
#' replacement = NA_character_)}.
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
clean_NA <- function(
    data, na_patterns = c("^[[:punct:]]$", "_NA", r"(^[Nn][/\\[[:space:]]]*[Aa]$)")) {
  checkmate::assert_character(na_patterns, min.len = 1)
  data |>
    clean_inf() |>
    clean_empty(na_patterns = na_patterns)
}

#' Replace \code{NaN, -Inf, Inf} with \code{NA}
#'
#' Replace \code{NaN, -Inf, Inf} with \code{NA}.
#'
#' The \code{NaN} don't need to be replaced as they are considered to be
#' \code{NA}. That is \code{is.na(x)} is \code{TRUE} when \code{x = NaN}.
#'
#' @inheritParams clean_NA
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
clean_inf <- function(data) {
  data |> purrr::modify_if(
    .p = tidyselect::where(is.numeric),
    .f = \(x) {
      dplyr::if_else(is.infinite(x), NA, x)
    }
  )
}

#' Replace empty string with \code{NA}
#'
#' Replace empty string with \code{NA}.
#'
#' Replace empty string with \code{NA}. The extra white spaces are removed with
#' \code{stringr::str_squish}. Important: This is done before the
#' conversions to \code{NA}. So that "  " will actually be treated as "".
#' . This behavior affects all the strings.
#'
#' @inheritParams clean_NA
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
clean_empty <- function(
    data, na_patterns = c("^[[:punct:]]$", "_NA", r"(^[Nn][/\\[[:space:]]]*[Aa]$)")) {
  checkmate::assert_character(na_patterns, min.len = 1)

  data <- purrr::modify_if(
    data,
    .p = tidyselect::where(is.character),
    .f = \(x) stringr::str_squish(x)
  )

  for (rgx in na_patterns) {
    data <- data |> purrr::modify_if(
      .p = tidyselect::where(is.character),
      .f = \(x) stringr::str_replace(
        x,
        pattern = stringr::regex(pattern = rgx, ignore_case = TRUE),
        replacement = NA_character_
      )
    )
  }
  data
}
