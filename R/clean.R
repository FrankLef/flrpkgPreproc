#' Replace empty string and \code{NaN, -Inf, Inf} with \code{NA}
#'
#' Replace empty string and \code{NaN, -Inf, Inf} with \code{NA}.
#'
#' This function calls \code{clean_nan_inf} and \code{clean_empty}.
#'
#' @seealso clean_inf clean_empty
#'
#' @param data Data frame.
#' @param na_patterns Character vector of regular expressions to run with
#' \code{stringr::str_replace(x, pattern = pattern, replacement = NA_character_)}.
#' Default value is \code{c("^[[:punct:]]$", r"(^[Nn][/\\][Aa]$)")}.
#' @param na_strings Character() with strings to b replaced by \code{NA}. You
#' must always keep "n/a" and "".
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
clean_NA <- function(data,
                     na_patterns = c("^[[:punct:]]$", r"(^[Nn][/\\][Aa]$)"),
                     na_strings = c("", "_NA")) {
  data |>
    clean_inf() |>
    clean_empty(na_patterns = na_patterns, na_strings = na_strings)
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
  data |> dplyr::mutate(dplyr::across(
    .cols = tidyselect::where(is.numeric),
    .fns = \(x) dplyr::if_else(is.infinite(x), NA, x)
  ))
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
#' The following strings are replaced with \code{NA}.
#' \itemize{
#'  \item Single punctuation, the regex in R is \code{"^[[:punct:]]$"}.
#'  \item All variant of "n/a". the regex in R is \code{r"(^[Nn][/\\][Aa]$)"}.
#' }
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
clean_empty <- function(data,
                        na_patterns = c("^[[:punct:]]$", r"(^[Nn][/\\][Aa]$)"),
                        na_strings = c("", "_NA")) {
  checkmate::assert_character(na_strings)

  # clean extra white spaces
  data <- data |>
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::where(is.character),
      .fns = stringr::str_squish
    ))

  for (i in seq_along(na_patterns)) {
    data <- dplyr::mutate(
      data,
      dplyr::across(
        .cols = tidyselect::where(is.character),
        .fns = \(x) stringr::str_replace(
          x,
          pattern = na_patterns[i], replacement = NA_character_
        )
      )
    )
  }

  for (i in seq_along(na_strings)) {
    data <- dplyr::mutate(
      data,
      dplyr::across(
        .cols = tidyselect::where(is.character),
        .fns = \(x) dplyr::na_if(x, y = na_strings[i])
      )
    )
  }
  data
}
