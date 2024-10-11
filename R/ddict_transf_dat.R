ddict_transf_dat_ym <- S7::new_generic(
  "DDict",
  dispatch_args = "object",
  fun = function(
      object, data, ..., table_nm = deparse1(substitute(data)),
      suffix = "ym", iso = TRUE) {
    checkmate::assert_string(table_nm, min.chars = 1, null.ok = TRUE)
    S7::S7_dispatch()
  }
)


#' Transform Date Variables to \code{yyyy.mm} Format
#'
#' Transform date variables to \code{yyyy.mm} format.
#'
#' The variables are transformed to a decimal format where the decimal
#' represents the month rather than a fraction of the year as is the usual
#' date decimal format.
#'
#' @name ddict_transf_dat_ym
#'
#' @param object Object of class \code{DDict}.
#' @param data Data frame with date variables.
#' @param table_nm Table name used to filter the dictionary.
#' @param suffix String use as suffix to name the new transformed variables.
#'   Default is \emph{ym}.
#' @param iso Logical. When \code{TRUE} the ISO year will be used for the year.
#'   When \code{FALSE}, the calendar year will be used. Default is \code{TRUE}.
#'
#' @return \code{data} with new transformed columns.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
S7::method(ddict_transf_dat_ym, DDict) <- function(
    object, data, table_nm = deparse1(substitute(data)), suffix = "ym", iso = TRUE) {
  rgx <- paste0("\\b", suffix, "\\b")
  nms <- ddict_filter(object,
    table_nm = table_nm,
    role_rgx = ".+", process_rgx = rgx
  ) |>
    pull(name)
  col_suffix <- paste0("{.col}_", suffix)
  dplyr::mutate(data, across(
    .cols = any_of(nms),
    .fns = \(x) {
      if (iso) {
        y <- lubridate::isoyear(x) + (lubridate::month(x) / 100)
      } else {
        y <- lubridate::year(x) + (lubridate::month(x) / 100)
      }
      y
    },
    .names = col_suffix
  ))
}


ddict_transf_dat_wk <- S7::new_generic(
  "DDict",
  dispatch_args = "object",
  fun = function(
      object, data, ..., table_nm = deparse1(substitute(data)), suffix = "wk") {
    checkmate::assert_string(table_nm, min.chars = 1, null.ok = TRUE)
    S7::S7_dispatch()
  }
)


#' Transform Date Variables to \code{floor(date)} by Week
#'
#' Transform date variables to \code{floor(date)} by week.
#'
#' The date are converted to the first day of the week when they occur. It uses
#' the function \code{floor_date(x, unit = "weeks")} from \pkg{lubridate}.
#'
#' @name ddict_transf_dat_wk
#'
#' @param object Object of class \code{DDict}.
#' @param data Data frame with date variables.
#' @param table_nm Table name used to filter the dictionary.
#' @param suffix String use as suffix to name the new transformed variables.
#'   Default is \emph{wk}.
#'
#' @return \code{data} with new transformed columns.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
S7::method(ddict_transf_dat_wk, DDict) <- function(
    object, data, table_nm = deparse1(substitute(data)), suffix = "wk") {
  rgx <- paste0("\\b", suffix, "\\b")
  nms <- ddict_filter(object,
    table_nm = table_nm,
    role_rgx = ".+", process_rgx = rgx
  ) |>
    pull(name)
  col_suffix <- paste0("{.col}_", suffix)
  dplyr::mutate(data, across(
    .cols = any_of(nms),
    .fns = \(x) {
      lubridate::floor_date(x, unit = "weeks")
    },
    .names = col_suffix
  ))
}
