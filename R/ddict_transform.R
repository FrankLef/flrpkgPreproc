ddict_transform <- S7::new_generic(
  "DDict",
  dispatch_args = "object",
  fun = function(object, data, fn, suffix, ...,
                 table_nm = deparse1(substitute(data)), col_nm = paste0("{.col}_", suffix)) {
    checkmate::assert_string(table_nm, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_function(fn)
    checkmate::assert_string(suffix, min.chars = 1L)
    S7::S7_dispatch()
  }
)


#' Transform Variables with a \code{DDict} and Custom Function
#'
#' Transform variables with a \code{DDict} and custom function.
#'
#' The data is transformed using a function defined in \code{fn} based on a
#' filter \code{process == suffix} from a \code{DDict}. The suffix is then
#' appended to the original name of the variable to create a new column in
#' \code{data}. When \code{col_nm = NULL}, no new column is created, the
#' original column is however transformed.
#'
#' @name ddict_transform
#'
#' @param object Object of class \code{DDict}.
#' @param data Data frame with date variables.
#' @param fn Custom function.
#' @param suffix String use as suffix to name the new transformed variables.
#' @param table_nm Table name used to filter the dictionary.
#' @param col_nm A \pkg{glue} specification for added column. See help for
#'   \code{across} in \pkg{dplyr}. In particular, \code{col_nm = NULL} is
#'   equivalent \code{col_nm = "{.col}"}.
#'
#' @return \code{data} with new transformed columns.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
S7::method(ddict_transform, DDict) <- function(
    object, data, fn, suffix,
    table_nm = deparse1(substitute(data)), col_nm = paste0("{.col}_", suffix)) {
  rgx <- paste0("\\b", suffix, "\\b")
  nms <- ddict_filter(object, table_nm = table_nm, process_rgx = rgx) |>
    dplyr::pull(name)
  dplyr::mutate(data, dplyr::across(
    .cols = tidyselect::any_of(nms),
    .fns = \(x) {
      fn(x)
    },
    .names = col_nm
  ))
}
