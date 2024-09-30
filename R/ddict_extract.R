ddict_extract <- S7::new_generic(
  "DDict",
  dispatch_args = "object",
  fun = function(
      object, data, ..., table_nm = deparse1(substitute(data))) {
    checkmate::assert_data_frame(data)
    checkmate::assert_string(table_nm, min.chars = 1)
    S7::S7_dispatch()
  }
)



#' Extract information about data to a \code{DDict}
#'
#' Extract information about data to a \code{DDict}.
#'
#' The information will be stored in an object of class \code{DDict}.
#'
#' @name ddict_extract
#'
#' @param object Object of class \code{DDict}.
#' @param data Data.frame from which to extract the variables' details.
#' @param table_nm Name of table. Used when doing loop or when \code{data}
#'   is from a function argument.
#'
#' @return Object of class \code{DDict}.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
S7::method(ddict_extract, DDict) <- function(
    object, data, table_nm = deparse1(substitute(data))) {
  # cat("\n", "table", "\n")
  # print(the_table)
  # cat("\n", "variables", "\n")
  the_variables <- sapply(X = data, FUN = \(x) class(x)[1])
  # print(the_variables)
  df <- data.frame(
    table = table_nm,
    raw_name = names(the_variables),
    name = names(the_variables),
    label = NA_character_,
    raw_dtype = unname(the_variables),
    dtype = unname(the_variables),
    role = NA_character_,
    process = NA_character_,
    rule = NA_character_,
    desc = NA_character_,
    note = NA_character_
  )
  new_data <- rbind(object@data, df)
  DDict(new_data)
}
