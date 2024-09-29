#' Create \code{TDict} Object From Data in Excel
#'
#' Create \code{TDict} object from data in Excel.
#'
#' Read an Excel file and load it into an object of class \code{TDict}.
#'
#' @param path Path where the excel is located.
#' @param file Name of the excel file. Default is \emph{tdict.xlsx}.
#' @param sheet Name of Excel sheet. Default is \emph{data}.
#'
#' @return Object of class \code{TDict}.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
tdict_read_xl <- function(path, file = "tdict.xlsx", sheet = "data") {
  fn <- file.path(path, file)
  df <- readxl::read_xlsx(fn, sheet = sheet, col_types = "text")
  TDict(df)
}


#' Create \code{TDict} Object From Filtered Data in Excel
#'
#' Create \code{TDict} object from filtered data in Excel.
#'
#' Read an Excel file and load it into an object of class \code{TDict}. The data
#' is filtered on column \code{process} with \emph{load}.
#'
#' @param path Path where the excel is located.
#' @param file Name of the excel file. Default is \emph{tdict.xlsx}.
#' @param sheet Name of Excel sheet. Default is \emph{data}.
#' @param process String used to filter the \code{process} column. Default
#'   to \emph{load}.
#'
#' @return Object of class \code{TDict}.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
tdict_read_xl_trim <- function(
    path, file = "tdict.xlsx", sheet = "data", process = "load") {
  fn <- file.path(path, file)
  df <- readxl::read_xlsx(fn, sheet = sheet, col_types = "text")
  df <- df |>
    dplyr::filter(grepl(pattern = r"(\bload\b)", x = process, ignore.case = TRUE))
  assertthat::assert_that(nrow(df) != 0, msg = "The tdict data is empty.")
  TDict(df)
}




#' Write Data from \code{DDict} to Excel
#'
#' Write data from \code{DDict} to Excel.
#'
#' An error message will be issued if the data from \code{DDict}.
#'
#' @param object Object of class \code{DDict}.
#' @param path Path to excel file.
#' @param file Name of Excel file. Often comes from \code{ddict@data_fn}.
#' @param sheet Name of Excel sheet. Defaults is \emph{data}.
#'
#' @return Full file name of Excel file.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
tdict_write_xl <- function(object, path, file = "ddict.xlsx", sheet = "data") {
  checkmate::assert_class(object, classes = "flrpkgPreproc::TDict")
  fn <- file.path(path, file)
  df <- tdict_table(object)
  writexl::write_xlsx(list("data" = df), path = fn)
  fn
}
