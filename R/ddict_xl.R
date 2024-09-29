#' Create \code{DDict} Object From Data in Excel
#'
#' Create \code{DDict} object from data in Excel.
#'
#' Read an Excel file and load it into an object of class \code{DDict}.
#'
#' @param path Path where the excel is located.
#' @param file Name of the excel file. Default is \emph{ddict.xlsx}.
#' @param sheet Name of Excel sheet. Default is \emph{data}.
#'
#' @return Object of class \code{DDict}.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
ddict_read_xl <- function(path, file = "ddict.xlsx", sheet = "data") {
  fn <- file.path(path, file)
  df <- readxl::read_xlsx(fn, sheet = sheet, col_types = "text")
  DDict(df)
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
ddict_write_xl <- function(object, path, file = "ddict.xlsx", sheet = "data") {
  checkmate::assert_class(object, classes = "flrpkgPreproc::DDict")
  fn <- file.path(path, file)
  df <- ddict_table(object)
  writexl::write_xlsx(list("data" = df), path = fn)
  fn
}

