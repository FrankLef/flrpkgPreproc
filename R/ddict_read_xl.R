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
