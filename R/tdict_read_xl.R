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
