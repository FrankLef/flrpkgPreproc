#' Create a Table Dictionary
#'
#' Create a table dictionary.
#'
#' Create table Dictionary with the following properties.
#' \describe{
#'    \item{data}{Dataframe of info on the variables. See details.}
#'    \item{data_path}{Path where the dictionary is saved, set to \code{getwd()}.}
#'    \item{data_base_fn}{Base file name to export data. Default is \emph{tdict_raw}.}
#'    \item{ext}{Extension fo as a suffix to file names. Default is \emph{"xlsx"}.}
#'    \item{data_fn}{Full file name to export the \emph{tdict} data.}
#' }
#'
#' @section Content of \code{data}:
#'
#' The \code{data} property is a data.frame with:
#' \describe{
#'    \item{path}{Directory of data source.}
#'    \item{file}{File name of data source.}
#'    \item{table}{Name of query/table (database) or spreadsheet (excel).}
#'    \item{name}{Name of table.}
#'    \item{raw_name}{Import name of table. Usually begins with \emph{raw_"}.}
#'    \item{label}{Label for the table.}
#'    \item{type}{Source type of raw data. e.g. \emph{accdb}, \emph{xlsx}, etc.}
#'    \item{role}{String to identify the role of the table.}
#'    \item{process}{Text to identify how to process the table}
#'    \item{rule}{Text to identify a set of rules applicable to the table.}
#'    \item{desc}{Description.}
#'    \item{note}{Some notes.}
#'    }
#'
#' @param data Table dictionary.
#'
#' @return Object of class \code{TDict}.
#'
#' @export
#'
#' @examples
#' tdict <- TDict()
#' stopifnot(S7::S7_inherits(tdict, class = TDict))
TDict <- S7::new_class("TDict",
  package = "flrpkgPreproc",
  properties = list(
    data_path = S7::class_character,
    data_base_fn = S7::class_character,
    ext = S7::class_character,
    data_fn = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        fn <- paste0(
          paste(self@data_base_fn, Sys.Date(), sep = "_"),
          ".", self@ext
        )
        file.path(self@data_path, fn)
      }
    ),
    data = S7::class_data.frame
  ),
  validator = function(self) {
    vars <- c(
      "path" = "character", "file" = "character",
      "table" = "character", "name" = "character",
      "raw_name" = "character", "label" = "character", "type" = "character",
      "role" = "character", "process" = "character", "rule" = "character",
      "desc" = "character", "note" = "character"
    )
    check <- checkmate::check_data_frame(
      self@data,
      types = vars,
      ncols = length(vars)
    )
    if (is.character(check)) {
      rlang::abort(
        message = check,
        class = "ValueError"
      )
    }
    check <- checkmate::check_names(names(self@data),
      permutation.of = names(vars)
    )
    if (is.character(check)) {
      rlang::abort(
        message = check,
        class = "ValueError"
      )
    }
    check <- checkmate::check_character(
      self@data$table,
      any.missing = FALSE, min.chars = 1
    )
    if (is.character(check)) {
      rlang::abort(
        message = check,
        class = "ValueError"
      )
    }
    check <- checkmate::check_character(
      self@data$raw_name,
      any.missing = FALSE, min.chars = 1
    )
    if (is.character(check)) {
      rlang::abort(
        message = check,
        class = "ValueError"
      )
    }
    check <- checkmate::check_character(
      self@data$name,
      any.missing = FALSE, min.chars = 1
    )
    if (is.character(check)) {
      rlang::abort(
        message = check,
        class = "ValueError"
      )
    }
    pk <- self@data$name
    # cat("\n", "pk", "\n")
    # print(pk)
    # cat("\n", "any duplicated pk", "\n")
    # print(any(duplicated(pk)))
    check <- sum(duplicated(pk))
    if (check) {
      rlang::abort(
        message = sprintf("self@data has %d duplicate records.", check),
        class = "ValueError"
      )
    }
  },
  constructor = function(data = NULL) {
    if (is.null(data)) {
      data <- data.frame(
        path = character(),
        file = character(),
        table = character(),
        name = character(),
        raw_name = character(),
        label = character(),
        type = character(),
        role = character(),
        process = character(),
        rule = character(),
        desc = character(),
        note = character()
      )
    }
    S7::new_object(
      TDict,
      data = data,
      data_path = getwd(),
      data_base_fn = "tdict_raw",
      ext = "xlsx"
    )
  }
)


tdict_table <- S7::new_generic("TDict", dispatch_args = "object")


#' Data From a \code{TDict}
#'
#' Data From a \code{TDict}.
#'
#' An error message is returned if no data is available.
#'
#' @name tdict_table
#'
#' @param object Object of class \code{TDict}.
#'
#' @return \code{data} from \code{TDict} object.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
S7::method(tdict_table, TDict) <- function(object) {
  data <- object@data

  if (!nrow(data)) {
    msg_head <- cli::col_red("The table dictionary is empty.")
    msg_body <- c(
      "x" = "There is nothing in the data dictionary."
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }

  data
}
