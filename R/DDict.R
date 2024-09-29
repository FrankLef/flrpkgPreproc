#' Create a Data Dictionary
#'
#' Create a data dictionary.
#'
#' Create data Dictionary with the following properties.
#' \describe{
#'    \item{data}{Dataframe of info on the variables. See details.}
#'    \item{data_path}{Path where the dictionary is saved, set to \code{getwd()}.}
#'    \item{data_base_fn}{Base file name to export data. Default is \emph{ddict_raw}.}
#'    \item{status_base_fn}{Base file name to export status, Default is \emph{ddict_status_raw}.}
#'    \item{ext}{Extension fo as a suffix to file names. Default is \emph{"xlsx"}.}
#'    \item{data_fn}{Full file name to export the \emph{ddict} data.}
#'    \item{status_fn}{Full file name to export the \emph{ddict} status.}
#'    \item{dtypes}{Character vector of data types allowed by \code{DDict}.
#'    Read-only property.}
#' }
#'
#' @section Content of \code{data}:
#'
#' The \code{data} property is a data.frame with:
#' \describe{
#'    \item{table}{Name of the data.frame containing the variable.}
#'    \item{raw_name}{Name of the variable as shown in the original data.frame.}
#'    \item{name}{New name to assign to the variable.}
#'    \item{label}{Label to identify the variable, e.g. used by \pkg{labelled}.}
#'    \item{raw_dtype}{Data type of raw data.}
#'    \item{dtype}{Data type of used data.}
#'    \item{role}{Text to identify the role of a variable.
#'    No validity check performed. e.g. "key" to flag keys in the data.}
#'    \item{process}{Text to identify transformations, etc., applied to the
#'    variable. No validity check performed. For example, "lg" could be used to
#'    flag a variable for logarithmic transformation.}
#'    \item{rule}{Text to identify a rule applicable to the variable. Typically
#'    used with the \pkg{validate} package. No validity check performed.}
#'    \item{desc}{Description.}
#'    \item{note}{Some notes.}
#'    }
#'
#' @param data Data dictionary.
#'
#' @return Object of class \code{DDict}.
#'
#' @export
#'
#' @examples
#' ddict <- DDict()
#' stopifnot(S7::S7_inherits(ddict, class = DDict))
DDict <- S7::new_class("DDict",
  package = "flrpkgPreproc",
  properties = list(
    dtypes = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        c(
          "integer", "numeric", "character", "logical",
          "factor", "Date", "POSIXct", "ymd"
        )
      }
    ),
    data_path = S7::class_character,
    ext = S7::class_character,
    data_base_fn = S7::class_character,
    status_base_fn = S7::class_character,
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
    status_fn = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        fn <- paste0(
          paste(self@status_base_fn, Sys.Date(), sep = "_"),
          ".", self@ext
        )
        file.path(self@data_path, fn)
      }
    ),
    data = S7::class_data.frame
  ),
  validator = function(self) {
    vars <- c(
      "table" = "character", "raw_name" = "character",
      "name" = "character", "label" = "character",
      "raw_dtype" = "character", "dtype" = "character",
      "role" = "character", "process" = "character",
      "rule" = "character", "desc" = "character", "note" = "character"
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
    pk <- paste(self@data$table, self@data$raw_name, sep = "-")
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
        table = character(),
        raw_name = character(),
        name = character(),
        label = character(),
        raw_dtype = character(),
        dtype = character(),
        role = character(),
        process = character(),
        rule = character(),
        desc = character(),
        note = character()
      )
    }
    S7::new_object(
      DDict,
      data = data,
      data_path = getwd(),
      ext = "xlsx",
      data_base_fn = "ddict_raw",
      status_base_fn = "ddict_status_raw"
    )
  }
)


#' Data about a table from a \code{DDict}
#'
#' Data about a table from a \code{DDict}.
#'
#' An error message is returned if no data on the table is available.
#'
#' @name ddict_table
#'
#' @param object Object of class \code{DDict}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{table_nm}{Name of the table.}
#' }
#'
#' @return \code{data} from \code{DDict} object.
#'
#' @importFrom dplyr filter
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
ddict_table <- S7::new_generic("DDict", dispatch_args = "object")

S7::method(ddict_table, DDict) <- function(object, table_nm = NULL) {
  checkmate::assert_string(table_nm, min.chars = 1, null.ok = TRUE)

  if (!is.null(table_nm)) {
    data <- dplyr::filter(object@data, table == table_nm)
  } else {
    data <- object@data
  }

  if (!nrow(data)) {
    msg_head <- cli::col_red("No records returned from the data dictionary.")
    msg_body <- c(
      "i" = "Verify the table name used to filter the data.",
      "x" = sprintf("Table: %s", table_nm)
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    # cat("\n", msg, "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }

  data
}
