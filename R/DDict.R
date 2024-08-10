#' Create a data dictionary
#'
#' Create a data Dictionary.
#'
#' Create data Dictionary with the following properties.
#' \describe{
#'    \item{data}{Dataframe of info on the variables. See details.}
#'    \item{data_path}{Path where files are saved and set as \code{getwd()}.}
#'    \item{data_base_fn}{Base file name to export data. Default is \emph{ddict_raw}.}
#'    \item{status_base_fn}{Base file name to export status, Default is \emph{ddict_status_raw}.}
#'    \item{data_fn}{Full file name to export the ddict data.}
#'    \item{status_fn}{Full file name to export the ddict status.}
#'    \item{ext}{Extension fo as a suffix to file names. Default is \emph{"xlsx"}.}
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
    data_path = S7::new_property(
      class = S7::class_character,
      default = getwd()
    ),
    ext = S7::new_property(
      class = S7::class_character,
      default = "xlsx"
    ),
    data_base_fn = S7::new_property(
      class = S7::class_character,
      default = "ddict_raw"
    ),
    status_base_fn = S7::new_property(
      class = S7::class_character,
      default = "ddict_status_raw"
    ),
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
    data = S7::new_property(
      class = S7::class_data.frame
    )
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
      data_base_fn = "ddict",
      status_base_fn = "ddict_status"
    )
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
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{data}{Data.frame from which to extract the variables' details.}
#'    \item{table_nm}{Name of table. Used when doing loop or when \code{data}
#'    is from a function argument.}
#' }
#'
#' @return Object of class \code{DDict}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
ddict_extract <- S7::new_generic("DDict", dispatch_args = "object")

S7::method(ddict_extract, DDict) <- function(
    object, data, table_nm = deparse1(substitute(data))) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(table_nm, min.chars = 1)
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

S7::method(ddict_table, DDict) <- function(object, table_nm) {
  checkmate::assert_string(table_nm, min.chars = 1)

  data <- dplyr::filter(object@data, table == table_nm)

  if (!nrow(data)) {
    msg_head <- cli::col_red("No records returned from the data dictionary.")
    msg_body <- c(
      "i" = "Verify the table name used to filter the data.",
      "x" = sprintf("Table: %s", table_nm)
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }

  data
}


#' Filter from a \code{DDict}
#'
#' Filter from a \code{DDict}.
#'
#' The records are filtered using regular expressions. If no criteria is
#' provided for **role**, **process** and **rule**, the full data is returned.
#'
#' @name ddict_filter
#'
#' @param object Object of class \code{DDict}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{table_nm}{Compulsory name of the table..}
#'    \item{role_rgx}{Regular expression to filter **role**.}
#'    \item{process_rgx}{Regular expression to filter **process**.}
#'    \item{rule_rgx}{Regular expression to filter **rule**.}
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
ddict_filter <- S7::new_generic("DDict", dispatch_args = "object")

S7::method(ddict_filter, DDict) <- function(
    object, table_nm = "", role_rgx = NULL, process_rgx = NULL, rule_rgx = NULL) {
  checkmate::assert_string(table_nm, min.chars = 1)
  checkmate::assert_string(role_rgx, na.ok = TRUE, null.ok = TRUE)
  checkmate::assert_string(process_rgx, na.ok = TRUE, null.ok = TRUE)
  checkmate::assert_string(rule_rgx, na.ok = TRUE, null.ok = TRUE)

  ddict <- ddict_table(object, table_nm = table_nm)

  params <- c("role" = role_rgx, "process" = process_rgx, "rule" = rule_rgx)
  for (nm in names(params)) {
    rgx <- params[nm]
    if (!is.na(rgx)) {
      ddict <- filter(ddict, grepl(pattern = rgx, x = .data[[nm]]))
    } else {
      ddict <- filter(ddict, is.na(.data[[nm]]))
    }
  }


  if (!nrow(ddict)) {
    msg_head <- cli::col_red("No records returned from the data dictionary.")
    msg_body <- c(
      "i" = "Verify the regular expressions used to filter the data.",
      "x" = sprintf("table name: %s", table_nm),
      "x" = sprintf("role regex: %s", role_rgx),
      "x" = sprintf("process regex: %s", process_rgx),
      "x" = sprintf("rule regex: %s", rule_rgx)
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }

  ddict
}
