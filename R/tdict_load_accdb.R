#' Extract Many Data Sets from MS Access
#'
#' Extract many data sets from MS Access.
#'
#' The extraction is done using the information from an object of class
#' \code{TDict}.
#'
#' @param object Obkject of class \code{TDict}.
#' @param raw_nm Flag, TRUE = use raw name to name the data.frame. Otherwise
#'   use the regular name. Default is TRUE.
#' @param type String to filter the type in \code{TDict}. Default is \emph{accdb}.
#' @param process String to filter the process in \code{TDict}. Default is \emph{load}.
#' @param env Environement used to create the data.frame. Don't change this
#'   unless you know what you are doing.
#'
#' @seealso [tdict_load_accdb()]
#'
#' @return Count of dataframe extracted.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
tdict_load_accdb_many <- function(
    object, raw_nm = TRUE, type = "accdb", process = "load", env = .GlobalEnv) {
  checkmate::assert_class(object, classes = "flrpkgPreproc::TDict")
  data <- tdict_filter(
    object,
    type_rgx = type, process_rgx = process
  )
  assertthat::assert_that(nrow(data) != 0)

  n <- 0L
  for (i in seq_len(nrow(data))) {
    logging::logdebug("Extracting %s \U2026", data$name[i])
    df <- tdict_load_accdb(
      path = data$path[i],
      file = data$file[i],
      qry = data$table[i]
    )
    if (raw_nm) {
      nm <- data$raw_name[i]
    } else {
      nm <- data$name[i]
    }
    assertthat::assert_that(
      isFALSE(is.null(nm) || nchar(trimws(nm)) == 0 || is.na(nm)),
      msg = "The name is empty."
    )
    assign(x = nm, value = df, envir = env)
    n <- n + 1L
  }
  n
}

#' Extract a Data Set from MS Access
#'
#' Extract a data set from MS Access.
#'
#' Extract data from MS Access. Regardless of whether it is a query or a table.
#'
#' @param path Path where MS Access database is located.
#' @param file Name of MS Access database.
#' @param qry Name of table/query.
#'
#' @return Data.frame.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
tdict_load_accdb <- function(path, file, qry) {
  checkmate::assertCharacter(qry, min.chars = 1, min.len = 1)

  fn <- file.path(path, file)
  checkmate::assertFileExists(fn, access = "r")

  # always close connections when exiting
  on.exit(RODBC::odbcCloseAll())

  tryCatch(
    {
      cnx <- RODBC::odbcConnectAccess2007(fn, uid = "", pwd = "", readOnly = TRUE)
    },
    error = function(cond) {
      msg <- sprintf("Error connecting to %s\n%s", file, cond$message)
      logging::logerror(msg)
      stop(msg)
    },
    warning = function(cond) {
      msg <- sprintf("Warning connecting to %s\n%s", file, cond$message)
      logging::logwarn(msg)
      stop(msg)
    }
  )

  qry <- paste0("SELECT * FROM ", qry, ";")

  tryCatch(
    {
      df <- RODBC::sqlQuery(channel = cnx, query = qry, errors = TRUE)
    },
    error = function(cond) {
      msg <- sprintf("Error extracting query to \"%s\"\n%s", qry, cond$message)
      logging::logerror(msg)
      stop(msg)
    },
    warning = function(cond) {
      msg <- sprintf("Warning extracting query to \"%s\"\n%s", qry, cond$message)
      logging::logwarn(msg)
      stop(msg)
    }
  )
  if (!is.data.frame(df)) {
    msg <- sprintf("No data returned by \n\"%s\"", qry)
    logging::logerror(msg)
    stop(msg)
  }

  df
}
