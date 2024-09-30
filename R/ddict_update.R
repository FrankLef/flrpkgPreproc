#' Create an Updated \code{DDict} Excel File
#'
#' Create an updated \code{DDict} excel file.
#'
#' Create an xl file with extracted information about 1 or many dataframes.
#'
#' @param nms Character() with names of data.frames.
#' @param env Environment. Default is \code{.GlobalEnv}, don't change unless
#'   you know what you are doing.
#'
#' @return Object of class \code{DDict}.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
ddict_update <- function(nms, env = .GlobalEnv) {
  checkmate::assert_names(nms)
  ddict <- DDict()
  for (nm in nms) {
    logging::logdebug("%s: Extract data dictionary.", nm)
    df <- get(nm, envir = env)
    ddict <- ddict_extract(ddict, data = df, table_nm = nm)
  }

  # inform user and abort.
  msg_head <- cli::col_yellow("Data dictionary created. Review before proceeding.")
  msg_body <- c(
    "i" = sprintf("Nb of tables: %s", length(nms))
  )
  msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
  rlang::warn(
    message = msg,
    class = "DDictWarning"
  )
  ddict
}
