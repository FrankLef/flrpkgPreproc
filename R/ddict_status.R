ddict_status <- S7::new_generic(
  "DDict",
  dispatch_args = "object",
  fun = function(
    object, data, ..., table_nm = deparse1(substitute(data)), do_abort = TRUE) {
    checkmate::assert_data_frame(data, min.rows = 1)
    checkmate::assert_string(table_nm, min.chars = 1, null.ok = FALSE)
    checkmate::assert_flag(do_abort)
    S7::S7_dispatch()
  })

#' Analyse the status of a table in \code{DDict}
#'
#' Analyse the status of a table in \code{DDict}.
#'
#' A data frame is returned with the following information
#' \describe{
#'    \item{table}{Name of the table.}
#'    \item{variable}{Name of the variable.}
#'    \item{is_ddict}{Flag. \code{TRUE}: the variable is in the data dictionary,
#'    \code{FALSE} if it is not.}
#'    \item{is_data}{Flag. \code{TRUE}: the variable is in the data,
#'    \code{FALSE} if it is not.}
#' }
#'
#' @name ddict_status
#'
#' @param object Object of class \code{DDict}.
#' @param data Data.frame with variables to analyse.
#' @param table_nm Name of the table.
#' @param do_abort \code{TRUE}: Throw error message when there
#'   is outstanding status. Default is \code{TRUE}.
#'
#' @return Data frame with status information.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
S7::method(ddict_status, DDict) <- function(
    object, data,
    table_nm = deparse1(substitute(data)),
    do_abort = TRUE) {

  ddict <- ddict_table(object, table_nm = table_nm)

  ddict_nms <- ddict$name
  data_nms <- sapply(X = data, FUN = \(x) class(x)[1])
  status_nms <- unique(c(ddict_nms, names(data_nms)))

  status_df <- data.frame(
    table = table_nm,
    variable = status_nms
  ) |>
    dplyr::mutate(
      is_ddict = variable %in% ddict_nms,
      is_data = variable %in% names(data_nms),
      data_dtype = NA_character_
    ) |>
    dplyr::arrange(variable)

  pos <- match(names(data_nms), status_df$variable)
  status_df$data_dtype[pos] <- unname(data_nms)

  check <- sum(!status_df$is_ddict & status_df$is_data)
  if (check & do_abort) {
    # inform user and abort.
    msg_head <- cli::col_red("Data dictionary should be updated.")
    msg_body <- c(
      "x" = sprintf("Nb of new variables: %d.", check),
      "i" = "To omit this error message, set `do_abort = FALSE`."
    )
    msg_body <- rlang::format_error_bullets(msg_body)
    msg <- paste(msg_head, msg_body, sep = "\n")
    rlang::abort(
      message = msg,
      class = "RuntimeError"
    )
  }

  status_df
}
