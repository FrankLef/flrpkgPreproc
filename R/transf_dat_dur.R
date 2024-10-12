#' Compute Duration in Days for Several Pairs of Columns.
#'
#' Compute duration in days for several pairs of columns.
#'
#' Compute the duration in the new column \emph{dur} using start date
#' from \emph{start} and end date using \emph{end}.
#'
#' @param data Data frame to process.
#' @param vars Data frame of variables with the following columns
#' \describe{
#'   \item{dur}{Name of the new column with the duration.}
#'   \item{start}{Name of the column with start dates.}
#'   \item{end}{Name of the column with end dates.}
#' }
#' @param table_nm Table name. Used for error message.
#'
#' @return Data frame with new duration columns in days.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
transf_dat_dur_many <- function(
    data, vars, table_nm = deparse1(substitute(data))) {
  checkmate::assert_data_frame(data)
  checkmate::assert_data_frame(vars, ncols = 3, min.rows = 1)
  checkmate::assert_names(table_nm)
  for (i in seq_len(nrow(vars))) {
    dur_var <- vars$dur[i]
    start_var <- vars$start[i]
    end_var <- vars$end[i]

    do_it <- (start_var %in% names(data)) & (end_var %in% names(data))

    if (do_it) {
      data <- transf_dat_dur(data,
                             dur_var = dur_var,
                             start_var = start_var,
                             end_var = end_var
      )
    } else {
      msg_head <- cli::col_green("Duration calculation is skipped.")
      msg_body <- c(
        "i" = sprintf("Data: %s", table_nm),
        "i" = sprintf("Column not found in data: %s", dur_var)
      )
      msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
      rlang::inform(
        message = msg,
        class = "ValueInform"
      )
    }
  }
  data
}


#' Compute Duration in Days for a Pairs of Columns.
#'
#' Compute duration in days for a pairs of columns.
#'
#' Compute the duration in the new column \code{dur} using start date
#' from \code{start} and end date using \code{end}. This function is used by
#' \code{transf_dat_dur_many}.
#'
#' @param data Data frame to process.
#' @param dur_var String. Name of duration variable.
#' @param start_var String. Name of start variable.
#' @param end_var String. Name of end variable.
#' @param digits Integer for digits to round the days.
#'
#' @seealso [transf_dat_dur_many()]
#'
#' @return Data frame with new column of duration.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
transf_dat_dur <- function(data, dur_var, start_var, end_var, digits = 0) {
  checkmate::assert_data_frame(data)
  checkmate::assert_names(dur_var)
  checkmate::assert_names(start_var)
  checkmate::assert_names(end_var)
  data |>
    dplyr::mutate(
      !!dur_var := lubridate::interval(
        start = .data[[start_var]], end = .data[[end_var]]
      ),
      !!dur_var := .data[[dur_var]] / lubridate::ddays(1),
      !!dur_var := round(.data[[dur_var]], digits = digits)
    )
}
