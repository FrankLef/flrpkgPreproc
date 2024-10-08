#' Replace out-of-bound date with \code{NA}
#'
#' Replace out-of-bound date with \code{NA}.
#'
#' Go through every column in \code{data} that are of type \code{POSIXct} only.
#' The interval between \code{start} and \code{end} is tested as
#' \code{(x >= start) & (x < end)}.
#'
#' @param data Data frame.
#' @param start String with start date.
#' @param end String with end date.
#'
#' @return Data frame with modified \code{POSIXct} columns.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
clean_dat_oob <- function(data, start = "2000-01-01", end = "2050-01-01") {
  checkmate::assert_data_frame(data)
  start <- as.POSIXct(start)
  checkmate::assert_posixct(start)
  end <- as.POSIXct(end)
  checkmate::assert_posixct(end)
  data |> purrr::modify_if(
    .p = lubridate::is.POSIXct,
    .f = \(x) {
      sel <- (x >= start) & (x < end)
      x[!sel] <- lubridate::NA_POSIXct_
      x
    }
  )
}
