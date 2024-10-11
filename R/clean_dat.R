#' Replace out-of-bound date with \code{NA}
#'
#' Replace out-of-bound date with \code{NA}.
#'
#' Go through every column in \code{data} that are of type \code{POSIXct} only.
#' The interval between \code{start} and \code{end} is tested as
#' \code{(x >= start) & (x < end)}.
#'
#' @param data Data frame.
#' @param start String with start date. Converted to \code{POSIXct} internally.
#' @param end String with end date. Converted to \code{POSIXct} internally.
#'
#' @return Data frame with modified \code{POSIXct} columns.
#' @export
#'
#' @examples
#' n <- 9L
#' df <- data.frame(
#'   ndx = seq_len(n),
#'   int = rpois(n, lambda = 5),
#'   dat = sample(
#'     c(as.POSIXct(Sys.Date()), as.POSIXct("1900-01-01")),
#'     size = n, replace = TRUE
#'   )
#' )
#' out <- clean_dat_oob(df, start = "2000-01-01", end = "2050-01-01")
#' out
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
