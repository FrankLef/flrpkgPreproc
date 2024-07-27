#' Print message that begins with "Hello" to console.
#'
#' @param msg Message to print.
#'
#' @return Message, invisibly.
#' @export
#'
#' @examples
#' hello()
hello <- function(msg = "Hello, world!") {
  if (grepl(pattern = "^hello.*", x = msg, ignore.case = TRUE)) {
    message(msg)
  } else {
    msg_head <- cli::col_red("Message must start with \"hello\"")
    msg_body <- c("i" = sprintf("Message: %s", msg))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }
  invisible(msg)
}
