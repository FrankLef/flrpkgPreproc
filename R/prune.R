#' Reset the prune ID column
#'
#' Reset the prune ID column.
#'
#' The column will be created if it doesn't exist. The column will be entirely
#' overwritten with \code{default}.
#'
#' @param data Data.frame.
#' @param id_var Name of the column with prune ID. Default is \emph{prune_id}.
#' @param default Default value in the id_var column. Default is \emph{ok}.
#'
#' @return Data.frame with new/reset id_var column.
#' @export
#'
#' @examples
#' df <- data.frame(x = letters[1:3])
#' df <- prune_reset(df)
#' stopifnot(
#'   identical(df, data.frame(x = letters[1:3], prune_id = "ok"))
#' )
#'
prune_reset <- function(data, id_var = "prune_id", default = "ok") {
  data |>
    dplyr::mutate(!!id_var := {{ default }})
}

#' Update the prune ID column.
#'
#' Update the prune ID column.
#'
#' The \code{id_var} column will be udated with the \code{id} value whenever
#' the corresponding element in \code{flags} will be \code{TRUE} \bold{AND}
#' the value in \code{id_var} will be equal to \code{default}.
#'
#' @param data Data.frame.
#' @param flags String, name of the column where logical values are.
#' @param id String value to update to. Must NOT be the same as \code{default}.
#' @param id_var Name of the prune id column. Default is \emph{prune_id}.
#' @param default Default value in the \code{id_var} column. Default is \emph{ok}.
#'
#' @seealso [prune_reset()]
#'
#' @return Data.frame with updated \code{id_var} column.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = letters[1:3],
#'   flags = c(TRUE, FALSE, TRUE),
#'   prune_id = c("ok", "ok", "X")
#' )
#' df <- df |> prune_upd(flags = "flags", id = "Y")
prune_upd <- function(data, flags, id, id_var = "prune_id", default = "ok") {
  checkmate::assert_data_frame(data)
  checkmate::assert_names(flags, subset.of = names(data))
  checkmate::assert_names(id_var, subset.of = names(data))
  checkmate::assert_names(id, disjunct.from = {{ default }})
  data |>
    dplyr::mutate(!!id_var := dplyr::if_else(
      .data[[flags]] & (.data[[id_var]] == {{ default }}),
      {{ id }},
      .data[[id_var]]
    ))
}
