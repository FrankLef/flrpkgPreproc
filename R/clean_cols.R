#' Find Variables With a constant Value
#'
#' Find Variables with a constant value.
#'
#' A named integer() will be returned with
#' the names and positions of the columns having a constant value.
#'
#' @param data Data frame.
#'
#' @return Integer() with positions and names.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
clean_cols_constant <- function(data) {
  nms <- sapply(data, FUN = \(x) {
    (sum(!duplicated(x)) == 1L)
  })
  nms <- nms[nms] # remove FALSE items
  pos <- match(names(nms), names(data)) # get position
  names(pos) <- names(nms) # add names to position vector
  pos
}
