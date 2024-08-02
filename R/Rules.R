#' Create a \code{Rules} Object
#'
#' Create a \code{Rules} object.
#'
#' Create a \code{Rules} object used to to managed validation rules as they are
#' typically used by the \pkg{validate} package.
#'
#'
#' @return Object of class \code{Rules}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
Rules <- S7::new_class("Rules",
  package = "flrpkgPreproc",
  properties = list(
    specs = S7::new_property(class = S7::class_list),
    rules = S7::new_property(class = S7::class_data.frame)
  ),
  validator = function(self) {
    vars <- c("name" = "character", "label" = "character", "rule" = "character")
    check <- checkmate::check_data_frame(
      self@rules,
      types = vars,
      ncols = length(vars)
    )
    if (is.character(check)) {
      rlang::abort(
        message = check,
        class = "ValueError"
      )
    }
  },
  constructor = function() {
    S7::new_object(
      Rules,
      specs = list(),
      rules = data.frame(
        name = character(),
        label = character(),
        rule = character()
      )
    )
  }
)
