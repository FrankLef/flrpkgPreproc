#' Create a Normalz Object
#'
#' Create a Normalz object.
#'
#' Create a \code{Normalz} object used to normalize conditionally with a given
#' amount. A typical application, for example, is to normalize all expense based
#' on the sales per year.
#'
#' @param basis Data.frame of data used to perform the normalization.
#' @param id_vars Names of columns used to join \code{data} and \code{base}.
#' @param base_var Name of the the base amount column is in \code{basis}.
#' @param scale Number used to multiply the resulting data.
#' @param sufx Character(1) for suffix to use in \code{.name} of \code{across}.
#' Default value is "nmz".
#'
#' @return Object of class \code{DDict}.
#'
#' @export
#'
#' @examples
#' basis <- data.frame(
#'   year = c(2020, 2021),
#'   amt = c(1250000, 1500000)
#' )
#' normlz <- Normalz(
#'   basis = basis, id_vars = "year",
#'   base_var = "amt", scale = 0.0001
#' )
#' stopifnot(S7::S7_inherits(normlz, class = Normalz))
Normalz <- S7::new_class("Normalz",
  package = "flpkgrWrap",
  properties = list(
    basis = S7::class_data.frame,
    id_vars = S7::class_character,
    base_var = S7::class_character,
    scale = S7::class_numeric,
    sufx = S7::class_character
  ),
  validator = function(self) {
    check <- checkmate::check_data_frame(
      self@basis,
      min.cols = 2, min.rows = 1
    )
    if (is.character(check)) {
      rlang::abort(
        message = check,
        class = "ValueError"
      )
    }
    check <- checkmate::check_names(c(self@id_vars, self@base_var),
      permutation.of = names(self@basis)
    )
    if (is.character(check)) {
      rlang::abort(
        message = check,
        class = "ValueError"
      )
    }
    check <- checkmate::check_number(self@scale, lower = 1e-9, upper = 1e9)
    if (is.character(check)) {
      rlang::abort(
        message = check,
        class = "ValueError"
      )
    }
  },
  constructor = function(basis, id_vars, base_var = "base_amt",
                         scale = 1, sufx = "nmz") {
    S7::new_object(
      Normalz,
      basis = basis,
      id_vars = id_vars,
      base_var = base_var,
      scale = scale,
      sufx = sufx
    )
  }
)

#' Apply Conditional Normalization
#'
#' Apply conditional Normalization.
#'
#' Normalize \code{data} depending on the \code{id_vars} from \code{Normalz}.
#'
#' @name doNormalz
#'
#' @param object Object of class \code{Normalz}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{data}{Data.frame to normalize.}
#'    \item{vars}{Columns to normalize.}
#'    \item{inverse}{Perform inverse normalization. The suffix \code{sufx} is
#'    not used when \code{inverse=TRUE}}
#'    \item{keep}{Keep the \code{base_var} in the output.}
#' }
#'
#'
#' @return Data.frame.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
doNormalz <- S7::new_generic("doNormalz", dispatch_args = "object")

# No need to document methods in S7.
# source: https://cran.r-project.org/web/packages/S7/vignettes/packages.html

S7::method(doNormalz, Normalz) <- function(object, data, vars,
                                           inverse = FALSE, keep = FALSE) {
  checkmate::assert_data_frame(data, min.cols = 2, min.rows = 1)
  checkmate::assert_names(vars, subset.of = names(data))
  checkmate::assert_names(object@id_vars, subset.of = names(data))
  checkmate::assert_names(object@id_vars, disjunct.from = vars)
  checkmate::assert_names(object@base_var, disjunct.from = names(data))


  the_basis <- object@basis
  the_ids <- object@id_vars
  the_base_var <- object@base_var
  the_scale <- object@scale
  the_sufx <- paste("{.col}", object@sufx, sep = "_")

  # cat("\n", "doNormalz: the_basis", "\n")
  # print(the_basis)
  # cat("\n", "doNormalz: the_ids", "\n")
  # print(the_ids)

  data <- dplyr::left_join(x = data, y = the_basis, by = the_ids)

  if (!inverse) {
    data <- data |>
      dplyr::mutate(dplyr::across(
        .cols = tidyselect::all_of(vars),
        .fns = \(x) x * the_scale / .data[[the_base_var]],
        .names = the_sufx
      ))
  } else {
    data <- data |>
      dplyr::mutate(dplyr::across(
        .cols = tidyselect::all_of(vars),
        .fns = \(x) x * .data[[the_base_var]] / the_scale
      ))
  }

  if (!keep) data <- data |> dplyr::select(-tidyselect::all_of(the_base_var))

  data
}
