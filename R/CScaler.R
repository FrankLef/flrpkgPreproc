#' Create a \code{CScaler} object to conditionally scale
#'
#' Create a \code{CScaler} object to conditionally scale.
#'
#' Create a \code{CScaler} object used to conditionally rescale to a maximum
#' amount conditional on some criteria. A typical application, for example, is
#' to scale all expenses based on the sales per year.
#'
#' @param basis Data.frame of data used to perform the scale.
#' @param id_vars Names of columns used to join \code{data} and \code{base}.
#' @param base_var Name of the the base amount column is in \code{basis}.
#' @param scale Number used to multiply the resulting data.
#' @param suffix Character(1) for suffix to use in \code{.name} of \code{across}.
#' Default value is "scl". If no suffix is desired, set it as an empty string
#' \code{suffix = ""}.
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
#' cscaler <- CScaler(
#'   basis = basis, id_vars = "year",
#'   base_var = "amt", scale = 1000
#' )
#' stopifnot(S7::S7_inherits(cscaler, class = CScaler))
CScaler <- S7::new_class("CScaler",
  package = "flrpkgPreproc",
  properties = list(
    basis = S7::class_data.frame,
    id_vars = S7::class_character,
    base_var = S7::class_character,
    scale = S7::class_numeric,
    suffix = S7::class_character
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
                         scale = 1, suffix = "scl") {
    S7::new_object(
      CScaler,
      basis = basis,
      id_vars = id_vars,
      base_var = base_var,
      scale = scale,
      suffix = suffix
    )
  }
)

#' Apply conditional scales
#'
#' Apply conditional scales.
#'
#' Scale \code{data} depending on the \code{id_vars} from \code{CScaler}.
#'
#' @name CScaler_do
#'
#' @param object Object of class \code{CScaler}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{data}{Data.frame to scale.}
#'    \item{vars}{Columns to scale}
#'    \item{inverse}{Perform inverse scale The suffix \code{suffix} is
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
CScaler_do <- S7::new_generic("CScaler_do", dispatch_args = "object")

S7::method(CScaler_do, CScaler) <- function(object, data, vars,
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
  the_suffix <- object@suffix
  if (nchar(the_suffix)) {
    the_suffix <- paste("{.col}", the_suffix, sep = "_")
  } else {
    the_suffix <- NULL
  }

  # cat("\n", "CScaler_do: the_basis", "\n")
  # print(the_basis)
  # cat("\n", "CScaler_do: the_ids", "\n")
  # print(the_ids)

  data <- dplyr::left_join(x = data, y = the_basis, by = the_ids)

  if (!inverse) {
    data <- data |>
      dplyr::mutate(dplyr::across(
        .cols = tidyselect::all_of(vars),
        .fns = \(x) x * the_scale / .data[[the_base_var]],
        .names = the_suffix
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
