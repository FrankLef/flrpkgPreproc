#' Add the flags from \code{validation} to data
#'
#' Add the Flags from \code{validation} to data.
#'
#' Add the results of an object of class \code{validation} fro \pkg{validate}
#' To the data. The new columns will have a \code{suffix}. One last logical
#' column called \code{paste("is", suffix, sep = "_")} is added with value
#' \code{TRUE} if any of the column with the \code{suffix} are \code{TRUE}.
#'
#'
#' @param data Data.frame.
#' @param validation Object of class \code{validation} from \pkg{validate}.
#' @param suffix String to add as a suffix to the new columns.
#'
#' @return Augmented data.frame with new columns.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
valid_cbind <- function(data, validation, suffix) {
  checkmate::assert_data_frame(data)
  checkmate::assert_class(validation, classes = "validation")
  checkmate::assert_names(suffix)

  # make sure to reverse the logical values
  values <- !validate::values(validation)

  new_var <- paste("is", suffix, sep = "_")

  data |>
    dplyr::bind_cols(values) |>
    dplyr::rowwise() |>
    dplyr::mutate(!!new_var := any(dplyr::c_across(tidyselect::ends_with(suffix)))) |>
    dplyr::ungroup()
}
