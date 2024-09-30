ddict_cast <- S7::new_generic(
  "DDict",
  dispatch_args = "object",
  fun = function(
      object, data, ..., is_raw_nm = FALSE, table_nm = deparse1(substitute(data))) {
    checkmate::assert_data_frame(data)
    checkmate::assert_flag(is_raw_nm)
    checkmate::assert_string(table_nm, min.chars = 1)
    S7::S7_dispatch()
  }
)


#' Cast data types of columns using a \code{DDict}
#'
#' Cast data types of columns using a \code{DDict}.
#'
#' The columns data type is recast to the data type specified in \code{dtype}
#' when it differ from the data type specified in \code{raw_dtype}. Nothing is
#' done When \code{dtype} is \code{NA} or empty and a warning is issued.
#'
#' @name ddict_cast
#'
#' @param object Object of class \code{DDict}.
#' @param data Data.frame with variables to cast to a data type.
#' @param is_raw_nm \code{FALSE} = use the \code{name} from
#'   \code{DDict}; \code{TRUE} = use \code{raw_name} from \code{DDict}. Default
#'  is \code{FALSE}.
#' @param table_nm Name of table. Used when doing loop or when \code{data}
#'  is from a function argument.
#'
#' @seealso [cast_data()]
#'
#' @return \code{data} with new data types.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
S7::method(ddict_cast, DDict) <- function(
    object, data, is_raw_nm = FALSE, table_nm = deparse1(substitute(data))) {
  the_choices <- object@dtypes

  # cat("\n", "ddict_cast: table_nm", "\n")
  # print(table_nm)

  ddict <- ddict_table(object, table_nm = table_nm)

  # cat("\n", "ddict_cast: ddict", "\n")
  # print(ddict)

  if (!is_raw_nm) {
    ddict <- ddict |>
      dplyr::select(name, role, raw_dtype, dtype)
  } else {
    ddict <- ddict |>
      dplyr::select(raw_name, role, raw_dtype, dtype) |>
      dplyr::rename(name = raw_name)
  }

  ddict <- ddict |>
    dplyr::filter(raw_dtype != dtype) |>
    dplyr::filter(dtype %in% the_choices)
  # cat("\n", "ddict_cast: the_types", "\n")
  # print(the_dtypes)

  check <- ddict |>
    filter(is.na(role)) |>
    nrow()
  if (check) {
    msg_head <- cli::col_red("variables must have a role.")
    msg_body <- c(
      "i" = sprintf("Table: %s", table_nm),
      "x" = sprintf("Nb of invalid variables: %s", check),
      "i" = "Verify the role of variables where raw_dtype \u2260 dtype."
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }


  if (!nrow(ddict)) {
    msg_head <- cli::col_yellow("There is no data type to cast.")
    msg_body <- c(
      "i" = sprintf("Table: %s", table_nm),
      "i" = "Verify the dtype column in the data dictionary.",
      "i" = "Input data is returned as is."
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::warn(
      message = msg,
      class = "ValueWarning"
    )
    return(data)
  }

  for (var in ddict$name) {
    a_dtype <- ddict$dtype[ddict$name == var]
    data <- cast_data(
      data,
      var = var,
      dtype = a_dtype,
      table_nm = table_nm
    )
  }

  data
}


#' Cast data to new data Type
#'
#' Cast data to new data type.
#'
#' This function is used by \code{ddict_cast()}. When \code{dtype} is invalid,
#' an error message is issued.
#'
#' @param data Data frame.
#' @param var Name of the variable to cast.
#' @param dtype Name of data type.
#' @param table_nm Name of table. Only used by error message.
#'
#' @seealso [ddict_cast()]
#'
#' @return \code{data} with new data type.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
cast_data <- function(data, var, dtype, table_nm) {
  checkmate::assert_data_frame(data)
  checkmate::assert_names(var)
  checkmate::assert_names(dtype)
  checkmate::assert_names(table_nm)

  out <- data
  if (dtype == "integer") {
    tryCatch(
      expr = {
        out <- dplyr::mutate(
          data,
          !!var := checkmate::assert_integerish(
            .data[[var]],
            coerce = TRUE, tol = sqrt(.Machine$double.eps)
          )
        )
      },
      error = function(e) {
        return(data)
      }
    )
  } else if (dtype == "numeric") {
    out <- dplyr::mutate(data, !!var := as.numeric(.data[[var]]))
    # y <- as.numeric(x)
  } else if (dtype == "character") {
    out <- dplyr::mutate(data, !!var := as.character(.data[[var]]))
    # y <- as.character(x)
  } else if (dtype == "logical") {
    out <- dplyr::mutate(data, !!var := as.logical(.data[[var]]))
    # y <- as.logical(x)
  } else if (dtype == "factor") {
    out <- dplyr::mutate(data, !!var := forcats::as_factor(.data[[var]]))
    # y <- forcats::as_factor(x)
  } else if (dtype == "Date") {
    out <- dplyr::mutate(data, !!var := as.Date(.data[[var]]))
    # y <- as.Date(x)
  } else if (dtype == "POSIXct") {
    out <- dplyr::mutate(data, !!var := as.POSIXct(.data[[var]]))
    # y <- as.POSIXct(x)
  } else if (dtype == "ymd") {
    out <- dplyr::mutate(data, !!var := lubridate::ymd(.data[[var]]))
    # y <- lubridate::ymd(x)
  } else {
    # NOTE: At this point this should not happen which is why it is an error
    #       rather than a warning as in ddict_cast()..
    msg_head <- cli::col_red("The data type is invalid.")
    msg_body <- c(
      "x" = sprintf("Table: %s", table_nm),
      "x" = sprintf("Column: %s", var),
      "x" = sprintf("Data type: %s", dtype),
      "i" = "See DDict@dtypes for allowed data types."
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }

  if (!identical(dim(data), dim(out))) {
    msg_head <- cli::col_red("The data type change failed.")
    msg_body <- c(
      "x" = sprintf("Table: %s", table_nm),
      "x" = sprintf("Column: %s", var),
      "x" = sprintf("Data type: %s", dtype),
      "i" = "See DDict@dtypes for allowed data types."
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }

  out
}
