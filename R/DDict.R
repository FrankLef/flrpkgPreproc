#' Create a data Dictionary
#'
#' Create a data Dictionary.
#'
#' Create data Dictionary with the following properties.
#' \describe{
#'    \item{data}{Dataframe of info on the variables. See details.}
#'    \item{dtypes}{Character vector of data types allowed by \code{DDict}.
#'    Read-only property.}
#' }
#'
#' @section Content of \code{data}:
#'
#' The \code{data} property is a data.frame with:
#' \describe{
#'    \item{table}{Name of the data.frame containing the variable.}
#'    \item{raw_name}{Name of the variable as shown in the original data.frame.}
#'    \item{name}{New name to assign to the variable..}
#'    \item{label}{Label to identify the variable, e.g. used by \pkg{labelled}.}
#'    \item{raw_dtype}{Data type of raw data.}
#'    \item{dtype}{Data type of used data.}
#'    \item{vtype}{Variable type. Discretionary type assigned to the variable.
#'    No validity check performed. e.g. "key" to flag keys in the data.}
#'    \item{desc}{Optional description.}
#'    \item{note}{Optional note.}
#'    }
#'
#' @param data Data.frame of info on the variables.
#'
#' @return Object of class \code{DDict}.
#'
#' @export
#'
#' @examples
#' ddict <- DDict()
#' stopifnot(S7::S7_inherits(ddict, class = DDict))
DDict <- S7::new_class("DDict",
  package = "flpkgrWrap",
  properties = list(
    dtypes = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        c(
          "integer", "numeric", "character", "logical",
          "factor", "Date", "POSIXct", "ymd"
        )
      }
    ),
    data = S7::new_property(
      class = S7::class_data.frame,
      default = data.frame(
        table = character(),
        raw_name = character(),
        name = character(),
        label = character(),
        raw_dtype = character(),
        dtype = character(),
        vtype = character(),
        desc = character(),
        note = character()
      )
    )
  ),
  validator = function(self) {
    vars <- c(
      "table" = "character", "raw_name" = "character",
      "name" = "character", "label" = "character",
      "raw_dtype" = "character", "dtype" = "character",
      "vtype" = "character", "desc" = "character", "note" = "character"
    )
    check <- checkmate::check_data_frame(
      self@data,
      types = vars,
      ncols = length(vars)
    )
    if (is.character(check)) {
      rlang::abort(
        message = check,
        class = "ValueError"
      )
    }
    check <- checkmate::check_names(names(self@data),
      permutation.of = names(vars)
    )
    if (is.character(check)) {
      rlang::abort(
        message = check,
        class = "ValueError"
      )
    }
    check <- checkmate::check_character(
      self@data$table,
      any.missing = FALSE, min.chars = 1
    )
    if (is.character(check)) {
      rlang::abort(
        message = check,
        class = "ValueError"
      )
    }
    check <- checkmate::check_character(
      self@data$raw_name,
      any.missing = FALSE, min.chars = 1
    )
    if (is.character(check)) {
      rlang::abort(
        message = check,
        class = "ValueError"
      )
    }
    check <- checkmate::check_character(
      self@data$name,
      any.missing = FALSE, min.chars = 1
    )
    if (is.character(check)) {
      rlang::abort(
        message = check,
        class = "ValueError"
      )
    }
    pk <- paste(self@data$table, self@data$raw_name, sep = "-")
    # cat("\n", "pk", "\n")
    # print(pk)
    # cat("\n", "any duplicated pk", "\n")
    # print(any(duplicated(pk)))
    check <- sum(duplicated(pk))
    if (check) {
      rlang::abort(
        message = sprintf("self@data has %d duplicate records.", check),
        class = "ValueError"
      )
    }
  }
)

#' Add a Variable to a Data Dictionnary
#'
#' Add a Variable to a data Dictionary.
#'
#' Add information about a variable to a DDictionary. The vector \code{vars}
#' must include the same information, in any order, as the one found  in the
#' \code{data} property of \code{DDict}.
#'
#' @name addDDict
#'
#' @param object Object of class \code{DDict}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{vars}{Named vector of variables to add to the DDictionary.}
#' }
#'
#'
#' @return Object of class \code{DDict}.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
addDDict <- S7::new_generic("addDDict", dispatch_args = "object")


# No need to document methods in S7.
# source: https://cran.r-project.org/web/packages/S7/vignettes/packages.html

S7::method(addDDict, DDict) <- function(object, vars) {
  checkmate::assert_vector(vars, names = "named")
  checkmate::assert_names(names(vars), permutation.of = names(object@data))
  object@data <- rbind(object@data, as.data.frame(t(vars)))
  object
}

#' Remove a Variable from a Data Dictionnary
#'
#' Remove a Variable from a data dictionary
#'
#' Remove all information about a variable from a DDictionary.
#'
#' @name rmDDict
#'
#' @param object Object of class \code{DDict}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{table}{Name of table from which to remove the variable.}
#'    \item{raw_name}{Name of variable to remove.}
#' }
#'
#' @return Object of class \code{DDict}
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
rmDDict <- S7::new_generic("rmDDict", dispatch_args = "object")


# No need to document methods in S7.
# source: https://cran.r-project.org/web/packages/S7/vignettes/packages.html

S7::method(rmDDict, DDict) <- function(object, table, raw_name) {
  checkmate::assert_string(raw_name)
  object@data <- object@data |>
    dplyr::filter(table != {{ table }}, raw_name != {{ raw_name }})
  object
}

#' Extract Information About Data to a Data Dictionary.
#'
#' Extract information about data to a data dictionary.
#'
#' The information will be stored in an object of class \code{DDict}.
#'
#' @name extractDDict
#'
#' @param object Object of class \code{DDict}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{data}{Data.frame from which to extract the variables' details.}
#'    \item{table_nm}{Name of the table.}
#' }
#'
#' @return Object of class \code{DDict}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
extractDDict <- S7::new_generic("DDict", dispatch_args = "object")

S7::method(extractDDict, DDict) <- function(
    object, data, table_nm = deparse1(substitute(data))) {
  checkmate::assert_data_frame(data)
  # cat("\n", "table", "\n")
  # print(the_table)
  # cat("\n", "variables", "\n")
  the_variables <- sapply(X = data, FUN = \(x) class(x)[1])
  # print(the_variables)
  df <- data.frame(
    table = table_nm,
    raw_name = names(the_variables),
    name = names(the_variables),
    label = NA_character_,
    raw_dtype = unname(the_variables),
    dtype = unname(the_variables),
    vtype = NA_character_,
    desc = NA_character_,
    note = NA_character_
  )
  object@data <- rbind(object@data, df)
  object
}

#' Rename Columns Using a \code{DDict}.
#'
#' Rename columns using a \code{DDict}.
#'
#' The information is stored in an object of class \code{DDict}.
#'
#' @name renDDict
#'
#' @param object Object of class \code{DDict}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{data}{Data.frame with variables to rename.}
#'    \item{table_nm}{Name of the table.}
#' }
#'
#' @return \code{data} with renamed columns.
#'
#' @importFrom dplyr filter
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
renDDict <- S7::new_generic("DDict", dispatch_args = "object")

S7::method(renDDict, DDict) <- function(
    object, data, table_nm = deparse1(substitute(data))) {
  checkmate::assert_data_frame(data)

  the_table <- table_nm
  ddict <- object@data |>
    dplyr::filter(table == {{ the_table }})

  pos <- match(ddict$raw_name, names(data))
  # cat("\n", "pos", "\n")
  # print(pos)
  names(data)[pos] <- ddict$name

  data
}

#' Set Labels to Columns Using a \code{DDict}.
#'
#' Set labels to columns using a \code{DDict}.
#'
#' The labels, stored in an object of class \code{DDict}, are used by
#' to set labels with \code{labelled::var_label()}. If the label in
#' \code{DDict} is empty or \code{NA}, an error is returned.
#'
#' @name labelDDict
#'
#' @param object Object of class \code{DDict}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{data}{Data.frame with variables to label.}
#'    \item{table_nm}{Name of the table.}
#'    \item{is_raw_nm}{\code{FALSE} (default) = use the \code{name} from
#' \code{DDict}; \code{TRUE} = use \code{raw_name} from \code{DDict}.}
#' }
#'
#' @return \code{data} with labels added to the columns.
#'
#' @importFrom labelled var_label
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
labelDDict <- S7::new_generic("DDict", dispatch_args = "object")

S7::method(labelDDict, DDict) <- function(
    object, data, table_nm = deparse1(substitute(data)), is_raw_nm = FALSE) {
  checkmate::assert_data_frame(data)

  # cat("\n", "labelDDict: table_nm", "\n")
  # print(table_nm)

  ddict <- object@data |>
    dplyr::filter(table == table_nm)

  # cat("\n", "labelDDict: ddict", "\n")
  # print(ddict)

  if (!is_raw_nm) {
    lbl <- ddict |>
      dplyr::select(name, label)
  } else {
    lbl <- ddict |>
      dplyr::select(raw_name, label) |>
      dplyr::rename(name = raw_name)
  }

  lbl <- lbl |>
    dplyr::filter(nchar(label) >= 1L, !is.na(label))
  # cat("\n", "labelDDict: lbl", "\n")
  # print(lbl)

  if (nrow(lbl) == 0) {
    msg_head <- cli::col_yellow("There are no label to apply.")
    msg_body <- c(
      "i" = sprintf("Table: %s", table_nm),
      "i" = "Verify the label columnin the data dictionary."
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }

  the_labels <- as.list(lbl$label)
  names(the_labels) <- lbl$name
  # cat("\n", "labelDDict: the_labels", "\n")
  # print(the_labels)

  labelled::var_label(data) <- the_labels
  data
}

#' Cast Data Types of Columns Using a \code{DDict}.
#'
#' Cast data types of columns using a \code{DDict}.
#'
#' The columns data type is recast to the data type specified in \code{dtype}
#' when it differ from the data type specified in \code{raw_dtype}. Nothing is
#' done When \code{dtype} is \code{NA} or empty.
#'
#' @name castDDict
#'
#' @param object Object of class \code{DDict}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{data}{Data.frame with variables to label.}
#'    \item{table_nm}{Name of the table.}
#'    \item{is_raw_nm}{\code{FALSE} (default) = use the \code{name} from
#' \code{DDict}; \code{TRUE} = use \code{raw_name} from \code{DDict}.}
#' }
#'
#' @return \code{data} with recasted columns.
#'
#' @importFrom forcats as_factor
#' @importFrom lubridate ymd
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
castDDict <- S7::new_generic("DDict", dispatch_args = "object")

S7::method(castDDict, DDict) <- function(
    object, data, table_nm = deparse1(substitute(data)), is_raw_nm = FALSE) {
  checkmate::assert_data_frame(data)

  the_choices <- object@dtypes

  # cat("\n", "castDDict: table_nm", "\n")
  # print(table_nm)

  ddict <- object@data |>
    dplyr::filter(table == table_nm)

  # cat("\n", "castDDict: ddict", "\n")
  # print(ddict)

  if (!is_raw_nm) {
    the_dtypes <- ddict |>
      dplyr::select(name, raw_dtype, dtype)
  } else {
    the_dtypes <- ddict |>
      dplyr::select(raw_name, raw_dtype, dtype) |>
      dplyr::rename(name = raw_name)
  }

  the_dtypes <- the_dtypes |>
    dplyr::filter(nchar(dtype) >= 1L, !is.na(dtype), raw_dtype != dtype) |>
    dplyr::filter(is.element(dtype, the_choices))

  # cat("\n", "castDDict: the_types", "\n")
  # print(the_dtypes)

  if (nrow(the_dtypes) == 0) {
    msg_head <- cli::col_yellow("There are no data type to cast.")
    msg_body <- c(
      "i" = sprintf("Table: %s", table_nm),
      "i" = "Verify the dtype column in the data dictionary."
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }

  for (nm in the_dtypes$name) {
    x <- data[, nm]
    a_dtype <- the_dtypes$dtype[the_dtypes$name == nm]
    data[, nm] <- cast_data(object, x = x, dtype = a_dtype)
  }

  data
}

#' Cast Data to New Data Type.
#'
#' @param object Object of class \code{DDict}.
#' @param x Vector.
#' @param dtype Name of data type.
#'
#' @return x with new data type.
cast_data <- function(object, x, dtype) {
  if (dtype == "integer") {
    tryCatch(
      expr = {
        x <- checkmate::assert_integerish(x,
          coerce = TRUE,
          tol = sqrt(.Machine$double.eps)
        )
      },
      error = function(e) {
        return(x)
      }
    )
  } else if (dtype == "numeric") {
    x <- as.numeric(x)
  } else if (dtype == "character") {
    x <- as.character(x)
  } else if (dtype == "logical") {
    x <- as.logical(x)
  } else if (dtype == "factor") {
    x <- forcats::as_factor(x)
  } else if (dtype == "Date") {
    x <- as.Date(x)
  } else if (dtype == "POSIXct") {
    x <- as.POSIXct(x)
  } else if (dtype == "ymd") {
    x <- lubridate::ymd(x)
  } else {
    msg_head <- cli::col_yellow("There are no data type to cast.")
    msg_body <- c(
      "i" = sprintf("Data type: %s", dtype),
      "i" = "Verify the dtype criteria in `cast_data()`."
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }
  x
}
