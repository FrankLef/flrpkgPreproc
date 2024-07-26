#' Create a data Dictionary
#'
#' Create a data Dictionary.
#'
#' Create data Dictionary with the following properties.
#' \describe{
#'    \item{data}{Dataframe of info on the variables. See details.}
#' }
#'
#' @section Content of \code{data}:
#'
#' The \code{data} property is a data.frame with:
#' \describe{
#'    \item{table}{Name of the data.frame containing the variable.}
#'    \item{raw_name}{Name of the variable as shown in the original data.frame.}
#'    \item{name}{New name to assign to the variable..}
#'    \item{label}{Label to identify the variable, e.g. used by \pkg{sjlabelled}.}
#'    \item{desc}{Description of the variable.}
#'    \item{note}{Discretionary note on the variable.}
#'    \item{raw_dtype}{Data type of raw data.}
#'    \item{dtype}{Data type of used data.}
#'    }
#'
#' @param data Data.frame of info on the variables.
#'
#' @return Object of class DDict.
#'
#' @importFrom checkmate check_data_frame check_names
#'
#' @export
#'
#' @examples
#' DDict <- DDict()
DDict <- S7::new_class("DDict",
  package = "flpkgrWrap",
  properties = list(
    data = S7::new_property(
      class = S7::class_data.frame,
      default = data.frame(
        table = character(),
        raw_name = character(),
        name = character(),
        label = character(),
        desc = character(),
        note = character(),
        raw_dtype = character(),
        dtype = character()
      )
    )
  ),
  validator = function(self) {
    nms <- c(
      "table", "raw_name", "name", "label", "desc", "note",
      "raw_dtype", "dtype"
    )
    check <- checkmate::check_data_frame(
      self@data,
      types = rep("character", times = 8L),
      ncols = length(nms))
    if (is.character(check)) {
      rlang::abort(
        message = check,
        class = "ValueError"
      )
    }
    check <- checkmate::check_names(names(self@data), permutation.of = nms)
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
#' @importFrom sjlabelled var_labels
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
    desc = NA_character_,
    note = NA_character_,
    raw_dtype = unname(the_variables),
    dtype = unname(the_variables)
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

#' Add Labels to Columns Using a \code{DDict}.
#'
#' Add labels to columns using a \code{DDict}.
#'
#' The labels, stored in an object of class \code{DDict}, are used by
#' \pkg{sjlabelled} to add labels. If the label is empty or \code{NA}, no label
#' is assigned.
#'
#' @name labelDDict
#'
#' @param object Object of class \code{DDict}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{data}{Data.frame with variables to label.}
#'    \item{table_nm}{Name of the table.}
#' }
#'
#' @return \code{data} with labels added to the columns.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
labelDDict <- S7::new_generic("DDict", dispatch_args = "object")

S7::method(labelDDict, DDict) <- function(
    object, data, table_nm = deparse1(substitute(data))) {
  checkmate::assert_data_frame(data)

  message("TODO")

  the_table <- table_nm
  # cat("\n", "table", "\n")
  # print(the_table)
  # cat("\n", "variables", "\n")
  object
}
