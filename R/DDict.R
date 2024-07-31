#' Create a Data Dictionary
#'
#' Create a data Dictionary.
#'
#' Create data Dictionary with the following properties.
#' \describe{
#'    \item{data}{Dataframe of info on the variables. See details.}
#'    \item{data_path}{Path where files are saved.}
#'    \item{data_base_fn}{Base file name to export data. Default is "ddict_data".}
#'    \item{status_base_fn}{Base file name to export status, Default is "ddict_status".}
#'    \item{data_fn}{File name to export data.
#'    \code{file.path(data_path, data_base_fn, Sys.Date())}}
#'    \item{status_fn}{Base file name to export status, Default is "ddict_status".
#'    \code{file.path(data_path, status_base_fn, Sys.Date())}}
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
#'    \item{name}{New name to assign to the variable.}
#'    \item{label}{Label to identify the variable, e.g. used by \pkg{labelled}.}
#'    \item{raw_dtype}{Data type of raw data.}
#'    \item{dtype}{Data type of used data.}
#'    \item{role}{Text to identify the role of a variable.
#'    No validity check performed. e.g. "key" to flag keys in the data.}
#'    \item{process}{Text to identify transformations, etc., applied to the
#'    variable. No validity check performed. For example, "lg" could be used to
#'    flag a variable for logarithmic transformation.}
#'    \item{rule}{Text to identify a rule applicable to the variable. Typically
#'    used with the \pkg{validate} package. No validity check performed.}
#'    \item{desc}{Description.}
#'    \item{note}{Some notes.}
#'    }
#'
#' @param data Data dictionary.
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
    data_path = S7::new_property(
      class = S7::class_character,
      default = getwd()
    ),
    data_base_fn = S7::new_property(
      class = S7::class_character,
      default = "ddict_data"
    ),
    status_base_fn = S7::new_property(
      class = S7::class_character,
      default = "ddict_status"
    ),
    data_fn = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        fn <- paste0(paste(self@data_base_fn, Sys.Date(), sep = "_"), ".xlsx")
        file.path(self@data_path, fn)
      }
    ),
    status_fn = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        fn <- paste0(paste(self@status_base_fn, Sys.Date(), sep = "_"), ".xlsx")
        file.path(self@data_path, fn)
      }
    ),
    data = S7::new_property(
      class = S7::class_data.frame
    )
  ),
  validator = function(self) {
    vars <- c(
      "table" = "character", "raw_name" = "character",
      "name" = "character", "label" = "character",
      "raw_dtype" = "character", "dtype" = "character",
      "role" = "character", "process" = "character" ,
      "rule" = "character", "desc" = "character", "note" = "character"
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
  },
  constructor = function(data = NULL) {
    if (is.null(data)) {
      data <- data.frame(
        table = character(),
        raw_name = character(),
        name = character(),
        label = character(),
        raw_dtype = character(),
        dtype = character(),
        role = character(),
        process = character(),
        rule = character(),
        desc = character(),
        note = character()
      )
    }
    S7::new_object(
      DDict,
      data = data,
      data_path = getwd(),
      data_base_fn = "ddict_data",
      status_base_fn = "ddict_status"
    )
  }
)


#' Extract Information About Data to a Data Dictionary
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
#'    \item{table_nm}{Name of table. Used when doing loop or when \code{data}
#'    is from a function argument.}
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
  checkmate::assert_string(table_nm, min.chars = 1)
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
    role = NA_character_,
    process = NA_character_,
    rule = NA_character_,
    desc = NA_character_,
    note = NA_character_
  )
  new_data <- rbind(object@data, df)
  DDict(new_data)
}


#' Data About a Table from a \code{DDict}
#'
#' Data about a table from a \code{DDict}.
#'
#' An error message is returned if no data on the table is available.
#'
#' @name tableDDict
#'
#' @param object Object of class \code{DDict}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{table_nm}{Name of the table.}
#' }
#'
#' @return \code{data} from \code{DDict} object.
#'
#' @importFrom dplyr filter
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
tableDDict <- S7::new_generic("DDict", dispatch_args = "object")

S7::method(tableDDict, DDict) <- function(object, table_nm) {
  checkmate::assert_string(table_nm, min.chars = 1)

  data <- dplyr::filter(object@data, table == table_nm)

  if (!nrow(data)) {
    msg_head <- cli::col_red("No records returned from the data dictionary.")
    msg_body <- c(
      "i" = "Verify the table name used to filter the data.",
      "x" = sprintf("Table: %s", table_nm)
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }

  data
}


#' Filter from a \code{DDict}
#'
#' Filter from a \code{DDict}.
#'
#' The records are filtered using regular expressions. If no criteria is
#' provided for **role**, **process** and **rule**, the full data is returned.
#'
#' @name filterDDict
#'
#' @param object Object of class \code{DDict}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{table_nm}{Compulsory name of the table..}
#'    \item{role_rgx}{Regular expression to filter **role**.}
#'    \item{process_rgx}{Regular expression to filter **process**.}
#'    \item{rule_rgx}{Regular expression to filter **rule**.}
#' }
#'
#' @return \code{data} from \code{DDict} object.
#'
#' @importFrom dplyr filter
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
filterDDict <- S7::new_generic("DDict", dispatch_args = "object")

S7::method(filterDDict, DDict) <- function(
    object, table_nm = "", role_rgx = NULL, process_rgx = NULL, rule_rgx = NULL) {
  checkmate::assert_string(table_nm, min.chars = 1)
  checkmate::assert_string(role_rgx, na.ok = TRUE, null.ok = TRUE)
  checkmate::assert_string(process_rgx, na.ok = TRUE, null.ok = TRUE)
  checkmate::assert_string(rule_rgx, na.ok = TRUE, null.ok = TRUE)

  ddict <- tableDDict(object, table_nm = table_nm)

  params <- c("role" = role_rgx, "process" = process_rgx, "rule" = rule_rgx)
  for (nm in names(params)) {
    rgx <- params[nm]
    if (!is.na(rgx)) {
      ddict <- filter(ddict, grepl(pattern = rgx, x = .data[[nm]]))
    } else {
      ddict <- filter(ddict, is.na(.data[[nm]]))
    }
  }


  if (!nrow(ddict)) {
    msg_head <- cli::col_red("No records returned from the data dictionary.")
    msg_body <- c(
      "i" = "Verify the regular expressions used to filter the data.",
      "x" = sprintf("table name: %s", table_nm),
      "x" = sprintf("role regex: %s", role_rgx),
      "x" = sprintf("process regex: %s", process_rgx),
      "x" = sprintf("rule regex: %s", rule_rgx)
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }

  ddict
}


#' Rename Columns Using a \code{DDict}
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
#'    \item{table_nm}{Name of table. Used when doing loop or when \code{data}
#'    is from a function argument.}
#' }
#'
#' @return \code{data} with renamed columns.
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
  checkmate::assert_string(table_nm, min.chars = 1)

  ddict <- tableDDict(object, table_nm = table_nm)

  ddict <- ddict |>
    dplyr::mutate(pos = match(raw_name, names(data))) |>
    dplyr::filter(!is.na(pos))
  # cat("\n", "ddict", "\n")
  # print(ddict)
  if (!nrow(ddict)) {
    msg_head <- cli::col_red("No `raw_name` found in the data names.")
    msg_body <- c(
      "i" = "Maybe the names have already been changed?",
      "x" = sprintf("Table: %s", table_nm)
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }

  names(data)[ddict$pos] <- ddict$name

  data
}

#' Set Labels to Columns Using a \code{DDict}
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
#'    \item{is_raw_nm}{\code{FALSE} (default) = use the \code{name} from
#' \code{DDict}; \code{TRUE} = use \code{raw_name} from \code{DDict}.}
#'    \item{table_nm}{Name of table. Used when doing loop or when \code{data}
#'    is from a function argument.}
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
    object, data, is_raw_nm = FALSE, table_nm = deparse1(substitute(data))) {
  checkmate::assert_data_frame(data)
  checkmate::assert_flag(is_raw_nm)
  checkmate::assert_string(table_nm, min.chars = 1)

  # cat("\n", "labelDDict: table_nm", "\n")
  # print(table_nm)

  ddict <- tableDDict(object, table_nm = table_nm)
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
    dplyr::filter(name %in% names(data))

  if (!nrow(lbl)) {
    msg_head <- cli::col_red("The variables to label where not found.")
    msg_body <- c(
      "i" = sprintf("Table: %s", table_nm),
      "i" = "Verify the raw_name/name columns in the data dictionary."
    )
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError"
    )
  }

  lbl <- lbl |>
    dplyr::filter(nchar(label) >= 1L, !is.na(label))
  # cat("\n", "labelDDict: lbl", "\n")
  # print(lbl)

  if (!nrow(lbl)) {
    msg_head <- cli::col_red("There are no label to apply.")
    msg_body <- c(
      "i" = sprintf("Table: %s", table_nm),
      "i" = "Verify the label column in the data dictionary."
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

#' Cast Data Types of Columns Using a \code{DDict}
#'
#' Cast data types of columns using a \code{DDict}.
#'
#' The columns data type is recast to the data type specified in \code{dtype}
#' when it differ from the data type specified in \code{raw_dtype}. Nothing is
#' done When \code{dtype} is \code{NA} or empty and a warning is issued.
#'
#' @name castDDict
#'
#' @param object Object of class \code{DDict}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{data}{Data.frame with variables to label.}
#'    \item{is_raw_nm}{\code{FALSE} (default) = use the \code{name} from
#' \code{DDict}; \code{TRUE} = use \code{raw_name} from \code{DDict}.}
#'    \item{table_nm}{Name of table. Used when doing loop or when \code{data}
#'    is from a function argument.}
#' }
#'
#' @return \code{data} with new data types..
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
    object, data, is_raw_nm = FALSE, table_nm = deparse1(substitute(data))) {
  checkmate::assert_data_frame(data)
  checkmate::assert_flag(is_raw_nm)
  checkmate::assert_string(table_nm, min.chars = 1)

  the_choices <- object@dtypes

  # cat("\n", "castDDict: table_nm", "\n")
  # print(table_nm)

  ddict <- tableDDict(object, table_nm = table_nm)

  # cat("\n", "castDDict: ddict", "\n")
  # print(ddict)

  if (!is_raw_nm) {
    ddict <- ddict |>
      dplyr::select(name, raw_dtype, dtype)
  } else {
    ddict <- ddict |>
      dplyr::select(raw_name, raw_dtype, dtype) |>
      dplyr::rename(name = raw_name)
  }

  ddict <- ddict |>
    dplyr::filter(raw_dtype != dtype) |>
    dplyr::filter(dtype %in% the_choices)
  # cat("\n", "castDDict: the_types", "\n")
  # print(the_dtypes)

  if (!nrow(ddict)) {
    msg_head <- cli::col_yellow("There is no data type to cast.")
    msg_body <- c(
      "!" = sprintf("Table: %s", table_nm),
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
    data <- cast_data(object, data,
      var = var,
      dtype = a_dtype,
      table_nm = table_nm
    )
  }

  data
}

#' Cast Data to New Data Type
#'
#' Cast data to new data type.
#'
#' This function is used by \code{castDDict()}. When \code{dtype} is invalid,
#' an error message is issued.
#'
#' @param object Object of class \code{DDict}.
#' @param data Data frame.
#' @param var Name of the variable to cast.
#' @param dtype Name of data type.
#' @param table_nm Name of table. Only used by error message.
#'
#' @seealso castDDict
#'
#' @return \code{data} with new data type.
cast_data <- function(object, data, var, dtype, table_nm) {
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
    #       rather than a warning as in castDDict()..
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

#' Analyse the Status of a Table in \code{DDict}
#'
#' Analyse the status of a table in \code{DDict}.
#'
#' A data frame is returned with the following information
#' \describe{
#'    \item{table}{Name of the table.}
#'    \item{variable}{Name of the variable.}
#'    \item{is_ddict}{Flag. \code{TRUE}: the variable is in the data dictionary,
#'    \code{FALSE} if it is not.}
#'    \item{is_data}{Flag. \code{TRUE}: the variable is in the data,
#'    \code{FALSE} if it is not.}
#' }
#'
#' @name statusDDict
#'
#' @param object Object of class \code{DDict}.
#' @param ... Additional arguments used by methods. Such as
#' \describe{
#'    \item{data}{Data.frame with variables to label.}
#'    \item{table_nm}{Name of the table.}
#' }
#'
#' @return Data frame with status information.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
statusDDict <- S7::new_generic("DDict", dispatch_args = "object")

S7::method(statusDDict, DDict) <- function(
    object, data, table_nm = deparse1(substitute(data))) {
  checkmate::assert_data_frame(data, min.cols = 1)
  checkmate::assert_string(table_nm, min.chars = 1)

  ddict <- tableDDict(object, table_nm = table_nm)

  ddict_nms <- ddict$name
  data_nms <- sapply(X = data, FUN = \(x) class(x)[1])
  status_nms <- unique(c(ddict_nms, names(data_nms)))

  status_df <- data.frame(
    table = table_nm,
    variable = status_nms
  ) |>
    dplyr::mutate(
      is_ddict = variable %in% ddict_nms,
      is_data = variable %in% names(data_nms),
      data_dtype = NA_character_
    ) |>
    dplyr::arrange(variable)

  pos <- match(names(data_nms), status_df$variable)
  status_df$data_dtype[pos] <- unname(data_nms)

  status_df
}
