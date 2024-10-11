#' Test Data Set to Clean
#'
#' Test data set to clean.
#'
#' An example of data set used to test the cleaning functions.
#'
#' @docType data
#'
#' @format Data frame with 30 rows and 29 columns.
#' \describe{
#'   \item{ndx}{Integer. Index.}
#'   \item{not_uniq}{Integer. One of the number is not unique.}
#'   \item{ran_pct}{Double. Random percentages / probabilities.}
#'   \item{ran_int}{Integer. Random integers.}
#'   \item{ran_color}{Character. Random primary, secondary or tertiary color names.}
#'   \item{ran_dat}{Random dates.}
#'   \item{is_rare}{Logical. Only one \code{TRUE}.}
#'   \item{bool10}{Logical. 10% of \code{TRUE}.}
#'   \item{bool25}{Logical. 25% of \code{TRUE}.}
#'   \item{bool50}{Logical. 50% of \code{TRUE}.}
#'   \item{same_num}{Numerical. Same number everywhere.}
#'   \item{nzv_num}{Numerical. Near-Zero-Variance data.}
#'   \item{int_num}{TBA}
#'   \item{clean_int_num}{TBA}
#'   \item{inth_num}{TBA}
#'   \item{normal_num}{TBA}
#'   \item{abnormal_num}{TBA}
#'   \item{lognormal_num}{TBA}
#'   \item{logabnormal_num}{TBA}
#'   \item{log1ps10}{TBA}
#'   \item{err_int}{TBA}
#'   \item{same_text}{TBA}
#'   \item{pos_text}{TBA}
#'   \item{cont_text}{TBA}
#'   \item{len_cont_text}{TBA}
#'   \item{clean_text}{TBA}
#'   \item{len_clean_text}{TBA}
#'   \item{cont_date}{TBA}
#'   \item{clean_date}{TBA}
#' }
#'
"data2clean"


#' Test Data Dictionary
#'
#' Test data dictionary.
#'
#' An example of data dictionary  based on \code{data2clean}.
#'
#' @docType data
#'
#' @format Data frame with 29 rows and 13 columns.
#' \describe{
#'   \item{table}{Name of the data frame containing the variable.}
#'   \item{raw_name}{Name of the variable as shown in the original data frame.}
#'   \item{name}{New name to assign to the variable.}
#'   \item{label}{Label to use with the variable, e.g. used by \pkg{labelled}.}
#'   \item{raw_dtype}{Data type of raw data.}
#'   \item{dtype}{Data type of used data.}
#'   \item{role}{Text to identify the role of a variable.}
#'   \item{process}{Text to identify processes associated used.}
#'   \item{rule}{Text to identify the rules applicable to the variable}
#'   \item{desc}{Description.}
#'   \item{note}{Note.}
#'   \item{name1}{Alternative \code{name}.}
#'   \item{dtype1}{Alternative \code{dtype}.}
#' }
#'
"data2clean_ddict"
