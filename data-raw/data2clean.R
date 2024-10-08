get_preproc <- function(nm) {
  checkmate::assert_names(nm)
  fn <- system.file(
    "extdata", "preproc.xlsx",
    package = "flrpkgPreproc", mustWork = TRUE
  )
  df <- readxl::read_xlsx(
    path = fn,
    sheet = "data")
  ddict <- readxl::read_xlsx(
    path = fn,
    sheet = "ddict")
  ddict <- ddict |>
    filter(table == "data1")
  df <- get_preproc_cast(df, ddict)
  out <- list(
    "df" = df,
    "ddict" = ddict
  )
  out[[nm]]
}

get_preproc_cast <- function(data, ddict) {
  checkmate::assert_names(names(data),identical.to = ddict$raw_name)
  n <- 0L
  for (nm in names(data)) {
    x <- data[, nm, drop = TRUE]
    dtype <- class(x)[1]
    raw_dtype <- ddict$raw_dtype[ddict$raw_name == nm]
    if (dtype != raw_dtype) {
      data[, nm] <- switch(
        raw_dtype,
        "integer" = as.integer(x),
        "numeric" = as.numeric(x),
        "logical" = as.logical(x),
        "Date" = as.Date(x),
        "POSIXct" = as.POSIXct(x),
        "character" = as.character(x),
        stop(sprintf("\"%s\" is an invalid raw data type.", raw_dtype))
      )
      # msg <- sprintf("\n\"%s\" was modified.\n", nm)
      # message(msg)
      n <- n + 1L
    }
  }
  msg <- sprintf("\n%d data types were modified to fit ddict.\n", n)
  message(msg)
  data
}

# data2clean <- get_preproc("df")
# data2clean_ddict <- get_preproc("ddict")
# usethis::use_data(data2clean, overwrite = TRUE)
# usethis::use_data(data2clean_ddict, overwrite = TRUE)
