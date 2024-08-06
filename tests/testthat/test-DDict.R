test_that("DDict: data property", {
  ddict1_df <- df_ddict(nm = "ddict1")
  ddict <- DDict(ddict1_df)

  expect_identical(ddict@data, ddict1_df)
})

test_that("DDict: dtypes property", {
  # testthat::skip("debug")
  ddict1_df <- df_ddict(nm = "ddict1")
  ddict <- DDict(ddict1_df)
  # cat("\n", "ddict@dtypes", "\n")
  # print(ddict@dtypes)

  dtypes <- c(
    "integer", "numeric", "character", "logical",
    "factor", "Date", "POSIXct", "ymd"
  )
  expect_identical(ddict@dtypes, dtypes)
})


test_that("DDict: File name properties", {
  ddict <- DDict()
  # print(ddict@data_path)
  # print(ddict@data_base_fn)
  # print(ddict@status_base_fn)

  # data file name
  fn <- paste0(paste("ddict_data", Sys.Date(), sep = "_"), ".xlsx")
  target <- file.path(getwd(), fn)
  expect_identical(ddict@data_fn, target)

  # status file name
  fn <- paste0(paste("ddict_status", Sys.Date(), sep = "_"), ".xlsx")
  target <- file.path(getwd(), fn)
  expect_identical(ddict@status_fn, target)
})


test_that("DDict: Validate DDict@data", {
  # testthat::skip("debug")

  # column missing
  err1 <- df_ddict_err(nm = "ddict1")
  expect_error(
    DDict(err1),
    class = "ValueError",
    regexp = "Must have exactly.+cols"
  )

  # invalid column
  err2 <- df_ddict_err(nm = "ddict2")
  expect_error(
    DDict(err2),
    class = "ValueError",
    regexp = "Names must be a permutation of set"
  )
  # invalid raw_name
  err3 <- df_ddict_err(nm = "ddict3")
  expect_error(
    DDict(err3),
    class = "ValueError",
    regexp = "All elements must have at least 1 characters"
  )
  # invalid name
  err4 <- df_ddict_err(nm = "ddict4")
  expect_error(
    DDict(err4),
    class = "ValueError",
    regexp = "Contains missing values"
  )


  # duplicate records
  ddict1 <- df_ddict(nm = "ddict1")
  ddict2 <- rbind(ddict1, ddict1)
  expect_error(
    DDict(ddict2),
    class = "ValueError",
    regexp = "has.+duplicate records"
  )
})


test_that("ddict_filter: Filter data dict", {
  ddict_df <- df_ddict(nm = "ddict2")
  ddict <- DDict(ddict_df)

  # no selection returns the full table.
  out <- ddict_filter(ddict, table_nm = "df2")
  # cat("\n", "out", "\n")
  # print(out)
  expect_identical(dim(out), dim(ddict_df))

  out <- ddict_filter(ddict, table_nm = "df2", role_rgx = r"(\brole1\b)")
  # cat("\n", "out", "\n")
  # print(out)
  expect_identical(dim(out), c(2L, length(ddict_df)))

  out <- ddict_filter(ddict, table_nm = "df2", role_rgx = NA_character_)
  # cat("\n", "out", "\n")
  # print(out)
  expect_identical(dim(out), c(3L, length(ddict_df)))

  out <- ddict_filter(ddict,
    table_nm = "df2",
    role_rgx = r"(\brole1\b)", process_rgx = r"(\proc1\b)"
  )
  # cat("\n", "out", "\n")
  # print(out)
  expect_identical(dim(out), c(1L, length(ddict_df)))
})

test_that("ddict_filter: ERROR", {
  ddict_df <- df_ddict(nm = "ddict3")
  ddict <- DDict(ddict_df)

  expect_error(ddict_filter(ddict, table_nm = "ERROR", role_rgx = r"(\brole1\b)"),
    class = "ValueError",
    regexp = "No records returned from the data dictionary"
  )

  expect_error(ddict_filter(ddict, table_nm = "df3", role_rgx = "ERROR"),
    class = "ValueError",
    regexp = "No records returned from the data dictionary"
  )
})

test_that("ddict_extract: Extract data in DDict", {
  # testthat::skip("debug")
  ddict <- DDict()
  df1 <- df_ddict(nm = "df1")

  ddict1_df <- df_ddict(nm = "ddict1")

  target <- DDict(ddict1_df)
  # target@data <- ddict1
  # cat("\n", "target", "\n")
  # print(target@data)

  out <- ddict_extract(ddict, df1)
  # cat("\n", "out", "\n")
  # print(out@data)

  expect_identical(out, target)
})

test_that("ddict_extract: ERROR extract data in DDict", {
  # testthat::skip("debug")
  ddict <- DDict()

  df1 <- df_ddict(nm = "df1")

  out <- ddict_extract(ddict, df1)

  expect_error(
    ddict_extract(out, df1),
    class = "ValueError",
    regexp = ".*has.+duplicate records.*"
  )
})

test_that("ddict_table: Table's data from a DDict", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict3")
  ddict <- DDict(ddict_df)

  # cat("\n", "ddict@data", "\n")
  # print(ddict@data)

  out <- ddict_table(ddict, table_nm = "df3")
  # cat("\n", "out", "\n")
  # print(out)

  # testthat::skip("debug")
  expect_identical(out, ddict@data)
})



test_that("ddict_table: ERROR", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict3")
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict@data", "\n")
  # print(ddict@data)

  expect_error(
    ddict_table(ddict, table_nm = "ERROR"),
    class = "ValueError",
    regexp = "No records returned from the data dictionary"
  )
})

test_that("ddict_ren: Rename columns using a DDict", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict3")
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict@data", "\n")
  # print(ddict@data)


  df3 <- df_ddict(nm = "df3")
  # cat("\n", "df3", "\n")
  # print(df3)

  out <- ddict_ren(ddict, data = df3)
  # cat("\n", "out", "\n")
  # print(out)

  # testthat::skip("debug")
  expect_identical(names(out), ddict@data$name)
})

test_that("ddict_label: Use name, i.e. raw_name = FALSE", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict3")
  ddict <- DDict(ddict_df)

  # cat("\n", "ddict@data", "\n")
  # print(ddict@data)

  # important to call it df3 to match the table in dictionary
  df3 <- df_ddict(nm = "df3")
  names(df3) <- ddict@data$name
  # cat("\n", "df3", "\n")
  # print(df3)


  out <- ddict_label(ddict, data = df3, is_raw_nm = FALSE)
  # cat("\n", "out", "\n")
  # print(out)
  out_lbl <- labelled::var_label(out)
  # cat("\n", "out labels", "\n")
  # print(out_lbl)

  target <- list(
    "Int" = "integer var 1",
    "Intish" = "integer var 2",
    "Dbl" = "double var 1",
    "Char" = "character var 1",
    "Date" = "date var 1",
    "POSIXct" = "posix var 1",
    "Factor" = "factor var 1"
  )
  # cat("\n", "target labels", "\n")
  # print(target)
  expect_identical(out_lbl, target)
})

test_that("ddict_label: Use name, i.e. raw_name = TRUE", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict3")
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict@data", "\n")
  # print(ddict@data)

  # important to call it df3 to match the table in dictionary
  df3 <- df_ddict(nm = "df3")
  # cat("\n", "df3", "\n")
  # print(df3)


  out <- ddict_label(ddict, data = df3, is_raw_nm = TRUE)
  # cat("\n", "out", "\n")
  # print(out)
  out_lbl <- labelled::var_label(out)
  # cat("\n", "out labels", "\n")
  # print(out_lbl)

  target <- list(
    "varInt" = "integer var 1",
    "varIntish" = "integer var 2",
    "varDbl" = "double var 1",
    "varChar" = "character var 1",
    "varDate" = "date var 1",
    "varPOSIXct" = "posix var 1",
    "varFactor" = "factor var 1"
  )
  # cat("\n", "target labels", "\n")
  # print(target)
  expect_identical(out_lbl, target)
})


test_that("ddict_label: No labels", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict3")
  ddict_df$label <- NA_character_
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict", "\n")
  # print(ddict)


  # important to call it df3 to match the table in dictionary
  df3 <- df_ddict(nm = "df3")
  # cat("\n", "ddict", "\n")
  # print(df3)

  expect_error(
    ddict_label(ddict, data = df3),
    class = "ValueError",
    regexp = "The variables to label where not found"
  )
})


test_that("ddict_cast: Use name, i.e. raw_name = FALSE", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict3")
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict3", "\n")
  # print(ddict@data)

  # important to call it df3 to match the table in dictionary
  df3 <- df_ddict(nm = "df3")
  names(df3) <- ddict@data$name
  # cat("\n", "df3", "\n")
  # print(df3)
  df3_dtypes <- sapply(df3, FUN = \(x) class(x)[1])
  # cat("\n", "df3 dtypes", "\n")
  # print(df3_dtypes)

  target_dtypes <- c(
    "Int" = "integer", "Intish" = "integer",
    "Dbl" = "numeric", "Char" = "factor",
    "Date" = "Date", "POSIXct" = "Date",
    "Factor" = "character"
  )
  # cat("\n", "target dtypes", "\n")
  # print(target_dtypes)

  out <- ddict_cast(ddict, data = df3, is_raw_nm = FALSE)
  out_dtypes <- sapply(out, FUN = \(x) class(x)[1])
  # cat("\n", "out dtypes", "\n")
  # print(out_dtypes)

  target <- c("a", "b")
  expect_identical(out_dtypes, target_dtypes)
})

test_that("ddict_cast: Use raw_name, i.e. raw_name = TRUE", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict3")
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict3", "\n")
  # print(ddict@data)

  # important to call it df3 to match the table in dictionary
  df3 <- df_ddict(nm = "df3")
  # cat("\n", "df3", "\n")
  # print(df3)
  df3_dtypes <- sapply(df3, FUN = \(x) class(x)[1])
  # cat("\n", "df3 dtypes", "\n")
  # print(df3_dtypes)

  target_dtypes <- c(
    "varInt" = "integer", "varIntish" = "integer",
    "varDbl" = "numeric", "varChar" = "factor",
    "varDate" = "Date", "varPOSIXct" = "Date",
    "varFactor" = "character"
  )
  # cat("\n", "target dtypes", "\n")
  # print(target_dtypes)

  out <- ddict_cast(ddict, data = df3, is_raw_nm = TRUE)
  out_dtypes <- sapply(out, FUN = \(x) class(x)[1])
  # cat("\n", "out dtypes", "\n")
  # print(out_dtypes)

  target <- c("a", "b")
  expect_identical(out_dtypes, target_dtypes)
})

test_that("ddict_cast: Warning", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict3")
  ddict_df$raw_dtype <- ddict_df$dtype
  ddict <- DDict(ddict_df)

  # important to call it df3 to match the table in dictionary
  df3 <- df_ddict(nm = "df3")
  # cat("\n", "df3", "\n")
  # print(df3)


  expect_warning(
    ddict_cast(ddict, data = df3, is_raw_nm = TRUE),
    class = "ValueWarning",
    regexp = "There is no data type to cast"
  )
})

test_that("ddict_cast: Error about role", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict3")
  ddict_df$role <- NA_character_
  ddict <- DDict(ddict_df)

  # important to call it df3 to match the table in dictionary
  df3 <- df_ddict(nm = "df3")
  # cat("\n", "df3", "\n")
  # print(df3)


  expect_error(
    ddict_cast(ddict, data = df3, is_raw_nm = TRUE),
    class = "ValueError",
    regexp = "variables must have a role"
  )
})

test_that("ddict_status: Get status of data", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict1")
  ddict_df <- ddict_df[-3, ]
  # cat("\n", "ddict_df", "\n")
  # print(ddict_df)
  ddict <- DDict(ddict_df)

  # important to call it df1 to match the table in dictionary
  df1 <- df_ddict(nm = "df1")
  df1 <- df1[-2]
  df1$varNew <- "new"
  # cat("\n", "df1", "\n")
  # print(df1)

  out <- ddict_status(ddict, data = df1, do_abort = FALSE)
  # cat("\n", "out", "\n")
  # print(out)

  ddict_nms <- ddict@data$name
  data_nms <- names(df1)
  status_nms <- unique(c(ddict_nms, data_nms))
  target <- data.frame(
    table = "df1",
    variable = status_nms
  ) |>
    dplyr::mutate(
      is_ddict = variable %in% ddict_nms,
      is_data = variable %in% data_nms
    ) |>
    dplyr::arrange(variable)
  target$data_dtype <- c(
    "character", "Date", "numeric", "factor",
    "integer", NA_character_, "character", "POSIXct"
  )
  # cat("\n", "target", "\n")
  # print(target)
  expect_identical(out, target)

  # return error when do_abort = TRUE
  expect_error(
    ddict_status(ddict, data = df1, do_abort = TRUE),
    class = "RuntimeError",
    regexp = "Data dictionary should be updated"
  )
})
