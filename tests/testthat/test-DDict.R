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
  ext <- "xlsx"

  # data file name
  fn <- paste0(paste("ddict_raw", Sys.Date(), sep = "_"), ".", ext)
  target <- file.path(getwd(), fn)
  expect_identical(ddict@data_fn, target)

  # status file name
  fn <- paste0(paste("ddict_status_raw", Sys.Date(), sep = "_"), ".", ext)
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
