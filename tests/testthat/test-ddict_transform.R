test_that("ddict_transform: Numeric", {
  # testthat::skip("debug")
  df <- data_clean("df")
  # cat("\n", "df", "\n")
  # str(df)
  ddict <- data_clean("ddict")
  # cat("\n", "ddict@data", "\n")
  # str(ddict@data)

  out <- ddict_transform(
    ddict,
    data = df, fn = \(x) x + 1L, suffix = "lg", table_nm = "data1"
  )
  # cat("\n", "out", "\n")
  # str(out)

  new_nms <- c(
    "normal_num_lg", "abnormal_num_lg", "lognormal_num_lg",
    "logabnormal_num_lg", "log1ps10_lg"
  )
  target <- c(names(df), new_nms)
  expect_identical(names(out), target)

  target <- df$normal_num + 1L
  expect_identical(out$normal_num_lg, target)
})


test_that("ddict_transform: Error", {
  # testthat::skip("debug")
  df <- data_clean("df")
  # cat("\n", "df", "\n")
  # str(df)
  ddict <- data_clean("ddict")
  # cat("\n", "ddict@data", "\n")
  # str(ddict@data)

  expect_error(
    ddict_transform(
      ddict,
      data = df, fn = \(x) x + 1L, suffix = "WRONG", table_nm = "data1"
    ),
    class = "ValueError",
    regexp = "No records returned from the filtered data dictionary"
  )
})


test_that("ddict_transform: POSIXct wk", {
  testthat::skip("debug")
  df <- data_clean("df")
  # cat("\n", "df", "\n")
  # str(df)
  ddict <- data_clean("ddict")
  # cat("\n", "ddict@data", "\n")
  # str(ddict@data)


  out <- ddict_transform(
    ddict,
    data = df,
    fn = \(x) lubridate::floor_date(x, unit = "weeks"),
    suffix = "wk", table_nm = "data1"
  )
  # str(out)

  target <- c(names(df), c("cont_date_wk", "clean_date_wk"))
  expect_identical(names(out), target)
})


test_that("ddict_transform: POSIXct ym", {
  testthat::skip("debug")
  df <- data_clean("df")
  # cat("\n", "df", "\n")
  # str(df)
  ddict <- data_clean("ddict")
  # cat("\n", "ddict@data", "\n")
  # str(ddict_data)

  out <- ddict_transform(
    ddict,
    data = df,
    fn = \(x) lubridate::isoyear(x) + (lubridate::month(x) / 100),
    suffix = "ym", table_nm = "data1"
  )
  # str(out)

  target <- c(names(df), c("cont_date_ym", "clean_date_ym"))
  expect_identical(names(out), target)
})

