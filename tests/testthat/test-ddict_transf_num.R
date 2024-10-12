test_that("ddict_transf_num", {
  # testthat::skip("debug")
  df <- data_clean("df")
  # cat("\n", "df", "\n")
  # str(df)
  ddict <- data_clean("ddict")
  # cat("\n", "ddict@data", "\n")
  # str(ddict@data)

  out <- ddict_transf_num(
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


test_that("ddict_transf_num: Error", {
  # testthat::skip("debug")
  df <- data_clean("df")
  # cat("\n", "df", "\n")
  # str(df)
  ddict <- data_clean("ddict")
  # cat("\n", "ddict@data", "\n")
  # str(ddict@data)

  expect_error(
    ddict_transf_num(
      ddict,
      data = df, fn = \(x) x + 1L, suffix = "WRONG", table_nm = "data1"
    ),
    class = "ValueError",
    regexp = "No records returned from the filtered data dictionary"
  )
})
