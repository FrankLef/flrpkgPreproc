test_that("clean_cols_constant", {
  testthat::skip("debug")
  df <- data2clean
  # cat("\n", "dim(df)", "\n")
  # print(dim(df))

  out <- clean_cols_constant(df)
  # cat("\n", "out", "\n")
  # print(out)

  target <- c("same_num" = 11L, "same_text" = 22L)
  expect_identical(out, target)
})
