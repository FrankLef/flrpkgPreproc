test_that("clean_cols_constant", {
  # testthat::skip("debug")
  df <- data_clean("df")
  # cat("\n", "dim(df)", "\n")
  # print(dim(df))

  out <- clean_cols_constant(df)
  # cat("\n", "out", "\n")
  # print(out)

  target <- c("same_num" = 11L, "same_text" = 22L)
  expect_identical(out, target)
})


test_that("clean_cols_nzv", {
  # testthat::skip("debug")
  df <- data_clean("df")
  # cat("\n", "dim(df)", "\n")
  # str(df)

  out <- clean_cols_nzv(df, freq_tol = 95 / 5, uniq_tol = 0.1, info = FALSE)
  # cat("\n", "out", "\n")
  # str(out)
  # print(out)

  target <- c("same_num" = 11L, "nzv_num" = 12L)
  expect_identical(out, target)

  out <- clean_cols_nzv(df, freq_tol = 95 / 5, uniq_tol = 0.1, info = TRUE)
  # cat("\n", "out", "\n")
  # print(out)
  expect_identical(dim(out), c(18L, 6L))

  target <- df |>
    dplyr::select(tidyselect::where(is.numeric)) |>
    names()
  expect_identical(out$id, target)
})
