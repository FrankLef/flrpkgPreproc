test_that("clean_dat_oob", {
  # testthat::skip("debug")
  df <- data_clean("df")
  # cat("\n", "df", "\n")
  # str(df)

  out <- clean_dat_oob(df, start = "2000-01-01", end = "2030-01-01")
  # cat("\n", "out", "\n")
  # str(out)


  expect_identical(out$cont_date, df$clean_date)
})
