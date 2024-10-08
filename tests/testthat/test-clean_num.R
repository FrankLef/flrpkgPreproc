test_that("clean_cols_nzv", {
  testthat::skip("depug")
  df <- df_clean_cols_nzv("data")

  out <- df |>
    clean_num_nzv()
  # cat("\n", "out", "\n")
  # print(out)

  target <- df_clean_cols_nzv("target")
  expect_identical(out, target)
})
