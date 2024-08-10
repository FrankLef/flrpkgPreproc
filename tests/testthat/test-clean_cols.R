test_that("clean_cols_constant", {
  df <- df_clean_cols_constant("data")

  out <- df |>
    clean_cols_constant()

  target <- df_clean_cols_constant("target")
  expect_identical(out, target)
})


test_that("clean_cols_nzv", {
  df <- df_clean_cols_nzv("data")

  out <- df |>
    clean_cols_nzv()
  # cat("\n", "out", "\n")
  # print(out)

  target <- df_clean_cols_nzv("target")
  expect_identical(out, target)
})
