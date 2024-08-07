test_that("clean_cols_uniq", {
  df <- df_clean_cols_uniq("data")

  out <- df |>
    clean_cols_uniq()

  target <- df_clean_cols_uniq("target")
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
