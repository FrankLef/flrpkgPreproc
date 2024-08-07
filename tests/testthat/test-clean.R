test_that("clean_inf", {
  # testthat::skip("debug")
  df <- df_clean(nm = "data")

  out <- clean_inf(df)

  cols <- c("int_ok", "int1", "dbl1")

  out_nb <- sapply(X = out[, cols], FUN = \(x) sum(is.na(x)))

  target <- df_clean(nm = "target")
  target_nb <- sapply(X = target[, cols], FUN = \(x) sum(is.na(x)))

  expect_identical(out_nb, target_nb)
})


test_that("clean_empty", {
  # testthat::skip("debug")
  df <- df_clean(nm = "data")

  out <- clean_empty(df)

  cols <- c("char_ok", "char1", "char2", "char_uniq", "char_empty")

  out_nb <- sapply(X = out[, cols], FUN = \(x) sum(is.na(x)))

  target <- df_clean(nm = "target")
  target_nb <- sapply(X = target[, cols], FUN = \(x) sum(is.na(x)))

  expect_identical(out_nb, target_nb)
})

test_that("clean_NA", {
  # testthat::skip("debug")
  df <- df_clean(nm = "data")

  out <- clean_NA(df)

  out_nb <- sapply(X = out, FUN = \(x) sum(is.na(x)))

  target <- df_clean(nm = "target")
  target_nb <- sapply(X = target, FUN = \(x) sum(is.na(x)))

  expect_identical(out_nb, target_nb)
  stopifnot(identical(out, target))
})
