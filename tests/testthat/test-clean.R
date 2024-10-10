test_that("clean_inf", {
  # testthat::skip("debug")
  df <- data_clean("df")
  seed <- 23627L
  n <- 3L
  df$normal_num[get_pos(df$normal_num, n = n, seed = seed)] <- Inf
  df$lognormal_num[get_pos(df$lognormal_num, n = n, seed = seed)] <- Inf
  # cat("\n", "df", "\n")
  # print(df[, c("ndx", "normal_num", "lognormal_num")])

  out <- clean_inf(df)

  na_nb <- sum(is.na(out$normal_num))
  target <- sum(is.na(df$normal_num)) + n
  expect_identical(na_nb, target)

  na_nb <- sum(is.na(out$lognormal_num))
  target <- sum(is.na(df$lognormal_num)) + n
  expect_identical(na_nb, target)
})


test_that("clean_empty", {
  # testthat::skip("debug")
  df <- data_clean("df")
  df <- df[, c("ndx", "ran_color", "cont_text", "clean_text")]
  # cat("\n", "df", "\n")
  # print(df)

  out <- clean_empty(df[, c("ndx", "ran_color", "cont_text")])
  # cat("\n", "out", "\n")
  # print(out)

  expect_identical(out$cont_text, df$clean_text)
})

test_that("clean_NA", {
  testthat::skip("debug")
  df <- df_clean(nm = "data")

  out <- clean_NA(df)

  out_nb <- sapply(X = out, FUN = \(x) sum(is.na(x)))

  target <- df_clean(nm = "target")
  target_nb <- sapply(X = target, FUN = \(x) sum(is.na(x)))

  expect_identical(out_nb, target_nb)
  stopifnot(identical(out, target))
})
