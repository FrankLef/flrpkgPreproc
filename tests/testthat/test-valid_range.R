test_that("valid_range: HB", {
  df <- df_valid()

  cols <- c("varNorm", "varLognorm", "varExp", "varBeta")
  out <- valid_range_hb(df, cols = cols)
  out <- lapply(out, FUN = \(x) round(x, 2))
  # out
  target <- list(
    "varNorm" = c("min" = 0.72, "max" = 11.56),
    "varLognorm" = c("min" = 4.46, "max" = 71.32),
    "varExp" = c("min" = 0.99, "max" = 15.88),
    "varBeta" = c("min" = 1.17, "max" = 18.76)
  )
  expect_identical(out, target)
})


test_that("valid_range: MADN", {
  df <- df_valid()

  cols <- c("varNorm", "varLognorm", "varExp", "varBeta")
  out <- valid_range_madn(df, cols = cols)
  out <- lapply(out, FUN = \(x) round(x, 2))
  # out

  # should aproximate normal dist for column varNorm
  # c("min" = qnorm(0.025, mean = 3, sd = 0.5),
  #   "max" = qnorm(0.975, mean = 3, sd = 0.5))

  target <- list(
    "varNorm" = c("min" = 1.73, "max" = 4.05),
    "varLognorm" = c("min" = 5.6, "max" = 30.06),
    "varExp" = c("min" = 0.65, "max" = 7.29),
    "varBeta" = c("min" = 2.53, "max" = 6.85)
  )
  expect_identical(out, target)
})
