test_that("valid_range: HB", {
  df <- df_valid()

  cols <- c("varNorm", "varLognorm", "varExp", "varBeta")
  out <- valid_range_hb(df, cols = cols)
  out <- lapply(out, FUN = \(x) round(x, 2))
  # out

  target <- rng_valid("hb")
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

  target <- rng_valid("madn")
  expect_identical(out, target)
})
