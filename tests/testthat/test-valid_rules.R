test_that("valid_rule_rng", {
  out <- valid_rule_rng("var", rng = c("min" = 0, "max" = 1))
  expect_identical(out, "in_range(var, min = 0, max = 1)")

  expect_error(
    valid_rule_rng("var", rng = c(0, 1)),
    regexp = r"(Assertion on 'names\(rng\))"
  )
})

test_that("valid_rules: range hb", {
  rng <- rng_valid("hb")

  out <- valid_rules(rng, suffix = "oob")
  # cat("\n", "out", "\n")
  # print(out)

  target <- rules_valid("hb")

  expect_identical(out, target)
})

test_that("valid_rules: range madn", {
  rng <- rng_valid("madn")

  out <- valid_rules(rng, suffix = "oob")
  # cat("\n", "out", "\n")
  # print(out)

  target <- rules_valid("madn")

  expect_identical(out, target)
})
