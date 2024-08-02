test_that("Rules: Instantiate new object.", {
  rules <- Rules()
  expect_true(S7::S7_inherits(rules, Rules))
})
