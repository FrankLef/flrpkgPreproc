test_that("Hello returns a message to console", {
  testthat::skip("Not used.")
  expect_message(object = hello(), regexp = "^Hello.*")
})

test_that("Hello returns a string invisibly", {
  testthat::skip("Not used.")
  # see example in testthat::expect_invisible() documentation
  out <- expect_invisible(call = hello())
  expect_match(object = out, regexp = "^Hello.*")
})

test_that("Hello returns an error", {
  testthat::skip("Not used.")
  expect_error(object = hello("wrong"), class = "ValueError")
})
