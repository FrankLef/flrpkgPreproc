test_that("prune_reset", {
  df <- df_prune()
  out <- prune_reset(df)
  target <- df
  target$prune_id <- "ok"
  expect_identical(out, target)
})


test_that("prune_upd: with dplyr", {
  # testthat::skip("debug")

  df <- df_prune()
  # cat("\n", "df", "\n")
  # print(df)

  out <- df |>
    prune_upd(flags = "varLog", id = "Y")
  # cat("\n", "out", "\n")
  # print(out)

  target <- df
  target$prune_id[df$varChar != "a" & target$varInt == 2L] <- "Y"
  # cat("\n", "target", "\n")
  # print(target)

  expect_identical(out, target)
})

test_that("prune_upd: ERROR, using ok as id", {
  df <- df_prune()

  expect_error(prune_upd(df, flags = "ERROR", id = "Y"),
    regexp = "Assertion on 'flags' failed"
  )

  expect_error(prune_upd(df, flags = "varLog", id = "ok"),
    regexp = "Assertion on 'id' failed"
  )
})
