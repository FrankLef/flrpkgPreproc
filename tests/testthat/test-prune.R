test_that("prune_reset", {
  df <- df_prune()
  out <- prune_reset(df)
  target <- df
  target$prune_id <- "ok"
  expect_identical(out, target)
})

test_that("prune_upd", {
  df <- df_prune()
  df <- prune_reset(df)
  df$prune_id[df$varInt == 2L] <- "X"
  # cat("\n", "df", "\n")
  # print(df)

  the_flags <- (df$varInt == 3L)
  out <- prune_upd(df, flags = the_flags, id = "Y")
  # cat("\n", "out", "\n")
  # print(out)

  target <- df
  target$prune_id[target$varInt == 3L] <- "Y"
  # cat("\n", "target", "\n")
  # print(target)

  expect_identical(out, target)
})

test_that("prune_upd: ERROR, using ok as id", {
  df <- df_prune()
  df <- prune_reset(df)
  df$prune_id[df$varInt == 2L] <- "X"
  # cat("\n", "df", "\n")
  # print(df)

  the_flags <- (df$varInt == 3L)
  expect_error(prune_upd(df, flags = the_flags, id = "ok"),
    regexp = "Assertion on 'id' failed"
  )
})
