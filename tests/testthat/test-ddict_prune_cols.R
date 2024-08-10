test_that("ddict_prune_cols: remove cols", {
  # testthat::skip("debug")

  ddict_df <- df_ddict(nm = "ddict2")
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict", "\n")
  # print(class(ddict))

  df2 <- df_ddict(nm = "df2")
  # cat("\n", "df2", "\n")
  # print(str(df2))
  out <- suppressWarnings(ddict_prune_cols(ddict, data = df2))
  # cat("\n", "out", "\n")
  # print(out)

  expect_identical(length(df2) - length(out), 4L)
})

test_that("ddict_prune_cols: Warning", {
  # testthat::skip("debug")

  ddict_df <- df_ddict(nm = "ddict2")
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict", "\n")
  # print(class(ddict))

  df2 <- df_ddict(nm = "df2")
  # cat("\n", "df2", "\n")
  # print(str(df2))

  expect_warning(
    ddict_prune_cols(ddict, data = df2),
    class = "ValueWarning",
    regexp = "Removing columns without a role"
  )
})

test_that("ddict_prune_cols: Error", {
  # testthat::skip("debug")

  ddict_df <- df_ddict(nm = "ddict2")
  ddict_df$role <- NA_character_
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict", "\n")
  # print(class(ddict))

  df2 <- df_ddict(nm = "df2")
  # cat("\n", "df2", "\n")
  # print(str(df2))

  expect_error(
    ddict_prune_cols(ddict, data = df2),
    class = "ValueError",
    regexp = "At least 1 column must be left in the data"
  )
})
