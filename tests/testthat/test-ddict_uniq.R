test_that("ddict_uniq: test uniqueness", {
  # testthat::skip("debug")

  ddict_df <- df_ddict(nm = "ddict2")
  ddict_df$role[c(4, 6, 7)] <- "uniq"
  # cat("\n", "ddict_df", "\n")
  # print(str(ddict_df))
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict", "\n")
  # print(class(ddict))

  df2 <- df_ddict(nm = "df2")
  # cat("\n", "df2", "\n")
  # print(str(df2))
  out <- ddict_uniq(ddict, data = df2, role_rgx = r"(\buniq\b)")
  # cat("\n", "out", "\n")
  # print(out)


  target <- c("varChar" = FALSE, "varPOSIXct" = TRUE, "varFactor" = TRUE)
  # cat("\n", "target", "\n")
  # print(target)

  expect_identical(out, target)
})

test_that("ddict_uniq: error", {
  # testthat::skip("debug")

  ddict_df <- df_ddict(nm = "ddict2")
  ddict_df$role[c(4, 6, 7)] <- "tba"
  # cat("\n", "ddict_df", "\n")
  # print(str(ddict_df))
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict", "\n")
  # print(class(ddict))

  df2 <- df_ddict(nm = "df2")
  # cat("\n", "df2", "\n")
  # print(str(df2))

  expect_error(ddict_uniq(ddict, data = df2, role_rgx = r"(\buniq\b)"),
    class = "ValueError",
    regexp = "There is no column identified as unique"
  )
})
