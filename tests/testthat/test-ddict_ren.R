test_that("ddict_ren: Rename columns using a DDict", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict3")
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict@data", "\n")
  # print(ddict@data)


  df3 <- df_ddict(nm = "df3")
  # cat("\n", "df3", "\n")
  # print(df3)

  out <- ddict_ren(ddict, data = df3)
  # cat("\n", "out", "\n")
  # print(out)

  # testthat::skip("debug")
  expect_identical(names(out), ddict@data$name)
})
