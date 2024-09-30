test_that("ddict_extract", {
  df1 <- df_ddict(nm = "df1")
  ddict_df <- df_ddict(nm = "ddict1")
  ddict <- DDict(ddict_df)

  ddict_out <- DDict()
  out <- ddict_extract(ddict_out, df1)
  # cat("\n", "out", "\n")
  # print(out)

  expect_identical(out, ddict)
})
