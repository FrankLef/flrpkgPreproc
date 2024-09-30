test_that("ddict_extract: Extract data in DDict", {
  # testthat::skip("debug")
  ddict <- DDict()
  df1 <- df_ddict(nm = "df1")

  ddict1_df <- df_ddict(nm = "ddict1")

  target <- DDict(ddict1_df)
  # target@data <- ddict1
  # cat("\n", "target", "\n")
  # print(target@data)

  out <- ddict_extract(ddict, df1)
  # cat("\n", "out", "\n")
  # print(out@data)

  expect_identical(out, target)
})

test_that("ddict_extract: ERROR extract data in DDict", {
  # testthat::skip("debug")
  ddict <- DDict()

  df1 <- df_ddict(nm = "df1")

  out <- ddict_extract(ddict, df1)

  expect_error(
    ddict_extract(out, df1),
    class = "ValueError",
    regexp = ".*has.+duplicate records.*"
  )
})
