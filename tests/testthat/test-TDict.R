test_that("TDict: Create", {
  tdict1_df <- df_tdict(nm = "tdict1")
  tdict <- TDict(tdict1_df)

  expect_identical(tdict@data, tdict1_df)
})
