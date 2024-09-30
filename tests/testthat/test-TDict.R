test_that("TDict: Create", {
  tdict1_df <- df_tdict(nm = "tdict1")
  tdict <- TDict(tdict1_df)

  expect_identical(tdict@data, tdict1_df)
})

test_that("TDict: File name properties", {
  tdict <- TDict()
  ext <- "xlsx"

  # data file name
  fn <- paste0(paste("tdict_raw", Sys.Date(), sep = "_"), ".", ext)
  target <- file.path(getwd(), fn)
  expect_identical(tdict@data_fn, target)
})

test_that("tdict_table: ERROR.", {
  # testthat::skip("debug")
  tdict <- TDict() # empty dictionary


  expect_error(tdict_table(tdict),
    class = "ValueError",
    regexp = "The table dictionary is empty"
  )
})
