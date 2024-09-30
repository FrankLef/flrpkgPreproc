test_that("tdict_read_xl", {
  path <- testthat::test_path()
  ddict <- tdict_read_xl(path)
  expect_s3_class(ddict, class = "flrpkgPreproc::TDict")
})

test_that("tdict_write_xl", {
  tdict1_df <- df_tdict(nm = "tdict1")
  tdict <- TDict(tdict1_df)
  # cat("\n", "tdict", "\n")
  # print(tdict@data)
  fn <- tdict@data_fn

  out <- tdict_write_xl(tdict, path = dirname(fn), file = basename(fn))

  expect_identical(out, fn)
  file.remove(fn)
})
