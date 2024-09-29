test_that("ddict_read_xl", {
  path <- testthat::test_path()
  ddict <- ddict_read_xl(path)
  expect_s3_class(ddict, class = "flrpkgPreproc::DDict")
})


test_that("ddict_write_xl", {
  ddict1_df <- df_ddict(nm = "ddict1")
  ddict <- DDict(ddict1_df)
  # cat("\n", "ddict", "\n")
  # print(ddict@data)
  fn <- ddict@data_fn

  out <- ddict_write_xl(ddict, path = dirname(fn), file = basename(fn))

  expect_identical(out, fn)
  file.remove(fn)
})
