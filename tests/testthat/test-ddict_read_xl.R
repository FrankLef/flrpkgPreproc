test_that("ddict_read_xl", {
  path <- testthat::test_path()
  ddict <- ddict_read_xl(path)
  expect_s3_class(ddict, class = "flrpkgPreproc::DDict")
})
