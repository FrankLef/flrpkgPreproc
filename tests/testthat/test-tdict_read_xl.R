test_that("tdict_read_xl", {
  path <- testthat::test_path()
  ddict <- tdict_read_xl(path)
  expect_s3_class(ddict, class = "flrpkgPreproc::TDict")
})
