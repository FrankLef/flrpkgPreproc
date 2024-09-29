test_that("tdict_load_accdb", {
  a_file <- "db_MsAccess.accdb"
  a_qry <- "qry_clients"
  fn <- system.file(
    "extdata", a_file,
    package = "flrpkgPreproc", mustWork = TRUE
  )
  out <- tdict_load_accdb(
    path = dirname(fn),
    file = basename(fn),
    qry = a_qry
  )
  target <- data.frame(
    client_no = 0:5,
    client =
      c("_NA", "client 1", "client 2", "client 3", "client 4", "client 5")
  )
  expect_identical(out, target)
})
