test_that("ddict_transf_dat_wk", {
  # testthat::skip("debug")
  df <- data_clean("df")
  # cat("\n", "df", "\n")
  # str(df)
  ddict <- data_clean("ddict")
  # cat("\n", "ddict@data", "\n")
  # str(ddict@data)

  out <- ddict_transf_dat_wk(ddict, data = df, table_nm = "data1")
  # str(out)

  target <- c(names(df), c("cont_date_wk", "clean_date_wk"))
  expect_identical(names(out), target)
})


test_that("ddict_transf_dat_ym", {
  # testthat::skip("debug")
  df <- data_clean("df")
  # cat("\n", "df", "\n")
  # str(df)
  ddict <- data_clean("ddict")
  # cat("\n", "ddict@data", "\n")
  # str(ddict_data)

  out <- ddict_transf_dat_ym(ddict, data = df, table_nm = "data1")
  # str(out)

  target <- c(names(df), c("cont_date_ym", "clean_date_ym"))
  expect_identical(names(out), target)
})
