test_that("ddict_transf_dat_wk", {
  # testthat::skip("debug")
  df <- data_clean("df")
  # cat("\n", "df", "\n")
  # str(df)
  ddict_data <- data_clean("ddict")
  # cat("\n", "ddict_data", "\n")
  # str(ddict_data)
  ddict <- DDict(ddict_data)
  # cat("\n", "ddict", "\n")
  # print(utils::head(ddict@data))

  out <- ddict_transf_dat_wk(ddict, data = df, table_nm = "data1")
  # str(out)

  target <- c(names(df), c("cont_date_wk", "clean_date_wk"))
  expect_identical(names(out), target)
})
