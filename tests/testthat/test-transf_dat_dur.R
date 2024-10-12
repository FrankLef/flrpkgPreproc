test_that("transf_dat_dur", {
  # testthat::skip("debug")
  df <- data_clean("df")
  # cat("\n", "df", "\n")
  # str(df)
  ddict <- data_clean("ddict")
  # cat("\n", "ddict@data", "\n")
  # str(ddict@data)

  out <- transf_dat_dur(
    df, dur_var = "dur", start_var = "cont_date", end_var = "end_date")

  target <- c(names(df), "dur")
  expect_identical(names(out), target)
})


test_that("transf_dat_dur_many", {
  # testthat::skip("debug")
  df <- data_clean("df")
  # cat("\n", "df", "\n")
  # str(df)
  ddict <- data_clean("ddict")
  # cat("\n", "ddict@data", "\n")
  # str(ddict@data)


  vars <- data.frame(
    dur = "dur",
    start = "cont_date",
    end = "end_date"
  )
  out <- transf_dat_dur_many(df, vars = vars, table_nm = "data1")

  target <- c(names(df), "dur")
  expect_identical(names(out), target)
})
