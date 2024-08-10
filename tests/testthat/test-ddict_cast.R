test_that("ddict_cast: Use name, i.e. raw_name = FALSE", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict3")
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict3", "\n")
  # print(ddict@data)

  # important to call it df3 to match the table in dictionary
  df3 <- df_ddict(nm = "df3")
  names(df3) <- ddict@data$name
  # cat("\n", "df3", "\n")
  # print(df3)
  df3_dtypes <- sapply(df3, FUN = \(x) class(x)[1])
  # cat("\n", "df3 dtypes", "\n")
  # print(df3_dtypes)

  target_dtypes <- c(
    "Int" = "integer", "Intish" = "integer",
    "Dbl" = "numeric", "Char" = "factor",
    "Date" = "Date", "POSIXct" = "Date",
    "Factor" = "character"
  )
  # cat("\n", "target dtypes", "\n")
  # print(target_dtypes)

  out <- ddict_cast(ddict, data = df3, is_raw_nm = FALSE)
  out_dtypes <- sapply(out, FUN = \(x) class(x)[1])
  # cat("\n", "out dtypes", "\n")
  # print(out_dtypes)

  target <- c("a", "b")
  expect_identical(out_dtypes, target_dtypes)
})

test_that("ddict_cast: Use raw_name, i.e. raw_name = TRUE", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict3")
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict3", "\n")
  # print(ddict@data)

  # important to call it df3 to match the table in dictionary
  df3 <- df_ddict(nm = "df3")
  # cat("\n", "df3", "\n")
  # print(df3)
  df3_dtypes <- sapply(df3, FUN = \(x) class(x)[1])
  # cat("\n", "df3 dtypes", "\n")
  # print(df3_dtypes)

  target_dtypes <- c(
    "varInt" = "integer", "varIntish" = "integer",
    "varDbl" = "numeric", "varChar" = "factor",
    "varDate" = "Date", "varPOSIXct" = "Date",
    "varFactor" = "character"
  )
  # cat("\n", "target dtypes", "\n")
  # print(target_dtypes)

  out <- ddict_cast(ddict, data = df3, is_raw_nm = TRUE)
  out_dtypes <- sapply(out, FUN = \(x) class(x)[1])
  # cat("\n", "out dtypes", "\n")
  # print(out_dtypes)

  target <- c("a", "b")
  expect_identical(out_dtypes, target_dtypes)
})

test_that("ddict_cast: Warning", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict3")
  ddict_df$raw_dtype <- ddict_df$dtype
  ddict <- DDict(ddict_df)

  # important to call it df3 to match the table in dictionary
  df3 <- df_ddict(nm = "df3")
  # cat("\n", "df3", "\n")
  # print(df3)


  expect_warning(
    ddict_cast(ddict, data = df3, is_raw_nm = TRUE),
    class = "ValueWarning",
    regexp = "There is no data type to cast"
  )
})

test_that("ddict_cast: Error about role", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict3")
  ddict_df$role <- NA_character_
  ddict <- DDict(ddict_df)

  # important to call it df3 to match the table in dictionary
  df3 <- df_ddict(nm = "df3")
  # cat("\n", "df3", "\n")
  # print(df3)


  expect_error(
    ddict_cast(ddict, data = df3, is_raw_nm = TRUE),
    class = "ValueError",
    regexp = "variables must have a role"
  )
})
