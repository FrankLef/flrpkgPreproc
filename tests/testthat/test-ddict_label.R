test_that("ddict_label: Use name, i.e. raw_name = FALSE", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict3")
  ddict <- DDict(ddict_df)

  # cat("\n", "ddict@data", "\n")
  # print(ddict@data)

  # important to call it df3 to match the table in dictionary
  df3 <- df_ddict(nm = "df3")
  names(df3) <- ddict@data$name
  # cat("\n", "df3", "\n")
  # print(df3)


  out <- ddict_label(ddict, data = df3, is_raw_nm = FALSE)
  # cat("\n", "out", "\n")
  # print(out)
  out_lbl <- labelled::var_label(out)
  # cat("\n", "out labels", "\n")
  # print(out_lbl)

  target <- list(
    "Int" = "integer var 1",
    "Intish" = "integer var 2",
    "Dbl" = "double var 1",
    "Char" = "character var 1",
    "Date" = "date var 1",
    "POSIXct" = "posix var 1",
    "Factor" = "factor var 1"
  )
  # cat("\n", "target labels", "\n")
  # print(target)
  expect_identical(out_lbl, target)
})

test_that("ddict_label: Use name, i.e. raw_name = TRUE", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict3")
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict@data", "\n")
  # print(ddict@data)

  # important to call it df3 to match the table in dictionary
  df3 <- df_ddict(nm = "df3")
  # cat("\n", "df3", "\n")
  # print(df3)


  out <- ddict_label(ddict, data = df3, is_raw_nm = TRUE)
  # cat("\n", "out", "\n")
  # print(out)
  out_lbl <- labelled::var_label(out)
  # cat("\n", "out labels", "\n")
  # print(out_lbl)

  target <- list(
    "varInt" = "integer var 1",
    "varIntish" = "integer var 2",
    "varDbl" = "double var 1",
    "varChar" = "character var 1",
    "varDate" = "date var 1",
    "varPOSIXct" = "posix var 1",
    "varFactor" = "factor var 1"
  )
  # cat("\n", "target labels", "\n")
  # print(target)
  expect_identical(out_lbl, target)
})


test_that("ddict_label: No labels", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict3")
  ddict_df$label <- NA_character_
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict@data", "\n")
  # print(ddict@data)


  # important to call it df3 to match the table in dictionary
  df3 <- df_ddict(nm = "df3")
  # cat("\n", "df3", "\n")
  # print(df3)

  # using the wrong column gives an error about unfound variables
  expect_error(
    ddict_label(ddict, data = df3, is_raw_nm = FALSE),
    class = "ValueError",
    regexp = "The variables to label where not found"
  )

  # warning when no label is to be applied because label column is empty
  expect_warning(
    ddict_label(ddict, data = df3, is_raw_nm = TRUE),
    class = "ValueWarning",
    regexp = "There is no label to apply"
  )
})
