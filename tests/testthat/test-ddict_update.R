test_that("ddict_update", {
  # Source:
  # https://stackoverflow.com/questions/70702130/testthat-create-shared-variables-between-unit-tests-which-dont-survive-the-tes
  # The rule for <<- is that it looks up the environment tree until it finds
  # the variable, and assigns in globalenv() if that search fails

  # NOTE: This is  a trick to be able to use get() within a function
  #       in testthat.

  df1 <<- df_ddict(nm = "df1")
  # cat("\n", "df1", "\n")
  # print(df1)
  df2 <<- df_ddict(nm = "df2")

  ddict <- suppressWarnings(ddict_update(c("df1", "df2")))
  expect_s3_class(ddict, class = "flrpkgPreproc::DDict")

  rgx <- "Data dictionary created[.] Review before proceeding."
  expect_warning(ddict_update(c("df1", "df2")),
    regexp = rgx,
    class = "DDictWarning"
  )
})
