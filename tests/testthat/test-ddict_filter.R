test_that("ddict_filter: Default, no filter", {
  df3 <- df_ddict(nm = "df3")
  # cat("\n", "df3", "\n")
  # print(df3)
  table_nm <- "df3"
  # cat("\n", "table_nm", "\n")
  # print(table_nm)

  ddict_df <- df_ddict(nm = "ddict3")
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict@data", "\n")
  # print(ddict@data)

  # when no filter, the full data is returned
  out <- ddict_filter(ddict)

  target <- ddict_df
  # cat("\n", "target", "\n")
  # print(target)

  expect_identical(out, target)
})


test_that("ddict_filter: Filter with table name.", {
  df3 <- df_ddict(nm = "df3")
  # cat("\n", "df3", "\n")
  # print(df3)
  table_nm <- "df3"
  # cat("\n", "table_nm", "\n")
  # print(table_nm)
  role_rgx <- r"(\brole1\b)"


  ddict_df <- df_ddict(nm = "ddict3")
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict@data", "\n")
  # print(ddict@data)

  out <- ddict_filter(ddict, table_nm = table_nm, role_rgx = role_rgx)

  target <- ddict_df |>
    dplyr::filter(table == table_nm, grepl(role_rgx, x = role))
  # cat("\n", "target", "\n")
  # print(target)

  expect_identical(out, target)
})


test_that("ddict_filter: ERROR filter with no result.", {
  df3 <- df_ddict(nm = "df3")
  # cat("\n", "df3", "\n")
  # print(df3)
  table_nm <- "df3"
  # cat("\n", "table_nm", "\n")
  # print(table_nm)

  ddict_df <- df_ddict(nm = "ddict3")
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict@data", "\n")
  # print(ddict@data)

  expect_error(
    ddict_filter(ddict, table_nm = table_nm, role_rgx = "ERROR"),
    class = "ValueError",
    regexp = "No records returned from the filtered data dictionary"
  )
})

test_that("ddict_filter: Filter with table name and pull_var.", {
  df3 <- df_ddict(nm = "df3")
  # cat("\n", "df3", "\n")
  # print(df3)
  table_nm <- "df3"
  # cat("\n", "table_nm", "\n")
  # print(table_nm)
  role_rgx <- r"(\brole1\b)"


  ddict_df <- df_ddict(nm = "ddict3")
  ddict <- DDict(ddict_df)
  # cat("\n", "ddict@data", "\n")
  # print(ddict@data)

  out <- ddict_filter(ddict,
    table_nm = table_nm, role_rgx = role_rgx,
    pull_var = "name"
  )

  target <- ddict_df |>
    dplyr::filter(table == table_nm, grepl(role_rgx, x = role)) |>
    dplyr::pull(name)
  # cat("\n", "target", "\n")
  # print(target)

  expect_identical(out, target)
})
