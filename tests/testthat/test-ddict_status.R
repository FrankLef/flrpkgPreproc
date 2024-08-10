test_that("ddict_status: Get status of data", {
  # testthat::skip("debug")
  ddict_df <- df_ddict(nm = "ddict1")
  ddict_df <- ddict_df[-3, ]
  # cat("\n", "ddict_df", "\n")
  # print(ddict_df)
  ddict <- DDict(ddict_df)

  # important to call it df1 to match the table in dictionary
  df1 <- df_ddict(nm = "df1")
  df1 <- df1[-2]
  df1$varNew <- "new"
  # cat("\n", "df1", "\n")
  # print(df1)

  out <- ddict_status(ddict, data = df1, do_abort = FALSE)
  # cat("\n", "out", "\n")
  # print(out)

  ddict_nms <- ddict@data$name
  data_nms <- names(df1)
  status_nms <- unique(c(ddict_nms, data_nms))
  target <- data.frame(
    table = "df1",
    variable = status_nms
  ) |>
    dplyr::mutate(
      is_ddict = variable %in% ddict_nms,
      is_data = variable %in% data_nms
    ) |>
    dplyr::arrange(variable)
  target$data_dtype <- c(
    "character", "Date", "numeric", "factor",
    "integer", NA_character_, "character", "POSIXct"
  )
  # cat("\n", "target", "\n")
  # print(target)
  expect_identical(out, target)

  # return error when do_abort = TRUE
  expect_error(
    ddict_status(ddict, data = df1, do_abort = TRUE),
    class = "RuntimeError",
    regexp = "Data dictionary should be updated"
  )
})
