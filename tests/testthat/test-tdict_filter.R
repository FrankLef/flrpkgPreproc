test_that("tdict_filter: Filter table dict", {
  # testthat::skip("debug")
  tdict1_df <- df_tdict(nm = "tdict1")
  tdict <- TDict(tdict1_df)

  # no selection returns the full table.
  out <- tdict_filter(tdict)
  # cat("\n", "out", "\n")
  # print(out)
  expect_identical(dim(out), dim(tdict1_df))

  out <- tdict_filter(tdict, type_rgx = "xlsx")
  # cat("\n", "out", "\n")
  # print(out)
  expect_identical(dim(out), c(1L, length(tdict1_df)))

  out <- tdict_filter(tdict, role_rgx = r"(\bdim\b)")
  # cat("\n", "out", "\n")
  # print(out)
  expect_identical(dim(out), c(1L, length(tdict1_df)))

  out <- tdict_filter(tdict, process_rgx = r"(\bload\b)")
  # cat("\n", "out", "\n")
  # print(out)
  expect_identical(dim(out), c(2L, length(tdict1_df)))

  out <- tdict_filter(tdict, rule_rgx = NA_character_)
  # cat("\n", "out", "\n")
  # print(out)
  expect_identical(dim(out), c(2L, length(tdict1_df)))

  out <- tdict_filter(
    tdict,
    type_rgx = "accdb", role_rgx = r"(\bdim\b)", process_rgx = r"(\bload\b)"
  )
  # cat("\n", "out", "\n")
  # print(out)
  expect_identical(dim(out), c(1L, length(tdict1_df)))
})

test_that("tdict_filter: ERROR", {
  # testthat::skip("debug")

  tdict1_df <- df_tdict(nm = "tdict1")
  tdict <- TDict(tdict1_df)

  expect_error(tdict_filter(tdict, role_rgx = "ERROR"),
    class = "ValueError",
    regexp = "No records returned from the table dictionary"
  )
})
