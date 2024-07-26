test_that("DDict: Create DDictionnary", {
  ddict <- DDict()

  target <- data.frame(
    table = character(),
    raw_name = character(),
    name = character(),
    label = character(),
    desc = character(),
    note = character(),
    raw_dtype = character(),
    dtype = character()
  )
  expect_identical(ddict@data, target)
})

test_that("DDict: Validate DDict@data", {
  ddict <- DDict()

  # column missing
  err1 <- df_ddict_err(nm = "ddict1")
  expect_error(
    {
      ddict@data <- err1
    },
    class = "ValueError",
    regexp = "Must have exactly.+cols"
  )

  # invalid column
  err2 <- df_ddict_err(nm = "ddict2")
  expect_error(
    {
      ddict@data <- err2
    },
    class = "ValueError",
    regexp = "Names must be a permutation of set"
  )
  # invalid raw_name
  err3 <- df_ddict_err(nm = "ddict3")
  expect_error(
    {
      ddict@data <- err3
    },
    class = "ValueError",
    regexp = "All elements must have at least 1 characters"
  )
  # invalid name
  err4 <- df_ddict_err(nm = "ddict4")
  expect_error(
    {
      ddict@data <- err4
    },
    class = "ValueError",
    regexp = "Contains missing values"
  )


  # duplicate records
  ddict1 <- df_ddict(nm = "ddict1")
  ddict2 <- rbind(ddict1, ddict1)
  expect_error(
    {
      ddict@data <- ddict2
    },
    class = "ValueError",
    regexp = "has.+duplicate records"
  )
})

test_that("addDDict: Input errors.", {
  ddict <- DDict()

  vars <- 1:3
  rgx <- "Assertion on 'vars' failed: Must have names"
  expect_error(addDDict(ddict, vars = vars), regexp = rgx)

  vars <- c("one" = 1, "two" = 2, "three" = 3)
  rgx <- "Assertion on 'names[(]vars[)]' failed: Names must be a permutation of set"
  expect_error(addDDict(ddict, vars = vars), regexp = rgx)
})

test_that("addDDict: Add variable to DDictionnary.", {
  ddict <- DDict()

  vars <- c(
    "table" = "tbl", "raw_name" = "var", "name" = "nm",
    "label" = "lbl", "desc" = "description", "note" = "a note",
    "raw_dtype" = "character", "dtype" = "character"
  )

  target <- ddict
  target@data <- rbind(target@data, as.data.frame(t(vars)))
  # cat("\n", "target", "\n")
  # print(target)

  out <- addDDict(ddict, vars = vars)
  # cat("\n", "test", "\n")
  # print(out)

  expect_identical(out, target)
})

test_that("rmDDict: Remove variable from DDictionnary.", {
  ddict <- DDict()

  vars1 <- c(
    "table" = "table1", "raw_name" = "var1", "name" = "nm1",
    "label" = "lbl", "desc" = "description", "note" = "a note",
    "raw_dtype" = "character", "dtype" = "character"
  )
  vars2 <- c(
    "table" = "table2", "raw_name" = "var2", "name" = "nm2",
    "label" = "lbl", "desc" = "description", "note" = "a note",
    "raw_dtype" = "integer", "dtype" = "integer"
  )
  ddict <- addDDict(ddict, vars = vars1)
  ddict <- addDDict(ddict, vars = vars2)
  # cat("\n", "ddict", "\n")
  # print(ddict)


  target <- DDict()
  target@data <- rbind(target@data, as.data.frame(t(vars1)))
  # cat("\n", "target", "\n")
  # print(target)

  out <- rmDDict(ddict, raw_name = "var2")
  # cat("\n", "out", "\n")
  # print(out)

  expect_identical(out, target)
})

test_that("extractDDict: Extract data in DDict", {
  ddict <- DDict()

  df1 <- df_ddict(nm = "df1")
  ddict1 <- df_ddict(nm = "ddict1")

  target <- DDict()
  target@data <- ddict1
  # cat("\n", "target", "\n")
  # print(target@data)

  out <- extractDDict(ddict, df1)
  # cat("\n", "out", "\n")
  # print(out@data)

  expect_identical(out, target)
})

test_that("extractDDict: ERROR extract data in DDict", {
  ddict <- DDict()

  df1 <- df_ddict(nm = "df1")
  ddict1 <- df_ddict(nm = "ddict1")

  out <- extractDDict(ddict, df1)

  expect_error(
    {
      extractDDict(out, df1)
    },
    class = "ValueError",
    regexp = ".*has.+duplicate records.*"
  )
})

test_that("renDDict: Rename columns using a DDict", {
  ddict <- DDict()


  df3 <- df_ddict(nm = "df3")
  ddict@data <- df_ddict(nm = "ddict3")

  out <- renDDict(ddict, data = df3)

  # cat("\n", "out", "\n")
  # print(out)

  # testthat::skip("debug")
  expect_identical(names(out), ddict@data$name)
})

test_that("labelDDict: Label columns using a DDict: Use name", {
  ddict <- DDict()

  # important to call it df3 to match the table in dictionary
  df3 <- df_ddict(nm = "df3")
  # cat("\n", "ddict", "\n")
  # print(df3)
  ddict@data <- df_ddict(nm = "ddict3")
  # cat("\n", "ddict", "\n")
  # print(ddict)


  out <- labelDDict(ddict, data = df3, is_raw_nm = FALSE)
  # cat("\n", "out", "\n")
  # print(out)
  out_lbl <- sjlabelled::get_label(out)
  # cat("\n", "out labels", "\n")
  # print(out_lbl)

  target <- c("varInt" = "integer var 1",
              "varIntish" = "integer var 2",
              "varDbl" = "double var 1",
              "varChar" = "character var 1",
              "varDate" = "date var 1",
              "varPOSIXct" = "posix var 1",
              "varFactor" = "factor var 1")
  # cat("\n", "target labels", "\n")
  # print(target)
  expect_identical(out_lbl, target)
})

test_that("labelDDict: Use raw_name = TRUE", {
  ddict <- DDict()

  # important to call it df3 to match the table in dictionary
  df3 <- df_ddict(nm = "df3")
  # cat("\n", "ddict", "\n")
  # print(df3)
  ddict@data <- df_ddict(nm = "ddict3")
  # cat("\n", "ddict", "\n")
  # print(ddict)


  out <- labelDDict(ddict, data = df3, is_raw_nm = TRUE)
  # cat("\n", "out", "\n")
  # print(out)
  out_lbl <- sjlabelled::get_label(out)
  # cat("\n", "out labels", "\n")
  # print(out_lbl)

  target <- c("varInt" = "integer var 1",
              "varIntish" = "integer var 2",
              "varDbl" = "double var 1",
              "varChar" = "character var 1",
              "varDate" = "date var 1",
              "varPOSIXct" = "posix var 1",
              "varFactor" = "factor var 1")
  # cat("\n", "target labels", "\n")
  # print(target)
  expect_identical(out_lbl, target)
})


test_that("labelDDict: Use raw_name = FALSE", {
  ddict <- DDict()

  # important to call it df3 to match the table in dictionary
  df3 <- df_ddict(nm = "df3")
  # cat("\n", "ddict", "\n")
  # print(df3)
  ddict@data <- df_ddict(nm = "ddict3")
  # cat("\n", "ddict", "\n")
  # print(ddict)


  out <- labelDDict(ddict, data = df3, is_raw_nm = FALSE)
  # cat("\n", "out", "\n")
  # print(out)
  out_lbl <- sjlabelled::get_label(out)
  # cat("\n", "out labels", "\n")
  # print(out_lbl)

  target <- c("varInt" = "integer var 1",
              "varIntish" = "integer var 2",
              "varDbl" = "double var 1",
              "varChar" = "character var 1",
              "varDate" = "date var 1",
              "varPOSIXct" = "posix var 1",
              "varFactor" = "factor var 1")
  # cat("\n", "target labels", "\n")
  # print(target)
  expect_identical(out_lbl, target)
})

test_that("labelDDict: No labels", {
  ddict <- DDict()

  # important to call it df3 to match the table in dictionary
  df3 <- df_ddict(nm = "df3")
  # cat("\n", "ddict", "\n")
  # print(df3)
  ddict@data <- df_ddict(nm = "ddict3")
  ddict@data$label <- NA_character_
  # cat("\n", "ddict", "\n")
  # print(ddict)

  expect_error({out <- labelDDict(ddict, data = df3)},
               class = "ValueError",
               regexp = "There are no label to apply")
})
