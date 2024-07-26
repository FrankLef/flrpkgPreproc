df_ddict <- function(nm) {
  lst <- list()
  lst <- within(lst, {
    df1n <- 5L
    df1 <- data.frame(
      varInt = sample(1:3, size = df1n, replace = TRUE),
      varIntish = sample(1:3 + 1e-9, size = df1n, replace = TRUE),
      varDbl = sample(c(1.23, 2.34, 5.67), size = df1n, replace = TRUE),
      varChar = sample(LETTERS[1:3], size = df1n, replace = TRUE),
      varDate = as.Date(sample(c("2020-01-31", "2021-06-30", "2023-12-31"),
        size = df1n, replace = TRUE
      )),
      varPOSIXct = as.POSIXct(sample(c("2020-01-31", "2021-06-30", "2023-12-31"),
        size = df1n, replace = TRUE
      )),
      varFactor = as.factor(sample(letters[1:3], size = df1n, replace = TRUE))
    )
    ddict1 <- data.frame(
      table = "df1",
      raw_name = c(
        "varInt", "varIntish", "varDbl", "varChar", "varDate",
        "varPOSIXct", "varFactor"
      ),
      name = c(
        "varInt", "varIntish", "varDbl", "varChar", "varDate",
        "varPOSIXct", "varFactor"
      ),
      label = NA_character_,
      desc = NA_character_,
      note = NA_character_,
      raw_dtype = c(
        "integer", "numeric", "numeric", "character", "Date",
        "POSIXct", "factor"
      ),
      dtype = c(
        "integer", "numeric", "numeric", "character", "Date",
        "POSIXct", "factor"
      )
    )
    df2n <- 7L
    df2 <- data.frame(
      varInt = sample(4:6, size = df2n, replace = TRUE),
      varIntish = sample(4:6 + 1e-9, size = df2n, replace = TRUE),
      varDbl = sample(c(12.34, 23.45, 56.78), size = df2n, replace = TRUE),
      varChar = sample(letters[4:6], size = df2n, replace = TRUE),
      varDate = as.Date(sample(c("2020-01-31", "2021-06-30", "2023-12-31"),
        size = df2n, replace = TRUE
      )),
      varPOSIXct = as.POSIXct(sample(c("2020-01-31", "2021-06-30", "2023-12-31"),
        size = df2n, replace = TRUE
      )),
      varFactor = as.factor(sample(LETTERS[4:6], size = df2n, replace = TRUE))
    )
    ddict2 <- data.frame(
      table = "df2",
      raw_name = c(
        "varInt", "varIntish", "varDbl", "varChar", "varDate",
        "varPOSIXct", "varFactor"
      ),
      name = c(
        "varInt", "varIntish", "varDbl", "varChar", "varDate",
        "varPOSIXct", "varFactor"
      ),
      label = NA_character_,
      desc = NA_character_,
      note = NA_character_,
      raw_dtype = c(
        "integer", "numeric", "numeric", "character", "Date",
        "POSIXct", "factor"
      ),
      dtype = c(
        "integer", "numeric", "numeric", "character", "Date",
        "POSIXct", "factor"
      )
    )
    df3n <- 9L
    df3 <- data.frame(
      varInt = sample(4:6, size = df3n, replace = TRUE),
      varIntish = sample(4:6 + 1e-9, size = df3n, replace = TRUE),
      varDbl = sample(c(12.34, 23.45, 56.78), size = df3n, replace = TRUE),
      varChar = sample(letters[4:6], size = df3n, replace = TRUE),
      varDate = as.Date(sample(c("2020-01-31", "2021-06-30", "2023-12-31"),
        size = df3n, replace = TRUE
      )),
      varPOSIXct = as.POSIXct(sample(c("2020-01-31", "2021-06-30", "2023-12-31"),
        size = df3n, replace = TRUE
      )),
      varFactor = as.factor(sample(LETTERS[4:6], size = df3n, replace = TRUE))
    )
    ddict3 <- data.frame(
      table = "df3",
      raw_name = c(
        "varInt", "varIntish", "varDbl", "varChar", "varDate",
        "varPOSIXct", "varFactor"
      ),
      name = c("Int", "Intish", "Dbl", "Char", "Date", "POSIXct", "Factor"),
      label = c(
        "integer var 1", "integer var 2", "double var 1",
        "character var 1", "date var 1", "posix var 1",
        "factor var 1"
      ),
      desc = NA_character_,
      note = NA_character_,
      raw_dtype = c(
        "integer", "numeric", "numeric", "character", "Date",
        "POSIXct", "factor"
      ),
      dtype = c(
        "integer", "integer", "integer", "factor", "POSIXct",
        "Date", "character"
      )
    )
  })
  lst[[nm]]
}

df_ddict_err <- function(nm) {
  lst <- list()
  lst <- within(lst, {
    # one column missing
    ddict1 <- data.frame(
      raw_name = c("varInt", "varChar", "varDate", "varPOSIXct", "varFactor"),
      name = c("varInt", "varChar", "varDate", "varPOSIXct", "varFactor"),
      label = NA_character_,
      desc = NA_character_,
      note = NA_character_,
      raw_dtype = c("integer", "character", "Date", "POSIXct", "factor"),
      dtype = c("integer", "character", "Date", "POSIXct", "factor")
    )
    # one column with invalid name
    ddict2 <- data.frame(
      ERROR = "error",
      raw_name = c("varInt", "varChar", "varDate", "varPOSIXct", "varFactor"),
      name = c("varInt", "varChar", "varDate", "varPOSIXct", "varFactor"),
      label = NA_character_,
      desc = NA_character_,
      note = NA_character_,
      raw_dtype = c("integer", "character", "Date", "POSIXct", "factor"),
      dtype = c("integer", "character", "Date", "POSIXct", "factor")
    )
    # invalid raw_name
    ddict3 <- data.frame(
      table = "df",
      raw_name = c("varInt", "", "varDate", "varPOSIXct", "varFactor"),
      name = c("integer", "character", "date", "pOSIXct", "factor"),
      label = NA_character_,
      desc = NA_character_,
      note = NA_character_,
      raw_dtype = c("integer", "character", "Date", "POSIXct", "factor"),
      dtype = c("integer", "character", "Date", "POSIXct", "factor")
    )
    # invalid raw_name
    ddict4 <- data.frame(
      table = "df",
      raw_name = c("varInt", "varChar", "varDate", "varPOSIXct", "varFactor"),
      name = c("integer", "character", NA_character_, "pOSIXct", "factor"),
      label = NA_character_,
      desc = NA_character_,
      note = NA_character_,
      raw_dtype = c("integer", "character", "Date", "POSIXct", "factor"),
      dtype = c("integer", "character", "Date", "POSIXct", "factor")
    )
  })
  lst[[nm]]
}
