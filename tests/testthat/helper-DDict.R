df_ddict <- function(nm) {
  lst <- list()
  lst <- within(lst, {
    df1n <- 5L
    set.seed(2027L)
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
      raw_dtype = c(
        "integer", "numeric", "numeric", "character", "Date",
        "POSIXct", "factor"
      ),
      dtype = c(
        "integer", "numeric", "numeric", "character", "Date",
        "POSIXct", "factor"
      ),
      role = NA_character_,
      process = NA_character_,
      rule = NA_character_,
      desc = NA_character_,
      note = NA_character_
    )
    df2n <- 7L
    set.seed(2039L)
    df2 <- data.frame(
      varInt = sample(4:6, size = df2n, replace = TRUE),
      varIntish = sample(4:6 + 1e-9, size = df2n, replace = TRUE),
      varDbl = sample(c(12.34, 23.45, 56.78), size = df2n, replace = TRUE),
      varChar = sample(letters[4:6],
        size = df2n, prob = c(0.5, 0.3, 0.2),
        replace = TRUE
      ),
      varDate = as.Date(sample(c("2020-01-31", "2021-06-30", "2023-12-31"),
        size = df2n, replace = TRUE
      )),
      varPOSIXct = as.POSIXct("2020-01-31") + seq_len(df2n),
      varFactor = as.factor(paste("Alpha", letters[seq_len(df2n)], sep = "-"))
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
      raw_dtype = c(
        "integer", "numeric", "numeric", "character", "Date",
        "POSIXct", "factor"
      ),
      dtype = c(
        "integer", "numeric", "numeric", "character", "Date",
        "POSIXct", "factor"
      ),
      role = c(
        "role1", "role1", "role2", NA_character_, "role3",
        NA_character_, NA_character_
      ),
      process = c(
        "proc1", "proc2", NA_character_, "proc3", NA_character_,
        "proc_extra", NA_character_
      ),
      rule = c(
        "rule1", NA_character_, "rule2", "rule3", NA_character_,
        NA_character_, "rule_extra"
      ),
      desc = NA_character_,
      note = NA_character_
    )
    df3n <- 9L
    set.seed(2063L)
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
      raw_dtype = c(
        "integer", "numeric", "numeric", "character", "Date",
        "POSIXct", "factor"
      ),
      dtype = c(
        "integer", "integer", "integer", "factor", "ymd",
        "Date", "character"
      ),
      role = c(
        "role1", "role1", "role2", "role2", "role3",
        "role3", "role3"
      ),
      process = c(
        "proc1", "proc2", NA_character_, "proc3", NA_character_,
        "proc_extra", NA_character_
      ),
      rule = c(
        "rule1", NA_character_, "rule2", "rule3", NA_character_,
        NA_character_, "rule_extra"
      ),
      desc = NA_character_,
      note = NA_character_
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
      raw_dtype = c("integer", "character", "Date", "POSIXct", "factor"),
      dtype = c("integer", "character", "Date", "POSIXct", "factor"),
      role = NA_character_,
      process = NA_character_,
      rule = NA_character_,
      desc = NA_character_,
      note = NA_character_
    )
    # one column with invalid name
    ddict2 <- data.frame(
      ERROR = "error",
      raw_name = c("varInt", "varChar", "varDate", "varPOSIXct", "varFactor"),
      name = c("varInt", "varChar", "varDate", "varPOSIXct", "varFactor"),
      label = NA_character_,
      raw_dtype = c("integer", "character", "Date", "POSIXct", "factor"),
      dtype = c("integer", "character", "Date", "POSIXct", "factor"),
      role = NA_character_,
      process = NA_character_,
      rule = NA_character_,
      desc = NA_character_,
      note = NA_character_
    )
    # invalid raw_name
    ddict3 <- data.frame(
      table = "df",
      raw_name = c("varInt", "", "varDate", "varPOSIXct", "varFactor"),
      name = c("integer", "character", "date", "pOSIXct", "factor"),
      label = NA_character_,
      raw_dtype = c("integer", "character", "Date", "POSIXct", "factor"),
      dtype = c("integer", "character", "Date", "POSIXct", "factor"),
      role = NA_character_,
      process = NA_character_,
      rule = NA_character_,
      desc = NA_character_,
      note = NA_character_
    )
    # invalid raw_name
    ddict4 <- data.frame(
      table = "df",
      raw_name = c("varInt", "varChar", "varDate", "varPOSIXct", "varFactor"),
      name = c("integer", "character", NA_character_, "pOSIXct", "factor"),
      label = NA_character_,
      raw_dtype = c("integer", "character", "Date", "POSIXct", "factor"),
      dtype = c("integer", "character", "Date", "POSIXct", "factor"),
      role = NA_character_,
      process = NA_character_,
      rule = NA_character_,
      desc = NA_character_,
      note = NA_character_
    )
  })
  lst[[nm]]
}
