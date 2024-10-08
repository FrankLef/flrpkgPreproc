get_preproc <- function(nm) {
  checkmate::assert_names(nm)
  fn <- system.file(
    "extdata", "preproc.xlsx",
    package = "flrpkgPreproc", mustWork = TRUE
  )
  df <- readxl::read_xlsx(
    path = fn,
    sheet = "data")
  ddict <- readxl::read_xlsx(
    path = fn,
    sheet = "ddict")
  ddict <- ddict |>
    filter(table == "data1")
  df <- get_preproc_cast(df, ddict)
  out <- list(
    "df" = df,
    "ddict" = ddict
  )
  out[[nm]]
}

get_preproc_cast <- function(data, ddict) {
  checkmate::assert_names(names(data),identical.to = ddict$raw_name)
  n <- 0L
  for (nm in names(data)) {
    x <- data[, nm, drop = TRUE]
    dtype <- class(x)[1]
    raw_dtype <- ddict$raw_dtype[ddict$raw_name == nm]
    if (dtype != raw_dtype) {
      data[, nm] <- switch(
        raw_dtype,
        "integer" = as.integer(x),
        "numeric" = as.numeric(x),
        "logical" = as.logical(x),
        "Date" = as.Date(x),
        "POSIXct" = as.POSIXct(x),
        "character" = as.character(x),
        stop(sprintf("\"%s\" is an invalid raw data type.", raw_dtype))
        )
      # msg <- sprintf("\n\"%s\" was modified.\n", nm)
      # message(msg)
      n <- n + 1L
    }
  }
  msg <- sprintf("\n%d data types were modified to fit ddict.\n", n)
  message(msg)
  data
}

df_clean <- function(nm) {
  lst <- list()
  lst$n <- 9L
  lst$seed <- 139L

  set.seed(lst$seed)
  lst$data <- data.frame(
    int_ok = sample(1:9, size = lst$n, replace = TRUE),
    char_ok = sample(letters[1:9], size = lst$n, replace = TRUE),
    char1 = c("", "  ", NA_character_, "a", "a.a", ".", ", ", "b, b", "b,  b"),
    char2 = c("NA", " _NA", NA_character_, "n/a", "N/A", "  N\\A", " n \ a ", "a  a", "bb"),
    int1 = c(1L, NaN, 3L, -Inf, 5L, Inf, 7L, NA_integer_, 9L),
    dbl1 = c(NaN, 2.3, -Inf, NA_real_, Inf, 6L, 7L, 8L, 9L),
    char_uniq = "x",
    char_empty = ""
  )

  set.seed(lst$seed)
  lst$target <- data.frame(
    int_ok = sample(1:9, size = lst$n, replace = TRUE),
    char_ok = sample(letters[1:9], size = lst$n, replace = TRUE),
    char1 = c(
      NA_character_, NA_character_, NA_character_, "a", "a.a",
      NA_character_, NA_character_, "b, b", "b, b"
    ),
    char2 = c(
      "NA", NA_character_, NA_character_, NA_character_,
      NA_character_, NA_character_, "n a", "a a", "bb"
    ),
    int1 = c(1L, NaN, 3L, NA_integer_, 5L, NA_integer_, 7L, NA_integer_, 9L),
    dbl1 = c(NaN, 2.3, NA_real_, NA_real_, NA_real_, 6L, 7L, 8L, 9L),
    char_uniq = "x",
    char_empty = NA_character_
  )
  lst[[nm]]
}


