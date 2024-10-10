data_clean <- function(nm) {
  df <- data2clean
  ddict <- data2clean_ddict
  out <- list(
    "df" = df,
    "ddict" = ddict
  )
  out[[nm]]
}

get_pos <- function(x, n, seed = NULL) {
  checkmate::assert_integer(n, lower = 1, upper = length(x))
  pos <- which(!is.na(x))
  set.seed(seed)
  sample(pos, size = n, replace = FALSE)
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


