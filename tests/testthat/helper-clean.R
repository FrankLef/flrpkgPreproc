data_clean <- function(nm) {
  df <- data2clean
  ddict <- data2clean_ddict
  out <- list(
    "df" = df,
    "ddict" = ddict
  )
  out[[nm]]
}

rand_pos <- function(x, n, seed = NULL) {
  checkmate::assert_integer(n, lower = 1, upper = length(x))
  pos <- which(!is.na(x))
  set.seed(seed)
  sample(pos, size = n, replace = FALSE)
}
