data_clean <- function(nm) {
  checkmate::assert_data_frame(data2clean, ncols = nrow(data2clean_ddict))
  df <- data2clean
  ddict_cols <- c(
    "table", "raw_name", "name", "label", "raw_dtype", "dtype",
    "role", "process", "rule", "desc", "note"
  )
  ddict <- DDict(data2clean_ddict[, ddict_cols])
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
