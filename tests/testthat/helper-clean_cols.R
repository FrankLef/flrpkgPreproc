df_clean_cols_uniq <- function(nm) {
  lst <- list()
  lst$data <- data.frame(
    varDate = as.Date(c(
      "2020-01-02", "2020-02-04", "error", "1001-01-02", "2000",
      "2021-06-15", "", "2023-11-31", Sys.Date()
    )),
    varChar = c("", "", NA_character_, "a", "b", "zz", ".", "aa", "bb"),
    varInt = c(1L, NaN, 3L, -Inf, 5L, Inf, 7L, NA_integer_, 9L),
    varDbl = c(NA_real_, 2, 3, 4, 5, 6, 7, 8, 9),
    varSame = "x",
    varEmpty = "",
    varNA = NA
  )

  lst$target <- c("varSame" = 5L, "varEmpty" = 6L, "varNA" = 7L)
  lst[[nm]]
}

df_clean_cols_nzv <- function(nm) {
  lst <- list()
  lst$nrow <- 200L
  lst$nnzv <- 5L
  lst$data <- data.frame(
    var_char = sample(letters, size = lst$nrow, replace = TRUE),
    var_int = sample(1:9, size = lst$nrow, replace = TRUE),
    var_nzv = c(rep.int(1L, times = lst$nnzv), rep.int(2L, times = lst$nrow - lst$nnzv)),
    var_zv = rep.int(1L, times = lst$nrow)
  )

  lst$target <- c("var_nzv" = 3L, "var_zv" = 4L)

  lst[[nm]]
}
