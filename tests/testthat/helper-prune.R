df_prune <- function() {
  n <- 9L
  set.seed(1367L)
  data.frame(
    "varChar" = sample(letters[1:3], size = n, replace = TRUE),
    "varInt" = sample(1:3, size = n, replace = TRUE),
    "varLog" = FALSE,
    "prune_id" = "ok"
  ) |>
    dplyr::mutate(varLog = ifelse(varInt == 2, TRUE, FALSE)) |>
    dplyr::mutate(prune_id = ifelse((varChar == "a") & (varInt == 2L), "X", prune_id))
}
