df_prune <- function() {
  n <- 9L
  set.seed(1319L)
  data.frame(
    "varChar" = sample(letters[1:3], size = n, replace = TRUE),
    "varInt" = sample(1:3, size = n, prob = c(0.50, 0.30, 0.20), replace = TRUE)
  )
}
