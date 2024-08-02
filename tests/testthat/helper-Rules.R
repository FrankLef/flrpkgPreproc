df_rules <- function() {
  lst <- list()
  set.seed(1019)
  df <- data.frame(
    idx = sample(x = letters[1:5], size = 10, replace = TRUE),
    x1 = round(stats::rlnorm(n = 10, meanlog = 1, sdlog = 0.5), 2),
    x2 = round(stats::rlnorm(n = 10, meanlog = 2, sdlog = 1), 2),
    x3 = round(stats::rlnorm(n = 10, meanlog = 3, sdlog = 1.5), 2)
  )
  df
}
