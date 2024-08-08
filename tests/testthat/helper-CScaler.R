df_cscaler <- function() {
  out <- list()
  within(out, {
    n <- 20
    set.seed(1823L)
    data <- data.frame(
      group = sample(letters[1:3], size = n, replace = TRUE),
      year = sample(1:3, size = n, replace = TRUE, prob = c(0.5, 0.4, 0.1)),
      amt1 = round(stats::rnorm(n, mean = 100, sd = 20), 0),
      amt2 = round(stats::rlnorm(n, meanlog = 5, sdlog = 1), 0)
    )
    basis <- data.frame(
      group = c("a", "a", "a", "b", "b", "c", "c", "x", "x"),
      year = c(1, 2, 3, 1, 2, 1, 9, 3, 9),
      base_amt = sample(c(10, 20, 30, 40, 50), size = 9, replace = TRUE)
    )
    id_vars <- c("group", "year")
    base_var <- "base_amt"
    scale <- 100
  })
}
