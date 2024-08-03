df_valid <- function() {
  set.seed(1019)
  n <- 25L
  nhrdl <- 5L
  df <- data.frame(
    cat1 = sample(x = letters[1:5], size = n, replace = TRUE),
    cat2 = sample(x = LETTERS[22:26], size = n, replace = TRUE),
    varNorm = stats::rnorm(n = n, mean = 3, sd = 0.5),
    varLognorm = stats::rlnorm(n = n, meanlog = 3, sdlog = 0.5),
    varExp = c(
      stats::rnorm(n = n - nhrdl, mean = 4, sd = 1),
      stats::rexp(n = nhrdl, rate = 2)
    ),
    varBeta = c(
      stats::rnorm(n = n - nhrdl, mean = 5, sd = 1),
      stats::rbeta(n = nhrdl, shape1 = 1, shape2 = 3)
    )
  )

  df |>
    dplyr::mutate(dplyr::across(.cols = where(is.numeric), .fns = \(x) round(x, 2)))
}
