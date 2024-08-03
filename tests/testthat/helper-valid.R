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

rng_valid <- function(suffix = c("hb", "madn")) {
  suffix <- match.arg(suffix)

  if (suffix == "hb") {
    out <- list(
      "varNorm" = c("min" = 0.72, "max" = 11.56),
      "varLognorm" = c("min" = 4.46, "max" = 71.32),
      "varExp" = c("min" = 0.99, "max" = 15.88),
      "varBeta" = c("min" = 1.17, "max" = 18.76)
    )
  } else if (suffix == "madn") {
    out <- list(
      "varNorm" = c("min" = 1.73, "max" = 4.05),
      "varLognorm" = c("min" = 5.6, "max" = 30.06),
      "varExp" = c("min" = 0.65, "max" = 7.29),
      "varBeta" = c("min" = 2.53, "max" = 6.85)
    )
  } else {
    msg <- sprintf("%s is an invalid suffix.", suffix)
  }
  out
}

rules_valid <- function(suffix = c("hb", "madn")) {
  if (suffix == "hb") {
    out <- data.frame(
      "name" = c("varNorm_oob", "varLognorm_oob", "varExp_oob", "varBeta_oob"),
      "label" = c("varNorm_oob", "varLognorm_oob", "varExp_oob", "varBeta_oob"),
      "rule" = c(
        "in_range(varNorm, min = 0.72, max = 11.56)",
        "in_range(varLognorm, min = 4.46, max = 71.32)",
        "in_range(varExp, min = 0.99, max = 15.88)",
        "in_range(varBeta, min = 1.17, max = 18.76)"
      )
    )
  } else if (suffix == "madn") {
    out <- data.frame(
      "name" = c("varNorm_oob", "varLognorm_oob", "varExp_oob", "varBeta_oob"),
      "label" = c("varNorm_oob", "varLognorm_oob", "varExp_oob", "varBeta_oob"),
      "rule" = c(
        "in_range(varNorm, min = 1.73, max = 4.05)",
        "in_range(varLognorm, min = 5.6, max = 30.06)",
        "in_range(varExp, min = 0.65, max = 7.29)",
        "in_range(varBeta, min = 2.53, max = 6.85)"
      )
    )
  } else {
    msg <- sprintf("%s is an invalid suffix.", suffix)
  }
  out
}
