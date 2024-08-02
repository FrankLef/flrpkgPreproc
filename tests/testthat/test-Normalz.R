test_that("Normalz: Instantiate new object.", {
  lst <- df_normalz()
  basis <- lst$basis
  normlz <- Normalz(
    basis = basis, id_vars = lst$id_vars,
    base_var = lst$base_var, scale = lst$scale
  )
  expect_true(S7::S7_inherits(normlz))

  # cat("\n", "normlz", "\n")
  # print(normlz)
  expect_identical(normlz@basis, basis)
  expect_identical(normlz@id_vars, lst$id_vars)
  expect_identical(normlz@base_var, lst$base_var)
  expect_identical(normlz@scale, lst$scale)
})


test_that("Normalz: Validate", {
  lst <- df_normalz()
  basis <- lst$basis

  # invalid basis
  expect_error(
    {
      Normalz(
        basis = data.frame(), id_vars = c("ERROR", "year"),
        base_var = "base_amt", scale = 1e6
      )
    },
    class = "ValueError",
    regexp = "Must have at least 1 rows"
  )


  # invalid variables
  expect_error(
    {
      Normalz(
        basis = basis, id_vars = c("ERROR", "year"),
        base_var = "base_amt", scale = 1e6
      )
    },
    class = "ValueError",
    regexp = "Names must be a permutation of set"
  )
})


test_that("doNormalz", {
  lst <- df_normalz()
  normlz <- Normalz(
    basis = lst$basis, id_vars = lst$id_vars,
    base_var = lst$base_var, scale = lst$scale
  )

  out <- doNormalz(normlz,
    data = lst$data, vars = c("amt1", "amt2"),
    inverse = FALSE, keep = FALSE
  )

  # cat("\n", "out", "\n")
  # print(out)

  target_dim <- dim(lst$data)
  target_dim[2] <- target_dim[2] + 2L
  expect_identical(dim(out), target_dim)
})


test_that("doNormalz: no suffix", {
  lst <- df_normalz()
  normlz <- Normalz(
    basis = lst$basis, id_vars = lst$id_vars,
    base_var = lst$base_var, scale = lst$scale,
    sufx = ""
  )

  out <- doNormalz(normlz,
    data = lst$data, vars = c("amt1", "amt2"),
    inverse = FALSE, keep = FALSE
  )

  # cat("\n", "out", "\n")
  # print(out)

  target_dim <- dim(lst$data)
  target_dim[2] <- target_dim[2]
  expect_identical(dim(out), target_dim)
})

test_that("doNormalz: inverse = TRUE", {
  # testthat::skip("debug")

  lst <- df_normalz()
  normlz <- Normalz(
    basis = lst$basis, id_vars = lst$id_vars,
    base_var = lst$base_var, scale = lst$scale
  )
  # cat("\n", "data", "\n")
  # print(lst$data)


  out <- doNormalz(normlz,
    data = lst$data,
    vars = c("amt1", "amt2"),
    inverse = FALSE, keep = FALSE
  )
  # cat("\n", "out", "\n")
  # print(out)

  inv <- doNormalz(normlz,
    data = out,
    vars = c("amt1_nmz", "amt2_nmz"),
    inverse = TRUE, keep = FALSE
  )
  # cat("\n", "inv", "\n")
  # print(inv)


  expect_identical(sum(is.na(inv$amt1_nmz)), 3L)
  expect_identical(sum(is.na(inv$amt2_nmz)), 3L)
  expect_true(all(lst$data$amt1 == inv$amt1_nmz, na.rm = TRUE))
  expect_true(all(lst$data$amt2 == inv$amt2_nmz, na.rm = TRUE))
})


test_that("doNormalz: keep = TRUE", {
  lst <- df_normalz()
  normlz <- Normalz(
    basis = lst$basis, id_vars = lst$id_vars,
    base_var = lst$base_var, scale = lst$scale
  )

  out <- doNormalz(normlz,
    data = lst$data, vars = c("amt1", "amt2"),
    inverse = FALSE, keep = TRUE
  )
  # cat("\n", "out", "\n")
  # print(out)

  target_nm <- c(
    "group", "year", "amt1", "amt2", "base_amt",
    "amt1_nmz", "amt2_nmz"
  )
  expect_identical(names(out), target_nm)
})
