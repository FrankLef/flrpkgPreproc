test_that("CScaler: Instantiate new object.", {
  lst <- df_cscaler()
  basis <- lst$basis
  cscaler <- CScaler(
    basis = basis, id_vars = lst$id_vars,
    base_var = lst$base_var, scale = lst$scale
  )
  expect_true(S7::S7_inherits(cscaler))

  # cat("\n", "cscaler", "\n")
  # print(cscaler)
  expect_identical(cscaler@basis, basis)
  expect_identical(cscaler@id_vars, lst$id_vars)
  expect_identical(cscaler@base_var, lst$base_var)
  expect_identical(cscaler@scale, lst$scale)
})


test_that("CScaler: Validate", {
  lst <- df_cscaler()
  basis <- lst$basis

  # invalid basis
  expect_error(
    {
      CScaler(
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
      CScaler(
        basis = basis, id_vars = c("ERROR", "year"),
        base_var = "base_amt", scale = 1e6
      )
    },
    class = "ValueError",
    regexp = "Names must be a permutation of set"
  )
})


test_that("CScaler_do", {
  lst <- df_cscaler()
  cscaler <- CScaler(
    basis = lst$basis, id_vars = lst$id_vars,
    base_var = lst$base_var, scale = lst$scale
  )

  out <- CScaler_do(cscaler,
    data = lst$data, vars = c("amt1", "amt2"),
    inverse = FALSE, keep = FALSE
  )

  # cat("\n", "out", "\n")
  # print(out)

  target_dim <- dim(lst$data)
  target_dim[2] <- target_dim[2] + 2L
  expect_identical(dim(out), target_dim)
})


test_that("CScaler_do: no suffix", {
  lst <- df_cscaler()
  cscaler <- CScaler(
    basis = lst$basis, id_vars = lst$id_vars,
    base_var = lst$base_var, scale = lst$scale,
    suffix = ""
  )

  out <- CScaler_do(cscaler,
    data = lst$data, vars = c("amt1", "amt2"),
    inverse = FALSE, keep = FALSE
  )

  # cat("\n", "out", "\n")
  # print(out)

  target_dim <- dim(lst$data)
  target_dim[2] <- target_dim[2]
  expect_identical(dim(out), target_dim)
})

test_that("CScaler_do: inverse = TRUE", {
  # testthat::skip("debug")

  lst <- df_cscaler()
  cscaler <- CScaler(
    basis = lst$basis, id_vars = lst$id_vars,
    base_var = lst$base_var, scale = lst$scale
  )
  # cat("\n", "data", "\n")
  # print(lst$data)


  out <- CScaler_do(cscaler,
    data = lst$data,
    vars = c("amt1", "amt2"),
    inverse = FALSE, keep = FALSE
  )
  # cat("\n", "out", "\n")
  # print(out)

  inv <- CScaler_do(cscaler,
    data = out,
    vars = c("amt1_scl", "amt2_scl"),
    inverse = TRUE, keep = FALSE
  )
  # cat("\n", "inv", "\n")
  # print(inv)


  expect_identical(sum(is.na(inv$amt1_scl)), 3L)
  expect_identical(sum(is.na(inv$amt2_scl)), 3L)
  expect_true(all(lst$data$amt1 == inv$amt1_scl, na.rm = TRUE))
  expect_true(all(lst$data$amt2 == inv$amt2_scl, na.rm = TRUE))
})


test_that("CScaler_do: keep = TRUE", {
  lst <- df_cscaler()
  cscaler <- CScaler(
    basis = lst$basis, id_vars = lst$id_vars,
    base_var = lst$base_var, scale = lst$scale
  )

  out <- CScaler_do(cscaler,
    data = lst$data, vars = c("amt1", "amt2"),
    inverse = FALSE, keep = TRUE
  )
  # cat("\n", "out", "\n")
  # print(out)

  target_nm <- c(
    "group", "year", "amt1", "amt2", "base_amt",
    "amt1_scl", "amt2_scl"
  )
  expect_identical(names(out), target_nm)
})
