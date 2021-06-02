context("Solow-Polasky diversity")

test_that("solow_polasky outputs reasonable values", {
  repls = 10L
  for (r in seq_len(repls)) {
    mu = sample(5:30, size = 1L)
    n = sample(1:10, size = 1L)
    x = matrix(runif(mu * n), nrow = n, ncol = mu)
    SP = solow_polasky(x, theta = 1 + runif(1L))
    checkmate::expect_number(SP, lower = 1, upper = mu)
  }
})

test_that("solow_polasky outputs 1 if all individuals are the same", {
  x = matrix(rep(1, 50), nrow = 2L)
  expect_equal(solow_polasky(x), 1)
})
