context("(inverted) generational distance")

test_that("calculation of GD/IGD/AHD works as expected", {
  # here all the points in the approximation are located on a line
  # |
  # |  o  (1,5)
  # |    o (2,4)
  # |      o (3,3)
  # |        o (4,2)
  # |          o (5,1)
  # __________________
  x = matrix(
    c(1, 5,
      2, 4,
      3, 3,
      4, 2,
      5, 1),
    nrow = 2L)

  # shift all point by one
  y = x
  y[2L, ] = y[2L, ] + 1

  xgd = gd(x, y, p = 1, modified = TRUE)
  expect_true(is.numeric(xgd))
  expect_equal(xgd, 1)

  # HV with self computed reference point
  expect_equal(gd(x, x), 0)
  expect_equal(igd(x, x), 0)
  expect_equal(ahd(x, x), 0)
})
