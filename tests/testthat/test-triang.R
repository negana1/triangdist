library(testthat)
library(triangdist)


# dtriang tests
# -------------------------

test_that("dtriang basic properties", {
  expect_equal(dtriang(0, 0, 1, 0.5), 0)
  expect_true(dtriang(0.5, 0, 1, 0.5) > 0)
  expect_equal(dtriang(-1, 0, 1, 0.5), 0)
  expect_equal(dtriang(2, 0, 1, 0.5), 0)
})

test_that("dtriang log works", {
  val <- dtriang(0.5, 0, 1, 0.5)
  expect_equal(dtriang(0.5, 0, 1, 0.5, log = TRUE), log(val))
})
