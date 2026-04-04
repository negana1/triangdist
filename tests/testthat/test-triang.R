library(testthat)


# Tests for dtriang
# -------------------------

test_that("dtriang returns 0 outside [min, max]", {
  expect_equal(dtriang(-1, min = 0, max = 1, mode = 0.5), 0)
  expect_equal(dtriang(2, min = 0, max = 1, mode = 0.5), 0)
})

test_that("dtriang returns 0 at the boundaries", {
  expect_equal(dtriang(0, min = 0, max = 1, mode = 0.5), 0)
  expect_equal(dtriang(1, min = 0, max = 1, mode = 0.5), 0)
})

test_that("dtriang returns correct value at mode", {
  expect_equal(dtriang(0.5, min = 0, max = 1, mode = 0.5), 2)
})

test_that("dtriang handles values just below and above mode", {
  expect_true(dtriang(0.49, 0, 1, 0.5) < dtriang(0.5, 0, 1, 0.5))
  expect_true(dtriang(0.51, 0, 1, 0.5) < dtriang(0.5, 0, 1, 0.5))
})

test_that("dtriang left branch is correct", {
  expect_equal(dtriang(0.25, min = 0, max = 1, mode = 0.5), 1)
})

test_that("dtriang right branch is correct", {
  expect_equal(dtriang(0.75, min = 0, max = 1, mode = 0.5), 1)
})

test_that("dtriang log argument works", {
  expect_equal(dtriang(0.5, min = 0, max = 1, mode = 0.5, log = TRUE), log(2))
})

test_that("dtriang is vectorized", {
  result <- dtriang(c(0, 0.25, 0.5, 0.75, 1), min = 0, max = 1, mode = 0.5)
  expect_equal(result, c(0, 1, 2, 1, 0))
})

test_that("dtriang stops with invalid parameters", {
  expect_error(dtriang(0.5, min = 1, max = 0, mode = 0.5))
  expect_error(dtriang(0.5, min = 0, max = 1, mode = 2))
})


# Tests for ptriang
# -------------------------

test_that("ptriang returns 0 at min", {
  expect_equal(ptriang(0, min = 0, max = 1, mode = 0.5), 0)
})

test_that("ptriang returns 1 at max", {
  expect_equal(ptriang(1, min = 0, max = 1, mode = 0.5), 1)
})

test_that("ptriang returns 0 below min", {
  expect_equal(ptriang(-1, min = 0, max = 1, mode = 0.5), 0)
})

test_that("ptriang returns 1 above max", {
  expect_equal(ptriang(2, min = 0, max = 1, mode = 0.5), 1)
})

test_that("ptriang left branch is correct", {
  expect_equal(ptriang(0.25, min = 0, max = 1, mode = 0.5), 0.25^2 / (1 * 0.5))
})

test_that("ptriang right branch is correct", {
  expect_equal(ptriang(0.75, min = 0, max = 1, mode = 0.5),
               1 - 0.25^2 / (1 * 0.5))
})

test_that("ptriang lower.tail = FALSE works", {
  p <- ptriang(0.3, min = 0, max = 1, mode = 0.5)
  expect_equal(
    ptriang(0.3, min = 0, max = 1, mode = 0.5, lower.tail = FALSE),
    1 - p
  )
})

test_that("ptriang is vectorized", {
  result <- ptriang(c(0, 0.5, 1), min = 0, max = 1, mode = 0.5)
  expect_equal(result, c(0, 0.5, 1))
})

test_that("ptriang stops with invalid parameters", {
  expect_error(ptriang(0.5, min = 1, max = 0, mode = 0.5))
  expect_error(ptriang(0.5, min = 0, max = 1, mode = 2))
})

test_that("ptriang and qtriang are inverse", {
  p <- seq(0.1, 0.9, length.out = 10)
  x <- qtriang(p, 0, 1, 0.5)
  expect_equal(ptriang(x, 0, 1, 0.5), p, tolerance = 1e-6)
})


# Tests for qtriang
# -------------------------

test_that("qtriang returns min when p = 0", {
  expect_equal(qtriang(0, min = 0, max = 1, mode = 0.5), 0)
})

test_that("qtriang returns max when p = 1", {
  expect_equal(qtriang(1, min = 0, max = 1, mode = 0.5), 1)
})

test_that("qtriang left branch is correct", {
  expect_equal(qtriang(0.25, min = 0, max = 1, mode = 0.5),
               sqrt(0.25 * 1 * 0.5))
})

test_that("qtriang right branch is correct", {
  expect_equal(qtriang(0.75, min = 0, max = 1, mode = 0.5),
               1 - sqrt(0.25 * 1 * 0.5))
})

test_that("qtriang is inverse of ptriang", {
  x <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  expect_equal(qtriang(ptriang(x, 0, 1, 0.5), 0, 1, 0.5), x)
})

test_that("qtriang is vectorized", {
  result <- qtriang(c(0, 0.5, 1), min = 0, max = 1, mode = 0.5)
  expect_equal(result, c(0, 0.5, 1))
})

test_that("qtriang stops when p is outside [0, 1]", {
  expect_error(qtriang(1.5, min = 0, max = 1, mode = 0.5))
  expect_error(qtriang(-0.1, min = 0, max = 1, mode = 0.5))
})

test_that("qtriang stops with invalid parameters", {
  expect_error(qtriang(0.5, min = 1, max = 0, mode = 0.5))
  expect_error(qtriang(0.5, min = 0, max = 1, mode = 2))
})


# Tests for rtriang
# -------------------------

test_that("rtriang returns correct number of values", {
  expect_length(rtriang(10, min = 0, max = 1, mode = 0.5), 10)
})

test_that("rtriang returns values within [min, max]", {
  set.seed(42)
  x <- rtriang(1000, min = 0, max = 1, mode = 0.5)
  expect_true(all(x >= 0 & x <= 1))
})

test_that("rtriang stops with invalid n", {
  expect_error(rtriang(-1, min = 0, max = 1, mode = 0.5))
  expect_error(rtriang(1.5, min = 0, max = 1, mode = 0.5))
})

test_that("rtriang stops with invalid parameters", {
  expect_error(rtriang(10, min = 1, max = 0, mode = 0.5))
  expect_error(rtriang(10, min = 0, max = 1, mode = 2))
})

test_that("rtriang is reproducible with set.seed", {
  set.seed(123)
  x1 <- rtriang(5, min = 0, max = 1, mode = 0.5)
  set.seed(123)
  x2 <- rtriang(5, min = 0, max = 1, mode = 0.5)
  expect_equal(x1, x2)
})
