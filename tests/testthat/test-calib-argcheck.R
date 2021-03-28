context("Argument checks")

test_that("NA values are rejected", {
  N <- 10
  X <- diag(N)
  d <- rep(1, N)
  totals <- rep(1, N)
  expect_error(dss(NA, d, totals), "missing value")
  expect_error(dss(X, NA, totals), "missing value")
  expect_error(dss(X, d, NA), "missing value")
  expect_error(dss(X, d, totals, q = NA), "missing value")
})

test_that("Conformance of dimensions", {
  N <- 10
  X <- diag(N)
  d <- rep(1, N)
  q <- rep(1, N)
  totals <- rep(1, N)
  expect_error(dss(X, d, head(totals, -1)), "not equal to number of columns")
  expect_error(dss(X, d, totals, head(q, -1)), "not equal to number of rows")
  expect_error(dss(X, d, totals, c(Inf, head(q, -1))), "infinite values")
})
