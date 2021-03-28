context("Trivial")

set.seed(20150120)

test_that("Calibrating a unit matrix against a unit vector", {
  N <- 10
  X <- diag(N)
  d <- rep(1, N)
  totals <- rep(1, N)
  for (method in eval(formals(dss)$method)) {
    g <- dss(X, d, totals, method = method)
    expect_equal(g, totals, info = method)
    expect_null(attr(g, "success"))
    expect_equal(length(g), N)
    expect_is(g, "numeric")
  }
})


test_that("Calibrating a unit matrix against a vector with random totals", {
  N <- 12
  X <- diag(N)
  d <- rep(1, N)
  totals <- runif(N)

  for (method in eval(formals(dss)$method)) {
    g <- dss(X, d, totals, method = method)
    expect_equal(g, totals, info = method)
    expect_null(attr(g, "success"))
    expect_equal(length(g), N)
    expect_is(g, "numeric")
  }
})


test_that("Test non-convergence", {
  X <- matrix(rep(1, 4), nrow = 2)
  d <- rep(1, 2)
  totals <- 1:2

  for (method in eval(formals(dss)$method)) {
    expect_warning(
      g <- dss(X, d, totals, method = method),
      "No convergence"
    )
    expect_null(attr(g, "success"))
    expect_equal(length(g), 2)
    expect_is(g, "numeric")
  }
})


test_that("Calibrating a unit matrix against a unit vector with bounds, failing", {
  N <- 10
  X <- diag(1:10)
  d <- rep(1, N)
  totals <- rep(10, N)
  bounds <- c(0.9, 1.1)

  expect_warning(
    g <- dss(X, d, totals, method = "logit", bounds = bounds),
    "No convergence"
  )
  expect_null(attr(g, "success"))
  expect_equal(length(g), N)
  expect_is(g, "numeric")
})
