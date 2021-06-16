test_that("Calibrating a unit matrix against a unit vector, with attributes", {
  N <- 10
  X <- diag(N)
  d <- rep(1, N)
  totals <- rep(1, N)
  for (method in eval(formals(dss)$method)) {
    g <- dss(X, d, totals, method = method, attributes = TRUE)
    expect_equal(as.numeric(g), totals, info = method)
    expect_true(attr(g, "success"))
    expect_equal(length(g), N)
    expect_is(g, "numeric")
    expect_is(attr(g, "iterations"), "integer")
    expect_lt(attr(g, "iterations"), 3)
    expect_equal(attr(g, "method"), method)
    if (method == "logit") {
      expect_equal(attr(g, "bounds"), c(0, 10))
    } else {
      expect_null(attr(g, "bounds"))
    }
  }
})

test_that("Test non-convergence, with attributes", {
  X <- matrix(rep(1, 4), nrow = 2)
  d <- rep(1, 2)
  totals <- 1:2

  for (method in eval(formals(dss)$method)) {
    expect_warning(
      g <- dss(X, d, totals, method = method, attributes = TRUE),
      NA
    )
    expect_false(attr(g, "success"))
    expect_equal(length(g), 2)
    expect_is(g, "numeric")
    expect_is(attr(g, "iterations"), "integer")
  }
})
