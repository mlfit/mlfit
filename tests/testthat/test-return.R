algos <- eval(formals(ml_fit)$algorithm)

test_that("success = TRUE", {
  minitoy <- readRDS(toy_example("Tiny"))

  for (algo in algos) {
    fit <- ml_fit(minitoy, algo)
    expect_true(fit$success, info = algo)
    expect_equal(length(fit$residuals), 5, info = algo)
    expect_lt(max(abs(fit$residuals)), 1e-3)
    expect_equal(length(fit$rel_residuals), 5, info = algo)
    expect_lt(max(abs(fit$rel_residuals)), 1e-6)
    expect_equal(length(fit$flat_weighted_values), 5, info = algo)
    expect_equal(length(fit$weights), 23, info = algo)
    expect_equal(length(fit$flat_weights), 8, info = algo)
    expect_equal(fit$tol, 1e-6, info = algo)
    expect_is(fit$iterations, "integer", info = algo)
  }
})

test_that("success = FALSE", {
  bad_problem <- readRDS(toy_example("Conflict"))

  for (algo in algos) {
    fit <- ml_fit(bad_problem, algo)
    expect_false(fit$success, info = algo)
  }
})
