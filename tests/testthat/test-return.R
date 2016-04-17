context("return")

algos <- eval(formals(ml_fit)$algorithm)

test_that("success = TRUE", {
  minitoy <- readRDS(toy_example("minitoy"))

  for (algo in algos) {
    fit <- ml_fit(algo, minitoy)
    expect_true(fit$success, info = algo)
    expect_equal(length(fit$residuals), 5, info = algo)
    expect_lt(max(abs(fit$residuals)), 1e-3)
    expect_equal(length(fit$rel_residuals), 5, info = algo)
    expect_lt(max(abs(fit$rel_residuals)), 1e-6)
    expect_equal(length(fit$flat_weighted_values), 5, info = algo)
    expect_equal(length(fit$weights), 23, info = algo)
    expect_equal(length(fit$flat_weights), 8, info = algo)
    expect_equal(fit$tol, 1e-6, info = algo)
  }
})

test_that("success = FALSE", {
  bad_problem <-
    fitting_problem(
      ref_sample = data_frame(gid = 1:2, iid = 1:2, g = 1, i = 1),
      group_controls = list(data_frame(g = 1, n = 4)),
      individual_controls = list(data_frame(i = 1, n = 3)),
      field_names = special_field_names("gid", "iid", count = "n"))

  for (algo in algos) {
    fit <- ml_fit(algo, bad_problem)
    expect_false(fit$success, info = algo)
  }
})
