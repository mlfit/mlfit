context("entropy_o")

test_that("entropy_o algorithm", {
  test_names <- c("minitoy", "toy", "dummytoy", "multitoy", "onetoy", "bitoy")
  test_paths <- toy_example(test_names)
  results <- llply(setNames(test_paths, nm=test_names), readRDS)
  llply(results, function(problem) {
    flat <- flatten_ml_fit_problem(problem)
    fit <- ml_fit_entropy_o(flat)
    fit_dss <- ml_fit_dss(flat)
    expect_lt(max(abs(fit$flat_weights - fit_dss$flat_weights)), 1e-5)

    margins <- compute_margins(problem, fit$weights)
    control_df <- margin_to_df(problem$controls)
    expect_message(margin_df <- margin_to_df(margins, verbose = TRUE), "as count column for")
    expect_equal(control_df[["..count.."]], margin_df[["..count.."]])
    expect_true(all(abs(fit$residuals) < 1e-6))
  })
})
