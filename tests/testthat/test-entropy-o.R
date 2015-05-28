context("entropy_o")

test_that("entropy_o algorithm", {
  test_names <- c("minitoy", "toy", "dummytoy", "multitoy", "onetoy", "bitoy")
  test_paths <- system.file(file.path("extdata", test_names), package = "MultiLevelIPF")
  results <- llply(setNames(test_paths, nm=test_names), import_IPAF_results)
  llply(results, function(problem) {
    fit <- ml_fit_entropy_o(problem)
    margins <- compute_margins(problem, fit$weights)
    control_df <- margin_to_df(problem$controls)
    expect_message(margin_df <- margin_to_df(margins, verbose = TRUE), "as count column for")
    expect_equal(control_df[["..count.."]], margin_df[["..count.."]])
    expect_true(all(abs(fit$residuals) < 1e-6))
  })
})
