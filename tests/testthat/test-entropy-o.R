context("entropy_o")

test_that("entropy_o algorithm", {
  require(plyr)
  require(kimisc)
  test_names <- c("minitoy", "toy", "dummytoy", "multitoy", "onetoy", "bitoy")
  test_paths <- system.file(file.path("extdata", test_names), package = "MultiLevelIPF")
  results <- llply(setNames(test_paths, nm=test_names), import_IPAF_results)
  llply(results, function(problem) {
    fit <- ml_fit_entropy_o(problem)
    margins <- compute_margins(problem, fit$weights)
    control_df <- margin_to_df(problem$controls, problem$fieldNames$count)
    margin_df <- margin_to_df(margins, problem$fieldNames$count)
    expect_equal(control_df[[problem$fieldNames$count]], margin_df[[problem$fieldNames$count]])
    expect_true(all(abs(fit$residuals) < 1e-6))
  })
})
