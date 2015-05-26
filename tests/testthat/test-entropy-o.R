context("entropy_o")

test_that("entropy_o algorithm", {
  require(plyr)
  require(kimisc)
  test_names <- c("minitoy", "toy", "dummytoy", "multitoy", "onetoy", "bitoy")
  test_paths <- system.file(file.path("extdata", test_names), package = "MultiLevelIPF")
  results <- llply(setNames(test_paths, nm=test_names), import_IPAF_results)
  llply(results, function(problem) {
    fit <- ml_fit_entropy_o(problem)
    expect_true(all(abs(fit$residuals) < 1e-6))
  })
})
