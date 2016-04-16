context("return")

test_that("success = TRUE", {
  algos <- eval(formals(ml_fit)$algorithm)
  minitoy <- readRDS(toy_example("minitoy"))

  for (algo in algos) {
    fit <- ml_fit(algo, minitoy)
    expect_true(fit$success, info = algo)
  }
})
