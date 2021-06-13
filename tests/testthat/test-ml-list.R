test_that("mlfit a ml_problem list", {
  problem <- readRDS(toy_example("Tiny"))
  problem_list <- list(problem, problem)
  fits <- ml_fit(problem_list, algorithm = "ipu")
  expect_true(is.list(fits))
  expect_true(length(fits) == length(problem_list))
  expect_true(all(sapply(fits, is.ml_fit)))
})
