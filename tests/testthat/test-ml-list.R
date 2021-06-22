problem <- readRDS(toy_example("Tiny"))

test_that("make_ml_list", {
  expect_error(make_ml_list(problem, "bad_class"), "Unknown ml_class") 
  expect_equal(length(make_ml_list(problem, "ml_problem")), 1L)
  expect_true(is(make_ml_list(problem, "ml_problem")[[1]], "ml_problem"))
  expect_error(
    make_ml_list(problem, "ml_problem")[[2]], 
    regexp = "subscript out of bounds"
  )

  good_problems <- list(problem, problem)
  expect_equal(
    length(make_ml_list(good_problems, "ml_problem")), 
    length(good_problems)
  )

  bad_problems <- list(problem, NA)
  expect_error(
    make_ml_list(bad_problems, "ml_problem"), 
    regexp = "Not all objects in the list are `ml_problem`."
  )
})


test_that("fit and replicate a ml_problem list", {
  problem_list <- list(problem, problem)
  fits <- ml_fit(problem_list, algorithm = "ipu")
  replicates <- ml_replicate(fits, verbose = TRUE)
  expect_true(is.list(fits))
  expect_true(length(fits) == length(problem_list))
  expect_true(all(sapply(fits, is_ml_fit)))
  expect_true(is.list(replicates))
  expect_true(length(replicates) == length(problem_list))
  expect_true(all(sapply(replicates, is.data.frame)))
})
