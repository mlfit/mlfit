context("Format")

test_that("Test formatting", {
  multitoy <- readRDS(toy_example("multitoy"))
  expect_output_file(print(multitoy), "output/multitoy_problem.txt", update = TRUE)

  flat <- as.flat_ml_fit_problem(multitoy)
  expect_output_file(print(flat), "output/multitoy_flat.txt", update = TRUE)

  result <- ml_fit_dss(multitoy)
  expect_output_file(print(result), "output/multitoy_result.txt", update = TRUE)
})
