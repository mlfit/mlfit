test_that("plot `ml_fit` object", {
  problem <- readRDS(toy_example("Separate-Grouped"))
  fit <- ml_fit(problem, "ipu")
  p_rr <- plot(fit, metric = "rel_residual")
  p_r <- plot(fit, metric = "residual")
  expect_s3_class(p_rr, "ggplot")
  expect_s3_class(p_r, "ggplot")
  expect_error(
    plot(fit, metric = "non-existed-metric"),
    regex = "should be one of"
  )
})
