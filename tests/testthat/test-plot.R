test_that("plot `ml_fit` object", {
  problem <- readRDS(toy_example("Separate-Grouped"))
  fit <- ml_fit(problem, "ipu")
  p_rr <- plot(fit, metric = "rel_residual")
  p_r <- plot(fit, metric = "residual")
  vdiffr::expect_doppelganger("Relative residuals", p_rr)
  vdiffr::expect_doppelganger("Residuals", p_r)
  expect_error(
    plot(fit, metric = "non-existed-metric"),
    regex = "should be one of"
  )
})
