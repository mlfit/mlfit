test_that("trs works", {
  fp <- readRDS(toy_example("Joint"))
  fit <- ml_fit_ipu(fp)

  # ml_fit
  set.seed(1)
  ml_fit_pop <-
    trs(x = fit)

  # data.frame
  set.seed(1)
  df_pop <-
    trs(
      x = fit$flat$fitting_problem$refSample,
      w = fit$weights,
      hid_col = fit$flat$fitting_problem$fieldNames$groupId,
      pid_col = fit$flat$fitting_problem$fieldNames$individualId,
      verbose = TRUE
    )

  expect_true(all(dim(df_pop) == dim(ml_fit_pop)))

})

test_that("trs with integer weights works", {
  fp <- readRDS(toy_example("Joint"))
  fit <- ml_fit_ipu(fp)

  set.seed(1)
  df_pop <-
    trs(
      x = fit$flat$fitting_problem$refSample,
      w = rep(1, length(fit$weights)),
      hid_col = fit$flat$fitting_problem$fieldNames$groupId,
      pid_col = fit$flat$fitting_problem$fieldNames$individualId,
      verbose = TRUE
    )

  expect_true(nrow(df_pop) == length(fit$weights))

})


test_that("assign_ids works", {
  fp <- readRDS(toy_example("Joint"))
  fit <- ml_fit_ipu(fp)
  ml_fit_pop <-
    trs(x = fit) %>%
    assign_ids(., hid_col = "HHNR", pid_col = "PNR", verbose = T)
  expect_true(ml_fit_pop[, uniqueN(pid)] == ml_fit_pop[, .N])
})
