test_that("ml_replicate works", {
  path <- toy_example("Tiny")
  problem <- readRDS(path)
  fit <- ml_fit(fitting_problem = problem, algorithm = "ipu")
  for (algo in c("trs", "pp", "round")) {
    syn_pop <- ml_replicate(fit, algorithm = algo)
    number_of_groups <- length(unique(syn_pop[[problem$fieldNames$groupId]]))
    expect_true(is.data.frame(syn_pop))
    expect_gte(number_of_groups, sum(fit$flat_weights))
    expect_true(ncol(syn_pop) == ncol(problem$refSample))
  }
  syn_pop <-
    ml_replicate(fit, algorithm = "round", .keep_original_ids = TRUE)
  id_cols <-
    c(problem$fieldNames$groupId, problem$fieldNames$individualId)
  expect_true(ncol(syn_pop) == ncol(problem$refSample) + 2L)
  expect_true(all(paste0(id_cols, "_old") %in% names(syn_pop)))
})

test_that("integerisation methods", {
  w <- 1:4 + runif(4)
  w_rounded_sum <- round(sum(w))

  expect_length(int_trs(w), 4)
  expect_gte(sum(int_trs(w)), w_rounded_sum)

  expect_length(int_pp(w), 4)
  expect_gte(sum(int_pp(w)), w_rounded_sum)

  expect_length(int_round(w), 4)
  expect_gte(sum(int_round(w)), w_rounded_sum)
})

test_that(".get_int_fnc works", {
  int_trs <- "global_env"
  expect_error(.get_int_fnc("not_exist"), regexp = "object \'int_not_exist\' not found")
  expect_true(is.function(.get_int_fnc("trs")))
})