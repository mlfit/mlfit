test_that("ml_replicate works", {
  path <- toy_example("Tiny")
  problem <- readRDS(path)
  fit <- ml_fit(algorithm = "ipu", fitting_problem = problem)
  for (algo in c("trs", "pp", "round")) {
    syn_pop <- ml_replicate(algorithm = algo, fit)
    expect_true(is.data.frame(syn_pop))
    if (algo == "round") {
      expect_true(nrow(syn_pop) >= sum(trunc(fit$weights)))
    } else {
      expect_true(nrow(syn_pop) >= round(sum(fit$weights)))
    }
    expect_true(ncol(syn_pop) == ncol(fit$flat$fitting_problem$refSample))
  }
})

test_that("integerisation methods", {
  w <- 1:4 + runif(4)
  w_rounded <- round(w)

  expect_length(int_trs(w), 4)
  expect_true(sum(int_trs(w)) >= sum(w_rounded))

  expect_length(int_pp(w), 4)
  expect_true(sum(int_pp(w)) >= sum(w_rounded))

  expect_length(int_round(w), 4)
  expect_true(sum(int_round(w)) >= sum(w_rounded))
})
