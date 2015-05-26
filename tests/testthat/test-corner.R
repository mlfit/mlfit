context("Corner cases")

ref_sample <- data.frame(gid=1:3)
ref_sample <- plyr::adply(ref_sample, 1, function(chunk)
  data.frame(iid = seq_len(chunk$gid), n = chunk$gid))

test_that("No controls", {
  problem <- fitting_problem(
    ref_sample,
    field_names = special_field_names("gid", "iid", "n", "N"),
    individual_controls = list(),
    group_controls = list()
  )
  flat <- flatten_ml_fit_problem(problem, verbose = TRUE)
  expect_equal(nrow(flat$ref_sample), 0)
  expect_equal(as.vector(flat$weights %*% flat$reverse_weights_transform), rep(1, 6))
  expect_equal(flat$weights, 3)
})

test_that("Only grand total for group", {
  group_control = data.frame(N=3)
  problem <- fitting_problem(
    ref_sample,
    field_names = special_field_names("gid", "iid", "n", "N"),
    individual_controls = list(),
    group_controls = list(group_control)
  )
  flat <- flatten_ml_fit_problem(problem, verbose = TRUE)
  expect_equal(nrow(flat$ref_sample), 1)
  expect_equal(as.vector(flat$weights %*% flat$reverse_weights_transform), rep(1, 6))
  expect_equal(flat$weights, 3)
})

test_that("Only grand total for individual", {
  individual_control = data.frame(N=3)
  problem <- fitting_problem(
    ref_sample,
    field_names = special_field_names("gid", "iid", "n", "N"),
    individual_controls = list(individual_control),
    group_controls = list()
  )
  flat <- flatten_ml_fit_problem(problem, verbose = TRUE)
  expect_equal(nrow(flat$ref_sample), 1)
  expect_equal(as.vector(flat$weights %*% flat$reverse_weights_transform), rep(1, 6))
  expect_equal(flat$weights, rep(1, 3))
})

test_that("Grand total for individual and group", {
  group_control = data.frame(N=3)
  individual_control = data.frame(N=6)
  problem <- fitting_problem(
    ref_sample,
    field_names = special_field_names("gid", "iid", "n", "N"),
    individual_controls = list(individual_control),
    group_controls = list(group_control)
  )
  flat <- flatten_ml_fit_problem(problem, verbose = TRUE)
  expect_equal(nrow(flat$ref_sample), 2)
  expect_equal(as.vector(flat$weights %*% flat$reverse_weights_transform), rep(1, 6))
  expect_equal(flat$weights, rep(1, 3))
})
