context("Corner cases")

test_that("No controls", {
  ref_sample <- data.frame(gid=1:3, iid=1:3, n=1)
  problem <- fitting_problem(
    ref_sample,
    field_names = special_field_names("gid", "iid", "n", "N"),
    individual_controls = list(),
    group_controls = list()
  )
  flat <- flatten_ml_fit_problem(problem, verbose = TRUE)
  expect_equal(as.vector(flat$weights %*% flat$reverse_weights_transform), rep(1, 3))
  expect_equal(flat$weights, 3)
})

test_that("Only grand total for group", {
  ref_sample <- data.frame(gid=1:3, iid=1:3, n=1)
  group_control = data.frame(N=3)
  problem <- fitting_problem(
    ref_sample,
    field_names = special_field_names("gid", "iid", "n", "N"),
    individual_controls = list(),
    group_controls = list(group_control)
  )
  flat <- flatten_ml_fit_problem(problem, verbose = TRUE)
  expect_equal(as.vector(flat$weights %*% flat$reverse_weights_transform), rep(1, 3))
  expect_equal(flat$weights, 3)
})

test_that("Only grand total for individual", {
  ref_sample <- data.frame(gid=1:3, iid=1:3, n=1)
  individual_control = data.frame(N=3)
  problem <- fitting_problem(
    ref_sample,
    field_names = special_field_names("gid", "iid", "n", "N"),
    individual_controls = list(individual_control),
    group_controls = list()
  )
  flat <- flatten_ml_fit_problem(problem, verbose = TRUE)
  expect_equal(as.vector(flat$weights %*% flat$reverse_weights_transform), rep(1, 3))
  expect_equal(flat$weights, 3)
})
