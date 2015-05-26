context("Corner cases")

ref_sample <- data.frame(gid=1:3, A=factor("a"))
ref_sample <- plyr::adply(ref_sample, 1, function(chunk)
  data.frame(iid = seq_len(chunk$gid), n = chunk$gid))

test_that("Grand totals only", {
  group_control_grand = data.frame(N=3)
  group_control_dummy = data.frame(N=3, A=factor("a"))
  individual_control_grand = data.frame(N=6)
  individual_control_dummy = data.frame(N=6, A=factor("a"))

  group_control_list <- list(
    list(),
    list(group_control_grand),
    list(group_control_dummy)
  )
  individual_control_list <- list(
    list(),
    list(individual_control_grand),
    list(individual_control_dummy)
  )

  for (group_controls in group_control_list) {
    for (individual_controls in individual_control_list) {
      problem <- fitting_problem(
        ref_sample,
        field_names = special_field_names("gid", "iid", "n", "N"),
        individual_controls = individual_controls,
        group_controls = group_controls
      )

      base_weight <- if (length(individual_controls) > 0 && length(group_controls) == 0) 2 else 1
      weights <- if (length(individual_controls) > 0) {
        if (length(group_controls) > 0) rep(1, 3) else rep(2, 3)
      } else 3

      flat <- flatten_ml_fit_problem(problem, verbose = TRUE)
      expect_equal(nrow(flat$ref_sample), length(individual_controls) + length(group_controls))
      expect_equal(as.vector(flat$weights %*% flat$reverse_weights_transform), rep(base_weight, 6))
      expect_equal(flat$weights, weights)
    }
  }
})
