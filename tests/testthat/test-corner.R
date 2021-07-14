test_that("Grand totals only", {
  ref_sample_full <- data.frame(gid = 1:3, A = factor("a"))
  ref_sample_full <- adply(ref_sample_full, 1, function(chunk) {
    data.frame(iid = seq_len(chunk$gid), n = chunk$gid)
  })

  ref_sample_one <- data.frame(gid = 1, iid = 1, n = 1, A = factor("a"))

  group_control_grand <- data.frame(N = 3)
  group_control_dummy <- data.frame(N = 3, A = factor("a"))
  individual_control_grand <- data.frame(N = 6)
  individual_control_dummy <- data.frame(N = 6, A = factor("a"))

  ref_sample_list <- list(
    ref_sample_full,
    ref_sample_one
  )
  group_control_list <- list(
    list(),
    list(group_control_grand),
    list(group_control_dummy),
    list(group_control_dummy, group_control_grand)
  )
  individual_control_list <- list(
    list(),
    list(individual_control_grand),
    list(individual_control_dummy),
    list(individual_control_dummy, individual_control_grand)
  )

  for (ref_sample in ref_sample_list) {
    for (group_controls in group_control_list) {
      for (individual_controls in individual_control_list) {
        problem <- ml_problem(
          ref_sample,
          field_names = special_field_names("gid", "iid", count = "N"),
          individual_controls = individual_controls,
          group_controls = group_controls
        )

        cols <- min(length(individual_controls), 1) + min(length(group_controls), 1)

        base_weight <- if (length(individual_controls) > 0 && length(group_controls) == 0) 2 else 1
        if (nrow(ref_sample) == 1 && cols > 0) base_weight <- base_weight * 3 / max(ref_sample$gid)
        orig_weights <- rep(base_weight, nrow(ref_sample))

        weights <- if (nrow(ref_sample) > 1) {
          if (length(individual_controls) > 0) {
            if (length(group_controls) > 0) rep(1, max(ref_sample$gid)) else rep(2, max(ref_sample$gid))
          } else {
            rep(1, max(ref_sample$gid))
          }
        } else {
          if (length(individual_controls) > 0) {
            if (length(group_controls) > 0) 3 else 6
          } else {
            if (length(group_controls) > 0) 3 else 1
          }
        }

        if (length(group_controls) + length(individual_controls) == 0L) {
          expect_error(
            flatten_ml_fit_problem(problem, verbose = TRUE),
            "at least one"
          )
        } else {
          flat <- flatten_ml_fit_problem(problem, verbose = TRUE)
          expect_equal(ncol(flat$ref_sample), cols)
          expect_equal(
            as.vector(flat$weights %*% flat$reverse_weights_transform),
            orig_weights
          )
          expect_equal(flat$weights, weights)
        }
      }
    }
  }
})

test_that("`flat_weights` has same length as no. groups - Issue 60", {
  reference_sample <- tibble::tribble(
        ~HHNR, ~PNR, ~CAR, ~WKSTAT,
        1L,   1L,  "0",     "1",
        1L,   2L,  "0",     "2",
        1L,   3L,  "0",     "3",
        2L,   4L,  "0",     "1",
        2L,   5L,  "0",     "3",
        6L,  15L,  "1",     "1",
        6L,  16L,  "1",     "2",
        8L,  22L,  "2",     "1",
        8L,  23L,  "2",     "2"
    )

    individual_control <- tibble::tribble(
        ~WKSTAT, ~N,
        "1", 91L,
        "2", 65L,
        "3", 104L
    )

    group_control <- tibble::tribble(
        ~CAR, ~N,
        "0", 35L,
        "1", 65L,
        "2", 0L
    )

    problem <- ml_problem(
        ref_sample = reference_sample,
        controls = list(
            individual = list(individual_control),
            group = list(group_control)
        ),
        field_names = special_field_names(
            groupId = "HHNR",
            individualId = "PNR",
            count = "N"
        )
    )

    algos <- eval(formals(ml_fit)$algorithm)
    fits <- lapply(algos, function(x) {ml_fit(problem, algorithm = x)})
    for (fit in fits) {
        expect_equal(
            length(fit$flat_weights), 
            length(unique(fit$flat$ml_problem$refSample))
        )
    }
})