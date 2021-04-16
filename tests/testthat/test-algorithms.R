context("algorithms")

test_that("algorithms", {
  test_names <- c("Tiny", "Single", "dummytoy", "Joint-Grouped", "onetoy", "Separate-Grouped")
  test_paths <- toy_example(test_names)
  results <- llply(setNames(test_paths, nm = test_names), readRDS)
  algos <- eval(formals(ml_fit)$algorithm)
  mapply(results, names(results), FUN = function(problem, problem_name) {
    l_ply(algos, function(algo) {
      if (algo == "ipu" && problem_name %in% c("Separate-Grouped")) {
        return()
      }
      if (algo == "hipf" && problem_name %in% c("Joint-Grouped", "onetoy", "Separate-Grouped")) {
        return()
      }
      fit <- ml_fit(problem, algo)
      if (!fit$success) {
        warning("No convergence of ", algo, " for ", problem_name, ".", call. = FALSE)
        return()
      }

      margins <- compute_margins(problem, fit$weights)
      control_df <- margin_to_df(problem$controls)
      expect_message(margin_df <- margin_to_df(margins, verbose = TRUE), "as count column for")
      expect_equal(
        control_df[["..count.."]] / margin_df[["..count.."]], rep(1, nrow(margin_df)),
        tolerance = 1e-6
      )
      expect_true(all(abs(fit$rel_residuals) < 1e-6))
    })
  })
})

test_that("dss and entropy_o give same results", {
  test_names <- c("Tiny", "Single", "dummytoy", "Joint-Grouped", "onetoy", "Separate-Grouped")
  test_paths <- toy_example(test_names)
  results <- llply(setNames(test_paths, nm = test_names), readRDS)
  l_ply(results, function(problem) {
    flat <- flatten_ml_fit_problem(problem)
    fit_entropy_o <- ml_fit_entropy_o(flat)
    fit_dss <- ml_fit_dss(flat)
    expect_lt(max(abs(fit_entropy_o$flat_weights - fit_dss$flat_weights)), 1e-5)
  })
})
