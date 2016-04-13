#' Estimate weights using Hierarchical Iterative Proportional Fitting
#'
#' This function reweights a reference sample to match constraints given by
#' aggregate controls by means of Hierarchical Iterative Proportional Fitting.
#'
#' @inheritParams ml_fit_ipu
#' @return An object of classes \code{ml_fit_hipf} and \code{ml_fit}.
#'
#' @export
#' @examples
#' path <- toy_example("minitoy")
#' ml_fit_hipf(fitting_problem = readRDS(path))
ml_fit_hipf <- function(fitting_problem, tol = 1e-6, maxiter = 5000, verbose = FALSE) {
  .patch_verbose()

  fitting_problem_ind <- fitting_problem(
    ref_sample = fitting_problem$refSample,
    individual_controls = list(),
    group_controls = fitting_problem$controls$individual,
    field_names = special_field_names(fitting_problem$fieldNames$individualId,
                                      fitting_problem$fieldNames$individualId,
                                      count = fitting_problem$fieldNames$count)
  )
  flat_ind <- as.flat_ml_fit_problem(fitting_problem_ind, model_matrix_type = "separate", verbose = verbose)
  stopifnot(ncol(flat_ind$ref_sample) == nrow(fitting_problem$refSample))

  fitting_problem_group <- fitting_problem(
    ref_sample = fitting_problem$refSample,
    individual_controls = list(),
    group_controls = fitting_problem$controls$group,
    field_names = special_field_names(fitting_problem$fieldNames$groupId,
                                      fitting_problem$fieldNames$groupId,
                                      count = fitting_problem$fieldNames$count)
  )
  flat_group <- as.flat_ml_fit_problem(fitting_problem_group, model_matrix_type = "separate", verbose = verbose)

  res <- run_hipf(flat_group, flat_ind, tol, maxiter, verbose)

  message("Done!")
  new_ml_fit_hipf(
    list(
      weights = expand_weights(res$weights, flat),
      success = res$success,
      residuals = res$residuals,
      flat = flat,
      flat_weights = res$weights
    )
  )
}

run_ipu <- function(flat_group, flat_individual, tol, maxiter, verbose) {
  .patch_verbose()

  ind_ref_sample <- flat_ind$ref_sample
  ind_target_values <- flat_ind$target_values
  ind_weights <- flat_ind$weights

  group_ref_sample <- flat_group$ref_sample
  group_target_values <- flat_group$target_values

  message("Start")

  success <- FALSE
  for (iter in seq.int(from = 1L, to = maxiter + 1, by = 1L)) {
    if (iter %% 100 == 0)
      message("Iteration ", iter)

    if (iter > 1) {
      residuals <- ind_ref_sample %*% weights - target_values
      if (all(abs(residuals) < tol)) {
        success <- TRUE
        message("Success")
        break
      }
    }

    for (row in seq_len(nrow(ind_ref_sample))) {
      col_indexes <- nonzero_col_index[[row]]
      ref_sample_entries <- nonzero_ref_sample[[row]]

      valid_weights <- weights[col_indexes]
      current_value <- sum(valid_weights * ref_sample_entries)
      weights[col_indexes] <- valid_weights / current_value * target_values[[row]]
    }
  }

  nlist(
    weights,
    residuals,
    success
  )
}

new_ml_fit_hipf <- make_new(c("ml_fit_hipf", "ml_fit"))
