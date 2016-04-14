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
  stopifnot(nrow(flat_ind$ref_sample) == nrow(fitting_problem$refSample))
  stopifnot(all(flat_ind$ref_sample %in% 0:1))

  fitting_problem_group <- fitting_problem(
    ref_sample = fitting_problem$refSample,
    individual_controls = list(),
    group_controls = fitting_problem$controls$group,
    field_names = special_field_names(fitting_problem$fieldNames$groupId,
                                      fitting_problem$fieldNames$groupId,
                                      count = fitting_problem$fieldNames$count)
  )
  flat_group <- as.flat_ml_fit_problem(fitting_problem_group, model_matrix_type = "separate", verbose = verbose)
  stopifnot(nrow(flat_group$ref_sample) ==
              sum(!duplicated(fitting_problem$refSample[[fitting_problem$fieldNames$groupId]])))
  stopifnot(all(flat_group$ref_sample %in% 0:1))

  res <- run_hipf(flat_group, flat_ind, tol, maxiter, verbose)

  message("Done!")
  new_ml_fit_hipf(
    list(
      weights = expand_weights(res$weights, flat_ind),
      success = res$success,
      residuals = res$residuals,
      flat = flat_ind,
      flat_group = flat_group,
      flat_weights = res$weights
    )
  )
}

run_hipf <- function(flat_group, flat_ind, tol, maxiter, verbose) {
  .patch_verbose()

  message("Preparing")

  ind_ref_sample <- flat_ind$ref_sample
  ind_target_values <- flat_ind$target_values
  ind_weights <- flat_ind$weights

  group_ref_sample <- flat_group$ref_sample
  group_target_values <- flat_group$target_values

  weights_transform_ind_to_group <- flat_ind$reverse_weights_transform %*% flat_group$weights_transform
  weights_transform_group_to_ind <- flat_group$reverse_weights_transform %*% flat_ind$weights_transform

  ind_nonzero_row_index <- lapply(
    seq_len(ncol(ind_ref_sample)),
    function(col) which(ind_ref_sample[, col] != 0)
  )

  group_nonzero_row_index <- lapply(
    seq_len(ncol(group_ref_sample)),
    function(col) which(group_ref_sample[, col] != 0)
  )

  message("Start")

  success <- FALSE
  for (iter in seq.int(from = 1L, to = maxiter + 1, by = 1L)) {
    if (iter %% 100 == 0)
      message("Iteration ", iter)

    if (iter > 1) {
      residuals <- ind_weights %*% ind_ref_sample - ind_target_values
      if (all(abs(residuals) < tol)) {
        success <- TRUE
        message("Success")
        break
      }
    }

    for (col in seq_len(ncol(ind_ref_sample))) {
      row_indexes <- ind_nonzero_row_index[[col]]

      valid_weights <- ind_weights[row_indexes]
      current_value <- sum(valid_weights)
      ind_weights[row_indexes] <- valid_weights / current_value * ind_target_values[[col]]
    }

    group_weights <- as.vector(ind_weights %*% weights_transform_ind_to_group)

    # TODO: fix persons per household ratio

    for (col in seq_len(ncol(group_ref_sample))) {
      row_indexes <- group_nonzero_row_index[[col]]

      valid_weights <- group_weights[row_indexes]
      current_value <- sum(valid_weights)
      group_weights[row_indexes] <- valid_weights / current_value * group_target_values[[col]]
    }

    ind_weights <- as.vector(group_weights %*% weights_transform_group_to_ind)
  }

  nlist(
    weights = ind_weights,
    residuals,
    success
  )
}

new_ml_fit_hipf <- make_new(c("ml_fit_hipf", "ml_fit"))
