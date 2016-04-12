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
#' ml_fit_ipu(fitting_problem = readRDS(path))
ml_fit_hipf <- function(fitting_problem, tol = 1e-6, maxiter = 5000, verbose = FALSE) {
  .patch_verbose()

  flat <- as.flat_ml_fit_problem(fitting_problem, model_matrix_type = "separate", verbose = verbose)

  res <- run_ipu(flat, tol, maxiter, verbose)

  message("Done!")
  new_ml_fit_ipu(
    list(
      weights = expand_weights(res$weights, flat),
      success = res$success,
      residuals = res$residuals,
      flat = flat,
      flat_weights = res$weights
    )
  )
}

run_ipu <- function(flat, tol, maxiter, verbose) {
  .patch_verbose()

  message("Preparing IPU data")
  ref_sample <- flat$ref_sample
  target_values <- flat$target_values
  prior_weights <- flat$weights

  nonzero_col_index <- lapply(
    seq_len(nrow(ref_sample)),
    function(row) which(ref_sample[row, ] != 0)
  )

  nonzero_ref_sample <- lapply(
    seq_len(nrow(ref_sample)),
    function(row) ref_sample[row, nonzero_col_index[[row]] ]
  )

  message("Start")

  weights <- prior_weights
  success <- FALSE
  for (iter in seq.int(from = 2L, to = maxiter + 1, by = 1L)) {
    if (iter %% 100 == 0)
      message("Iteration ", iter)
    residuals <- ref_sample %*% weights - target_values
    if (all(abs(residuals) < tol)) {
      success <- TRUE
      message("Success")
      break
    }

    for (row in seq_len(nrow(ref_sample))) {
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

new_ml_fit_ipu <- make_new(c("ml_fit_ipu", "ml_fit"))
