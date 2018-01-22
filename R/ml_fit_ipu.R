#' `ml_fit_ipu()` implements Iterative Proportional Updating.
#'
#' @rdname ml_fit
#' @param diff_tol Tolerance, the algorithm stops when relative difference of
#'   control values between iterations drops below this value
#' @param maxiter Maximum number of iterations.
#' @references Ye, X., K. Konduri, R. M. Pendyala, B. Sana and P. A. Waddell (2009)
#' A methodology to match distributions of both household and person attributes
#' in the generation of synthetic populations, paper presented at the *88th
#' Annual Meeting of the Transportation Research Board*, Washington, D.C.,
#' January 2009.
#'
#' @export
#' @examples
#' ml_fit_ipu(fitting_problem = readRDS(path))
ml_fit_ipu <- function(fitting_problem, diff_tol = 16 * .Machine$double.eps,
                       tol = 1e-6, maxiter = 2000, verbose = FALSE) {
  .patch_verbose()

  flat <- as.flat_ml_fit_problem(fitting_problem, model_matrix_type = "separate", verbose = verbose)
  ipu_res <- run_ipu(flat, tol, diff_tol, maxiter, verbose)

  message("Done!")
  res <- new_ml_fit_ipu(
    list(
      flat = flat,
      flat_weights = ipu_res$weights
    )
  )

  set_weights_success_and_residuals(res, tol, ipu_res$iter)
}

run_ipu <- function(flat, tol, diff_tol, maxiter, verbose) {
  .patch_verbose()

  message("Preparing IPU data")
  ref_sample <- flat$ref_sample
  target_values <- flat$target_values
  prior_weights <- flat$weights

  nonzero_row_index <- lapply(
    seq_len(ncol(ref_sample)),
    function(col) which(ref_sample[, col] != 0)
  )

  nonzero_ref_sample <- lapply(
    seq_len(ncol(ref_sample)),
    function(col) ref_sample[nonzero_row_index[[col]], col ]
  )

  message("Start")

  weights <- prior_weights
  for (iter in seq.int(from = 2L, to = maxiter + 1, by = 1L)) {
    last_weights <- weights
    if (iter %% 100 == 0)
      message("Iteration ", iter)

    for (col in seq_len(ncol(ref_sample))) {
      row_indexes <- nonzero_row_index[[col]]
      ref_sample_entries <- nonzero_ref_sample[[col]]

      valid_weights <- weights[row_indexes]
      current_value <- sum(valid_weights * ref_sample_entries)
      weights[row_indexes] <- valid_weights / current_value * target_values[[col]]
    }

    if (get_success_and_residuals(weights %*% ref_sample, target_values, tol)$success) {
      message("Target tolerance reached in iteration ", iter, ", exiting.")
      break
    }

    if (tol_reached(last_weights, weights, diff_tol)) {
      message("Weights haven't changed in iteration ", iter, ", exiting.")
      break
    }
  }

  nlist(
    weights,
    iter
  )
}

new_ml_fit_ipu <- make_new(c("ml_fit_ipu", "ml_fit"))
