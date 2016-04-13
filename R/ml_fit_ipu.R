#' Estimate weights using Iterative Proportional Updating
#'
#' This function reweights a reference sample to match constraints given by
#' aggregate controls by means of Iterative Proportional Updating.
#'
#' @inheritParams ml_fit
#' @return An object of classes \code{ml_fit_ipu} and \code{ml_fit}.
#' @references Ye, X., K. Konduri, R. M. Pendyala, B. Sana and P. A. Waddell (2009)
#' A methodology to match distributions of both household and person attributes
#' in the generation of synthetic populations, paper presented at the \emph{88th
#' Annual Meeting of the Transportation Research Board}, Washington, D.C.,
#' January 2009.
#'
#' @export
#' @param tol Tolerance, the algorithm stops when all target values are reached
#'   within this tolerance.
#' @param maxiter Maximum number of iterations.
#' @examples
#' path <- toy_example("minitoy")
#' ml_fit_ipu(fitting_problem = readRDS(path))
#' \dontrun{ml_fit_ipu(fitting_problem = readRDS(path), ginv = solve)}
ml_fit_ipu <- function(fitting_problem, tol = 1e-6, maxiter = 5000, verbose = FALSE) {
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
  success <- FALSE
  for (iter in seq.int(from = 2L, to = maxiter + 1, by = 1L)) {
    if (iter %% 100 == 0)
      message("Iteration ", iter)
    residuals <- weights %*% ref_sample  - target_values
    if (all(abs(residuals) < tol)) {
      success <- TRUE
      message("Success")
      break
    }

    for (col in seq_len(ncol(ref_sample))) {
      row_indexes <- nonzero_row_index[[col]]
      ref_sample_entries <- nonzero_ref_sample[[col]]

      valid_weights <- weights[row_indexes]
      current_value <- sum(valid_weights * ref_sample_entries)
      weights[row_indexes] <- valid_weights / current_value * target_values[[col]]
    }
  }

  nlist(
    weights,
    residuals,
    success
  )
}

new_ml_fit_ipu <- make_new(c("ml_fit_ipu", "ml_fit"))
