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
#' @examples
#' path <- toy_example("minitoy")
#' ml_fit_ipu(fitting_problem = readRDS(path))
#' \dontrun{ml_fit_ipu(fitting_problem = readRDS(path), ginv = solve)}
ml_fit_ipu <- function(fitting_problem, tol = 1e-6, maxiter = 5000, verbose = FALSE) {
  .patch_verbose()

  flat <- if (is.fitting_problem(fitting_problem)) {
    flatten_ml_fit_problem(fitting_problem = fitting_problem, model_matrix_type = "separate", verbose = verbose)
  } else {
    fitting_problem
  }

  weights <- run_ipu(flat, tol, maxiter, verbose)

  weights
}

run_ipu <- function(flat, tol, maxiter, verbose) {
  .patch_verbose()
  ref_sample <- flat$ref_sample
  target_values <- flat$target_values
  weights <- rep(1, ncol(ref_sample))

  nonzero_col_index <- lapply(
    seq_len(nrow(ref_sample)),
    function(row) which(ref_sample[row, ] != 0)
  )

  nonzero_ref_sample <- lapply(
    seq_len(nrow(ref_sample)),
    function(row) ref_sample[row, nonzero_col_index[[row]] ]
  )

  for (iter in seq.int(2L, maxiter + 1)) {
    residuals <- ref_sample %*% weights - target_values
    if (all(abs(residuals) < tol))
      break

    for (row in seq_len(nrow(ref_sample))) {
      col_indexes <- nonzero_col_index[[row]]
      ref_sample_entries <- nonzero_ref_sample[[row]]

      valid_weights <- weights[col_indexes]
      current_value <- valid_weights * ref_sample_entries
      target_value <- current_value / sum(current_value) * target_values[[row]]
      weights[col_indexes] <- target_value / ref_sample_entries
    }
  }

  weights
}

new_ml_fit_ipu <- make_new(c("ml_fit_ipu", "ml_fit"))
