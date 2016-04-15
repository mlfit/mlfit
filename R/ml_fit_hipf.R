#' Estimate weights using Hierarchical Iterative Proportional Fitting
#'
#' This function reweights a reference sample to match constraints given by
#' aggregate controls by means of Hierarchical Iterative Proportional Fitting.
#'
#' @inheritParams ml_fit
#' @inheritParams ml_fit_ipu
#' @return An object of classes \code{ml_fit_hipf} and \code{ml_fit}.
#'
#' @references
#'    Müller, K. and Axhausen, K. W. (2011), Hierarchical IPF: Generating a
#'        synthetic population for Switzerland, paper presented at the 51st
#'        Congress of the European Regional Science Association, University of
#'        Barcelona, Barcelona.
#' @export
#' @examples
#' path <- toy_example("minitoy")
#' ml_fit_hipf(fitting_problem = readRDS(path))
ml_fit_hipf <- function(fitting_problem, tol = 1e-6, maxiter = 500, verbose = FALSE) {
  .patch_verbose()

  group_ind_totals <- get_group_ind_totals(fitting_problem, verbose)
  flat_ind <- create_flat_ind(fitting_problem, verbose)
  flat_group <- create_flat_group(fitting_problem, verbose)
  res <- run_hipf(flat_group, flat_ind, group_ind_totals, tol, maxiter, verbose)

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

get_group_ind_totals <- function(fitting_problem, verbose) {
  if (length(fitting_problem$controls$individual) == 0L ||
      length(fitting_problem$controls$group) == 0L) {
    stop("Need at least one control at both individual and group levels for HIPF.", call. = FALSE)
  }
  flat <- flatten_ml_fit_problem(fitting_problem)
  list(
    group = flat$target_values[["(Intercept)_g"]],
    ind = flat$target_values[["(Intercept)_i"]]
  )
}

create_flat_ind <- function(fitting_problem, verbose) {
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

  flat_ind
}

create_flat_group <- function(fitting_problem, verbose) {
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

  flat_group
}

run_hipf <- function(flat_group, flat_ind, group_ind_totals, tol, maxiter, verbose) {
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

  weights_transform_group_to_groupsize <- get_transform_group_to_groupsize(
    flat_group$reverse_weights_transform)

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

    group_weights <- rescale_group_weights_for_ind_per_group(
      group_weights, weights_transform_group_to_groupsize, group_ind_totals)

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
    residuals = ind_weights %*% ind_ref_sample - ind_target_values,
    success
  )
}

get_transform_group_to_groupsize <- function(group_reverse_weights_transform) {
  group_sizes <- Matrix::rowSums(group_reverse_weights_transform)
  Matrix::sparseMatrix(
    i = seq_along(group_sizes),
    j = group_sizes,
    x = 1)
}

rescale_group_weights_for_ind_per_group <- function(
  group_weights, weights_transform_group_to_groupsize, group_ind_totals) {

  # Appendix A
  Fp <- group_weights %*% weights_transform_group_to_groupsize
  ap <- as.vector(Fp) * (seq_along(Fp) * (group_ind_totals$group / group_ind_totals$ind) - 1)
  dx <- polyroot(remove_leading_zeros(ap))
  d <- Re(dx[abs(Im(dx)) < sqrt(.Machine$double.eps)])
  d <- d[d > 0]
  stopifnot(length(d) == 1L)

  dp <- d ** seq_along(Fp)
  c <- group_ind_totals$group / sum(Fp * dp)

  fhprime_by_fh <- c * dp

  group_weights <- group_weights * as.vector(
    Matrix::tcrossprod(fhprime_by_fh, weights_transform_group_to_groupsize))

  stopifnot(abs(sum(group_weights) / group_ind_totals$group - 1) < 1e-6)
  stopifnot(abs(sum(as.vector(group_weights %*% weights_transform_group_to_groupsize) * seq_along(Fp)) / group_ind_totals$ind - 1) < 1e-6)

  group_weights
}



# S3 ----------------------------------------------------------------------

new_ml_fit_hipf <- make_new(c("ml_fit_hipf", "ml_fit"))