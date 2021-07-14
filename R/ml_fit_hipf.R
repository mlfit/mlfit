#' `ml_fit_hipf()` implements Hierarchical Iterative Proportional Fitting.
#'
#' @rdname ml_fit
#'
#' @references
#'    Müller, K. and Axhausen, K. W. (2011), Hierarchical IPF: Generating a
#'        synthetic population for Switzerland, paper presented at the 51st
#'        Congress of the European Regional Science Association, University of
#'        Barcelona, Barcelona.
#' @export
#' @examples
#' ml_fit_hipf(ml_problem = readRDS(path))
ml_fit_hipf <- function(ml_problem, diff_tol = 16 * .Machine$double.eps,
                        tol = 1e-6, maxiter = 2000, verbose = FALSE) {
  .patch_verbose()

  flat <- flatten_ml_fit_problem(ml_problem)
  group_ind_totals <- get_group_ind_totals(flat, verbose)

  flat_ind <- create_flat_ind(ml_problem, verbose)
  flat_group <- create_flat_group(ml_problem, verbose)
  hipf_res <- run_hipf(
    flat, flat_group, flat_ind, group_ind_totals,
    tol, diff_tol, maxiter, verbose
  )

  message("Done!")
  res <- new_ml_fit_hipf(
    list(
      flat_ind = flat_ind,
      flat_group = flat_group
    )
  )

  set_weights_success_and_residuals(
    res,
    flat,
    flat_weights = hipf_res$weights,
    tol,
    iterations = hipf_res$iter
  )
}

get_group_ind_totals <- function(flat, verbose) {
  ret <- list(
    group = flat$target_values[["(Intercept)_g"]],
    ind = flat$target_values[["(Intercept)_i"]]
  )

  if (is.null(ret$group) || is.null(ret$ind)) {
    stop("Need at least one control at both individual and group levels for HIPF.", call. = FALSE)
  }

  ret
}

create_flat_ind <- function(ml_problem, verbose) {
  ml_problem_ind <- ml_problem(
    ref_sample = ml_problem$refSample,
    individual_controls = list(),
    group_controls = ml_problem$controls$individual,
    field_names = special_field_names(
      ml_problem$fieldNames$individualId,
      ml_problem$fieldNames$individualId,
      count = ml_problem$fieldNames$count
    )
  )

  flat_ind <- as_flat_ml_fit_problem(ml_problem_ind, model_matrix_type = "separate", verbose = verbose)

  stopifnot(nrow(flat_ind$ref_sample) == nrow(ml_problem$refSample))
  stopifnot(all(flat_ind$ref_sample@x %in% 0:1))

  flat_ind
}

create_flat_group <- function(ml_problem, verbose) {
  ml_problem_group <- ml_problem(
    ref_sample = ml_problem$refSample,
    individual_controls = list(),
    group_controls = ml_problem$controls$group,
    field_names = special_field_names(
      ml_problem$fieldNames$groupId,
      ml_problem$fieldNames$groupId,
      count = ml_problem$fieldNames$count
    )
  )

  flat_group <- as_flat_ml_fit_problem(ml_problem_group, model_matrix_type = "separate", verbose = verbose)

  stopifnot(all(flat_group$ref_sample@x %in% 0:1))

  flat_group
}

run_hipf <- function(flat, flat_group, flat_ind, group_ind_totals, tol, diff_tol, maxiter, verbose) {
  .patch_verbose()

  message("Preparing")

  ref_sample <- flat$ref_sample
  target_values <- flat$target_values

  ind_ref_sample <- flat_ind$ref_sample
  ind_target_values <- flat_ind$target_values

  group_ref_sample <- flat_group$ref_sample
  group_target_values <- flat_group$target_values
  group_weights <- flat_group$weights

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
    flat_group$reverse_weights_transform
  )

  message("Start")

  for (iter in seq.int(from = 1L, to = maxiter + 1, by = 1L)) {
    last_group_weights <- group_weights
    if (iter %% 100 == 0) {
      message("Iteration ", iter)
    }

    ind_weights <- as.vector(group_weights %*% weights_transform_group_to_ind)

    for (col in seq_len(ncol(ind_ref_sample))) {
      row_indexes <- ind_nonzero_row_index[[col]]

      valid_weights <- ind_weights[row_indexes]
      current_value <- sum(valid_weights)
      ind_weights[row_indexes] <- valid_weights / current_value * ind_target_values[[col]]
    }

    group_weights <- as.vector(ind_weights %*% weights_transform_ind_to_group)

    group_weights <- rescale_group_weights_for_ind_per_group(
      group_weights, weights_transform_group_to_groupsize, group_ind_totals
    )

    for (col in seq_len(ncol(group_ref_sample))) {
      row_indexes <- group_nonzero_row_index[[col]]

      valid_weights <- group_weights[row_indexes]
      current_value <- sum(valid_weights)
      group_weights[row_indexes] <- valid_weights / current_value * group_target_values[[col]]
    }

    if (get_success_and_residuals(group_weights %*% ref_sample, target_values, tol)$success) {
      message("Target tolerance reached in iteration ", iter, ", exiting.")
      break
    }

    if (tol_reached(last_group_weights, group_weights, diff_tol)) {
      message("Weights haven't changed in iteration ", iter, ", exiting.")
      break
    }
  }

  residuals <- group_weights %*% group_ref_sample - group_target_values
  tibble::lst(
    weights = group_weights,
    iter
  )
}

get_transform_group_to_groupsize <- function(group_reverse_weights_transform) {
  group_sizes <- rowSums(group_reverse_weights_transform)
  sparseMatrix(
    i = seq_along(group_sizes),
    j = group_sizes,
    x = 1
  )
}

rescale_group_weights_for_ind_per_group <- function(group_weights, weights_transform_group_to_groupsize, group_ind_totals) {

  # Appendix A
  Fp <- group_weights %*% weights_transform_group_to_groupsize
  ap <- as.vector(Fp) * (seq_along(Fp) * (group_ind_totals$group / group_ind_totals$ind) - 1)
  dx <- polyroot(remove_leading_zeros(ap))
  d <- Re(dx[abs(Im(dx)) < sqrt(.Machine$double.eps)])
  d <- d[d > 0]

  # No root can happen with malformed problems, silently return
  # and let higher-level routine handle this
  if (length(d) != 1L) {
    return(group_weights)
  }

  dp <- d**seq_along(Fp)
  c <- group_ind_totals$group / sum(Fp * dp)

  fhprime_by_fh <- c * dp

  group_weights <- group_weights * as.vector(
    tcrossprod(fhprime_by_fh, weights_transform_group_to_groupsize)
  )

  stopifnot(abs(sum(group_weights) / group_ind_totals$group - 1) < 1e-6)
  stopifnot(abs(sum(as.vector(group_weights %*% weights_transform_group_to_groupsize) * seq_along(Fp)) / group_ind_totals$ind - 1) < 1e-6)

  group_weights
}



# S3 ----------------------------------------------------------------------

new_ml_fit_hipf <- make_new(c("ml_fit_hipf", "ml_fit"))
