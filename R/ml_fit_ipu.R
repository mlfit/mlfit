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
ml_fit_ipu <- function(fitting_problem,
                       verbose = FALSE) {
  .patch_verbose()

  flat <- if (is.fitting_problem(fitting_problem)) {
    flatten_ml_fit_problem(fitting_problem = fitting_problem, verbose = verbose)
  } else {
    fitting_problem
  }

  message("Calibrating")
  method <- match.arg(method)

  if (!identical(ginv, MASS::ginv)) {
    stop("Currently, only ginv = MASS::ginv is supported.", call. = FALSE)
  }

  g <- sampling::calib(
    Xs = t(flat$ref_sample),
    d = flat$weights,
    total = flat$control_totals,
    method = method,
    max_iter = 50)
  weights.agg <- g * flat$weights

  message("Done!")
  new_ml_fit_dss(
    list(
      weights=expand_weights(weights.agg, flat),
      success=TRUE,
      residuals = (flat$ref_sample %*% weights.agg)[,1] - flat$control_totals,
      flat=flat,
      flat_weights=weights.agg
    )
  )
}

new_ml_fit_dss <- make_new(c("ml_fit_dss", "ml_fit"))
