#' Estimate weights using generalized raking
#'
#' This function reweights a reference sample to match constraints given by
#' aggregate controls by means of generalized raking.
#'
#' Internally, \code{grake::\link[grake]{calibWeights}} is called.
#'
#' @inheritParams ml_fit
#' @inheritParams grake::calibWeights
#' @return An object of classes \code{ml_fit_dss} and \code{ml_fit},
#'   essentially a named list.
#' @references Deville, J.-C. and \enc{Särndal}{Saerndal}, C.-E. (1992)
#' Calibration estimators in survey sampling. \emph{Journal of the American
#' Statistical Association}, \bold{87}(418), 376--382.
#'
#' Deville, J.-C., \enc{Särndal}{Saerndal}, C.-E. and Sautory, O. (1993)
#' Generalized raking procedures in survey sampling. \emph{Journal of the
#' American Statistical Association}, \bold{88}(423), 1013--1020.
#'
#' @seealso \code{\link[grake]{calibWeights}}
#' @import grake
#' @export
#' @examples
#' path <- system.file("extdata/minitoy", package="MultiLevelIPF")
#' ml_fit_dss(ref_sample = import_IPAF_results(path))
ml_fit_dss <- function(ref_sample, controls, field_names,
                       method = c("raking", "linear", "logit"),
                       verbose = FALSE)
{
  .patch_ml_fit_args()
  .patch_verbose()

  flat <- flatten_ml_fit_problem(ref_sample = ref_sample, controls = controls,
                                 field_names = field_names, verbose = verbose)

  message("Calibrating")
  g <- grake::calibWeights(X = t(flat$ref_sample), d = flat$weights,
                            totals = flat$control_totals, method = method)
  weights.agg <- g * flat$weights

  weights.ref_sample <- as.vector(weights.agg %*% flat$reverse_weights_transform)

  message("Done!")
  structure(
    list(
      weights=unname(weights.ref_sample),
      success=TRUE,
      residuals=(flat$ref_sample %*% weights.agg)[,1] - flat$control_totals,
      flat=flat,
      flat_weights=weights.agg
    ),
    class=c("ml_fit_dss", "ml_fit")
  )
}
