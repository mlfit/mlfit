#' Estimate weights using a given algorithm
#' 
#' This function reweights a reference sample to match constraints given by
#' aggregate controls using an algorithm given as parameter.
#' 
#' @usage
#' ml_fit(algorithm, ref_sample, controls, field_names)
#' @param algorithm Algorithm to use
#' @param ref_sample Reference sample.  Alternatively, a configuration as
#'   returned by \code{\link{import_IPAF_results}}; in this case, the other
#'   arguments are ignored.
#' @param controls Control totals
#' @param field_names Names of special columns in the data
#' @return An object of class \code{ml_fit}, essentially a named list.
#' @export
ml_fit <- function(algorithm = c("entropy_o"),
                   ref_sample, controls, field_names) {
  algorithm <- match.arg(algorithm)
  fun.name <- sprintf("ml_fit_%s", algorithm)
  if (!exists(fun.name))
    stop("Unknown algorithm:", algorithm)

  get(fun.name)(
    ref_sample=ref_sample, controls=controls, field_names=field_names)
}

.patch_ml_fit_args <- function() {
  config <- get("ref_sample", parent.frame())
  if (inherits(config, "IPAF_result")) {
    export.list(list(ref_sample=config$refSample,
                     controls=config$controls,
                     field_names=config$fieldNames),
                target.env=parent.frame())
  }
}
