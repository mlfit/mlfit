#' Estimate weights using an entropy optimization approach
#' 
#' This function reweights a reference sample to match constraints given by
#' aggregate controls using an entropy optimization method.
#' 
#' @inheritParams ml_fit
#' @return Named list.
#' @export
ml_fit_entropy_o <- function(ref_sample, controls, field_names) {
  .patch_ml_fit_args()
}
