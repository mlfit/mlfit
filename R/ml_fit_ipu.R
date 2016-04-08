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
ml_fit_ipu <- function(fitting_problem, verbose = FALSE) {
  .patch_verbose()

  .check_is_fitting_problem(fitting_problem)

  flat_ipu <- flatten_for_ipu(fitting_problem)

  ref_sample <- fitting_problem$refSample
  controls <-

  browser()
}

#' @import dplyr
flatten_for_ipu <- function(fitting_problem) {
  browser()
  fitting_problem$ref
}
