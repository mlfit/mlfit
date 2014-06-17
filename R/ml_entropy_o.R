#' Estimate weights using an entropy optimization approach
#'
#'
#'
#' @param path Path to the directory that contains the input/output files
#' @param all_weights Should all weights be loaded, or only the final ones
#'   (default)?
#' @param config_name Name of the main configuration file, defaults to
#'   \code{"config.xml"}
#' @return Named list with the following components:
#' \describe{
#'   \item{\code{refSample}}{The reference sample, a \code{data.frame}.}
#'   \item{\code{controls}}{A named list with two components, \code{individual}
#'   and \code{group}. Each contains a list of controls as \code{data.frame}s.}
#'   \item{\code{fieldNames}}{A named list with the names of special fields.}
#'   \item{\code{algorithms}}{A list of algorithm names.}
#'   \item{\code{weights}}{A named list with weight vectors, one per algorithm.}
#' }
#' @export
