#' Imports results from a run of the original Python implementation
#' 
#' This helper function imports data and results from a run of the original
#' Python implementation into a named list.
#' 
#' @param path Path to the directory that contains the input/output files
#' @return Named list
#' @export
import_IPAF_results <- function(path) {
  stopifnot(length(path) == 1)
  print(dir(path))
}
