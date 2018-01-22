#' Access to toy examples bundled in this package
#'
#' Returns the paths to all available toy examples, or to a specific toy
#' example.  Load via [readRDS()].
#'
#' @param name Name of the example, default: return all
#' @return A named vector of file system paths.
#'
#' @export
#' @importFrom stats setNames
#' @examples
#' toy_example()
toy_example <- function(name = NULL) {
  root <- system.file("extdata", package="MultiLevelIPF")
  if (is.null(name)) {
    name <- dir(root)
    name <- gsub("[.]rds$", "", name)
  }

  path <- normalizePath(file.path(root, paste0(name, ".rds")), mustWork = TRUE)
  setNames(path, name)
}
