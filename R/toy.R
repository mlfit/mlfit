#' Access to toy examples bundled in this package
#'
#' Returns the paths to all available toy examples, or to a specific toy
#' example.  Load via \code{\link{import_IPAF_results}}.
#'
#' @param name Name of the example, default: return all
#' @return A named vector of file system paths.
#'
#' @export
#' @importFrom stats setNames
toy_example <- function(name = NULL) {
  root <- system.file("extdata", package="MultiLevelIPF")
  if (is.null(name)) {
    name <- dir(root)
    name <- name[file.info(file.path(root, name))$isdir]
  }

  path <- normalizePath(file.path(root, name), mustWork = TRUE)
  setNames(path, name)
}
