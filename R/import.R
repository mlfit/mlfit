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

  require(XML)
  require(plyr)
  require(kimisc)

  rl <- function(n) file.path(path, n)

  xml_file <- rl("config.xml")
  config <- xmlToList(xmlTreeParse(xml_file))

  fileExtension <-  config$fileExtension
  fx <- function(n) rl(sprintf("%s.%s", n, fileExtension))

  separator <- config$separator
  if (separator == "\\t") separator = "\t"
  rd <- function(n) read.table(file=fx(n), header=TRUE, sep=separator)

  refSample <- rd(config$refSample)

  controls <- llply(
    setNames(nm=c("individual", "group")),
    function(type) {
      llply(
        setNames(nm=config$controls[[sprintf("%s.control", type)]]),
        function (control) {
          rd(control)
        }
      )
    }
  )

  fieldNames <- config$fieldNames

  algorithms <- setNames(nm=unlist(config$algorithms))

  weights <- llply(
    algorithms,
    function (algo) {
      subdir_paths <- dir(path, pattern=glob2rx(sprintf("*-%s", algo)),
                          full.names=TRUE)
      if (length(subdir_paths) == 0) {
        warning("No results found for algorithm ", algo)
      } else {
        if (length(subdir_paths) > 1) {
          warning("Multiple results found for algorithm ", algo,
                  " using ", subdir_paths[[1]])
        }
        fn_rx <- "^.*-weights-([0-9]+).csv$"
        csv_paths <- dir(subdir_paths, pattern=fn_rx, full.names=TRUE)
        csv_numbers <- as.integer(gsub(fn_rx, "\\1", csv_paths))
        csv_path <- csv_paths[which.max(csv_numbers)]
        weights <- read.csv(csv_path)
        refSampleNew <- merge(refSample, weights, all.x=TRUE)
        if (any(is.na(refSampleNew$w)))
          warning("Missing weights for algorithm ", algo)
        refSampleNew$w
      }
    }
  )

  nlist(refSample, controls, fieldNames, weights)
}
