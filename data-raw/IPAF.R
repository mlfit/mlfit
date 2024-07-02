#' Imports results from a run of the original Python implementation
#'
#' This helper function imports input, data and results from a run of the
#' original Python implementation into a named list.  The file `config.xml`
#' is read and interpreted.
#'
#' @param path Path to the directory that contains the input/output files
#' @param all_weights Should all weights be loaded, or only the final ones
#'   (default)?
#' @param config_name Name of the main configuration file, defaults to
#'   \code{"config.xml"}
#' @return An object of class \code{IPAF_result}, essentially a named list with
#'   the following components:
#' \describe{
#'   \item{\code{refSample}}{The reference sample, a \code{data.frame}.}
#'   \item{\code{controls}}{A named list with two components, \code{individual}
#'   and \code{group}. Each contains a list of controls as \code{data.frame}s.}
#'   \item{\code{fieldNames}}{A named list with the names of special fields.}
#'   \item{\code{algorithms}}{A list of algorithm names.}
#'   \item{\code{weights}}{A named list with weight vectors, one per algorithm.}
#' }
#' @importFrom plyr llply l_ply d_ply
#' @importFrom stats setNames
#' @export
#' @examples
#' path <- system.file("extdata/minitoy", package = "mlfit")
#' fitting_problem <- import_IPAF_results(path)
#' names(fitting_problem)
#' fitting_problem$controls
import_IPAF_results <- function(path, all_weights = FALSE, config_name = "config.xml") {
  stopifnot(length(path) == 1)

  rl <- function(n) file.path(path, n)

  xml_file <- rl(config_name)
  config <- .xmlToList(XML::xmlTreeParse(xml_file))

  fileExtension <- config$fileExtension
  fx <- function(n) rl(sprintf("%s.%s", n, fileExtension))

  separator <- config$separator
  if (separator == "\\t") {
    separator <- "\t"
  }
  rd <- function(n) read.table(file = fx(n), header = TRUE, sep = separator)

  refSample <- rd(config$refSample)

  fieldNames <- config$fieldNames

  controls <- llply(
    setNames(nm = c("individual", "group")),
    function(type) {
      llply(
        setNames(nm = unlist(config$controls[[type]])),
        function(control) {
          control.df <- rd(control)
          control.columns <- setdiff(colnames(control.df), fieldNames$count)
          for (control.column in setdiff(control.columns, fieldNames$count)) {
            control.df[[control.column]] <- factor(control.df[[control.column]])
          }

          control.df
        }
      )
    }
  )

  control.columns <- llply(
    controls,
    function(control.type) {
      llply(
        control.type,
        function(control) {
          names(control)
        }
      )
    }
  )

  control.columns <- unique(unlist(control.columns))
  for (control.column in setdiff(control.columns, fieldNames$count)) {
    refSample[[control.column]] <- factor(refSample[[control.column]])
  }

  algorithms <- setNames(nm = unlist(config$algorithms))

  weights <- llply(
    algorithms,
    function(algo) {
      subdir_paths <- dir(
        path,
        pattern = glob2rx(sprintf("*-%s", algo)),
        full.names = TRUE
      )
      if (length(subdir_paths) == 0) {
        warning("No results found for algorithm ", algo)
      } else {
        if (length(subdir_paths) > 1) {
          warning(
            "Multiple results found for algorithm ", algo,
            " using ", subdir_paths[[1]]
          )
        }
        fn_rx <- "^.*-weights-([0-9]+).csv$"
        csv_paths <- dir(subdir_paths, pattern = fn_rx, full.names = TRUE)
        csv_numbers <- as.integer(gsub(fn_rx, "\\1", csv_paths))
        names(csv_paths) <- csv_numbers

        if (!all_weights) {
          csv_paths <- csv_paths[which.max(csv_numbers)]
        }

        llply(
          csv_paths,
          function(csv_path) {
            weights_table <- read.csv(csv_path)
            weights <- setNames(weights_table$w, weights_table[[config$fieldNames$individualId]])
            ret_weights <- unname(weights[as.character(refSample[[config$fieldNames$individualId]])])
            if (any(is.na(ret_weights))) {
              warning("Missing weights for algorithm ", algo)
            }
            ret_weights
          }
        )
      }
    }
  )

  new_IPAF_result(
    tibble::lst(refSample, controls, fieldNames, algorithms, weights)
  )
}

new_IPAF_result <- make_new(c("IPAF_result", "ml_problem"))

# nolint start
.xmlToList <- function(node, addAttributes = TRUE, simplify = FALSE) {
  if (is.character(node)) {
    node <- XML::xmlParse(node)
  }
  if (inherits(node, "XMLAbstractDocument")) {
    node <- XML::xmlRoot(node)
  }
  if (any(inherits(node, c("XMLTextNode", "XMLInternalTextNode")))) {
    XML::xmlValue(node)
  } else if (XML::xmlSize(node) == 0) {
    XML::xmlAttrs(node)
  } else {
    tmp <- vals <- (if (simplify) {
      XML::xmlSApply
    } else {
      XML::xmlApply
    })(node, .xmlToList, addAttributes)
    tt <- XML::xmlSApply(node, inherits, c("XMLTextNode", "XMLInternalTextNode"))
    vals[tt] <- (if (simplify) {
      sapply
    } else {
      lapply
    })(vals[tt], function(x) x[[1]])
    if (length(attrs <- XML::xmlAttrs(node)) > 0) {
      if (addAttributes) {
        vals[[".attrs"]] <- attrs
      } else {
        attributes(vals) <- as.list(attrs)
      }
    }
    if (any(tt) && length(vals) == 1 && names(vals) == "text") {
      vals[[1]]
    } else {
      vals
    }
  }
}
# nolint end

raw_ex <- list.dirs("data-raw", recursive = FALSE)
exd <- lapply(raw_ex, import_IPAF_results, all_weights = TRUE)
