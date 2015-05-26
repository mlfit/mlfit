#' Create an instance of a fitting problem
#'
#' @param ref_sample The reference sample
#' @param controls Control totals, by default initialized from the
#'   \code{individual_controls} and \code{group_controls} arguments
#' @param field_names Names of special fields, construct using
#'   \code{\link{special_field_names}}
#' @param individual_controls,group_controls Control totals at individual
#'   and group level, given as a list of data frames where each data frame
#'   defines a control
#' @param prior_weights Prior (or design) weights
#' @return An object of class \code{fitting_problem}, essentially a named list
#'   with the following components:
#' \describe{
#'   \item{\code{refSample}}{The reference sample, a \code{data.frame}.}
#'   \item{\code{controls}}{A named list with two components, \code{individual}
#'   and \code{group}. Each contains a list of controls as \code{data.frame}s.}
#'   \item{\code{fieldNames}}{A named list with the names of special fields.}
#' }
#' @export
fitting_problem <- function(ref_sample,
                            controls = list(individual = individual_controls,
                                            group = group_controls),
                            field_names,
                            individual_controls, group_controls,
                            prior_weights = NULL)
{
  structure(
    list(refSample = ref_sample, controls = controls, fieldNames = field_names,
         priorWeights = prior_weights),
    class = c("fitting_problem")
  )
}

#' @export
#' @rdname fitting_problem
#' @param x An object
is.fitting_problem <- function(x) inherits(x, "fitting_problem")

#' @param groupId,individualId Name of the column that defines the ID of the
#'   group or the individual
#' @param individualsPerGroup Name of a column that contains the number
#'   of persons in the group
#' @param count Name of control total column in control tables
#'
#' @export
#' @rdname fitting_problem
special_field_names <- function(groupId, individualId, individualsPerGroup, count) {
  kimisc::nlist(groupId, individualId, individualsPerGroup, count)
}
