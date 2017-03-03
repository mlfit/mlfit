#' Create an instance of a fitting problem
#'
#' @param ref_sample The reference sample
#' @param controls Control totals, by default initialized from the
#'   `individual_controls` and `group_controls` arguments
#' @param field_names Names of special fields, construct using
#'   [special_field_names()]
#' @param individual_controls,group_controls Control totals at individual
#'   and group level, given as a list of data frames where each data frame
#'   defines a control
#' @param prior_weights Prior (or design) weights at group level; by default
#'   a vector of ones will be used, which corresponds to random sampling of
#'   groups
#' @return An object of class `fitting_problem`, essentially a named list
#'   with the following components:
#' \describe{
#'   \item{`refSample`}{The reference sample, a `data.frame`.}
#'   \item{`controls`}{A named list with two components, `individual`
#'   and `group`. Each contains a list of controls as `data.frame`s.}
#'   \item{`fieldNames`}{A named list with the names of special fields.}
#' }
#' @export
fitting_problem <- function(ref_sample,
                            controls = list(individual = individual_controls,
                                            group = group_controls),
                            field_names,
                            individual_controls, group_controls,
                            prior_weights = NULL) {
  new_fitting_problem(
    list(refSample = ref_sample, controls = controls, fieldNames = field_names,
         priorWeights = prior_weights)
  )
}

new_fitting_problem <- make_new("fitting_problem")

#' @export
#' @rdname fitting_problem
#' @param x An object
is.fitting_problem <- make_is("fitting_problem")

#' @export
#' @rdname fitting_problem
#' @param ... Ignored.
format.fitting_problem <- function(x, ...) {
  c(
    "An object of class fitting_problem",
    "  Reference sample: " %+% nrow(x$refSample) %+% " observations",
    "  Control totals: " %+% length(x$controls$individual) %+%
      " at individual, and " %+% length(x$controls$group) %+% " at group level",
    if (length(x$algorithms) > 0L)
      "  Results for algorithms: " %+% paste(x$algorithms, collapse = ", ")
  )
}

#' @export
#' @rdname fitting_problem
print.fitting_problem <- default_print

#' @param groupId,individualId Name of the column that defines the ID of the
#'   group or the individual
#' @param individualsPerGroup Obsolete.
#' @param count Name of control total column in control tables (use first numeric
#'   column in each control by default).
#'
#' @importFrom kimisc nlist
#' @export
#' @rdname fitting_problem
special_field_names <- function(groupId, individualId, individualsPerGroup = NULL,
                                count = NULL) {
  if (!is.null(individualsPerGroup)) {
    warning("The individualsPerGroup argument is obsolete.", call. = FALSE)
  }
  nlist(groupId, individualId, count)
}
