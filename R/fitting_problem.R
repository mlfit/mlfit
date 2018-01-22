#' Create an instance of a fitting problem
#'
#' The `fitting_problem()` function is the first step for fitting a reference
#' sample to known control totals with [MultiLevelIPF].
#' All algorithms (see [ml_fit()]) expect an object created by this function (or
#' optionally processed with [flatten_ml_fit_problem()]).
#'
#' @param ref_sample The reference sample
#' @param controls Control totals, by default initialized from the
#'   `individual_controls` and `group_controls` arguments
#' @param field_names Names of special fields, construct using
#'   `special_field_names()`
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
#' @examples
#' # Create example from Ye et al., 2009
#'
#' # Provide reference sample
#' ye <- tibble::tribble(
#'   ~HHNR, ~APER, ~HH_VAR, ~P_VAR,
#'   1,     3,     1,       1,
#'   1,     3,     1,       2,
#'   1,     3,     1,       3,
#'   2,     2,     1,       1,
#'   2,     2,     1,       3,
#'   3,     3,     1,       1,
#'   3,     3,     1,       1,
#'   3,     3,     1,       2,
#'   4,     3,     2,       1,
#'   4,     3,     2,       3,
#'   4,     3,     2,       3,
#'   5,     3,     2,       2,
#'   5,     3,     2,       2,
#'   5,     3,     2,       3,
#'   6,     2,     2,       1,
#'   6,     2,     2,       2,
#'   7,     5,     2,       1,
#'   7,     5,     2,       1,
#'   7,     5,     2,       2,
#'   7,     5,     2,       3,
#'   7,     5,     2,       3,
#'   8,     2,     2,       1,
#'   8,     2,     2,       2
#' )
#' ye
#'
#' # Specify control at household level
#' ye_hh <- tibble::tribble(
#'   ~HH_VAR, ~N,
#'   1,       35,
#'   2,       65
#' )
#' ye_hh
#'
#' # Specify control at person level
#' ye_ind <- tibble::tribble(
#'   ~P_VAR, ~N,
#'   1,      91,
#'   2,      65,
#'   3,      104
#' )
#' ye_ind
#'
#' ye_problem <- fitting_problem(
#'   ref_sample = ye,
#'   field_names = special_field_names(
#'     groupId = "HHNR", individualId = "PNR", count = "N"
#'   ),
#'   group_controls = list(ye_hh),
#'   individual_controls = list(ye_ind)
#' )
#' ye_problem
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

#' @description
#' The `special_field_names()` function is useful for the `field_names` argument
#' to `fitting_problem`.
#'
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
