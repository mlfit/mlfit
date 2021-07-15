#' Create an instance of a fitting problem
#'
#' The `ml_problem()` function is the first step for fitting a reference
#' sample to known control totals with [mlfit].
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
#' @param geo_hierarchy A table shows mapping between a larger zoning level to
#'  many zones of a smaller zoning level. The column name of the larger level
#'  should be specified in `field_names` as 'region' and the smaller one as
#'  'zone'.
#' @return An object of class `ml_problem` or a list of them if `geo_hierarchy`
#'  was given, essentially a named list with the following components:
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
#'   ~HHNR, ~PNR, ~APER, ~HH_VAR, ~P_VAR,
#'   1, 1, 3, 1, 1,
#'   1, 2, 3, 1, 2,
#'   1, 3, 3, 1, 3,
#'   2, 4, 2, 1, 1,
#'   2, 5, 2, 1, 3,
#'   3, 6, 3, 1, 1,
#'   3, 7, 3, 1, 1,
#'   3, 8, 3, 1, 2,
#'   4, 9, 3, 2, 1,
#'   4, 10, 3, 2, 3,
#'   4, 11, 3, 2, 3,
#'   5, 12, 3, 2, 2,
#'   5, 13, 3, 2, 2,
#'   5, 14, 3, 2, 3,
#'   6, 15, 2, 2, 1,
#'   6, 16, 2, 2, 2,
#'   7, 17, 5, 2, 1,
#'   7, 18, 5, 2, 1,
#'   7, 19, 5, 2, 2,
#'   7, 20, 5, 2, 3,
#'   7, 21, 5, 2, 3,
#'   8, 22, 2, 2, 1,
#'   8, 23, 2, 2, 2
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
#'   1, 91,
#'   2, 65,
#'   3, 104
#' )
#' ye_ind
#'
#' ye_problem <- ml_problem(
#'   ref_sample = ye,
#'   field_names = special_field_names(
#'     groupId = "HHNR", individualId = "PNR", count = "N"
#'   ),
#'   group_controls = list(ye_hh),
#'   individual_controls = list(ye_ind)
#' )
#' ye_problem
#'
#' fit <- ml_fit_dss(ye_problem)
#' fit$weights
ml_problem <- function(ref_sample,
                       controls = list(
                         individual = individual_controls,
                         group = group_controls
                       ),
                       field_names,
                       individual_controls, group_controls,
                       prior_weights = NULL,
                       geo_hierarchy = NULL) {
  if (!is.null(geo_hierarchy)) {
    message("Creating a list of fitting problems by zone")
    return(ml_problem_by_zone(
      ref_sample,
      controls,
      field_names,
      prior_weights,
      geo_hierarchy
    ))
  }

  new_ml_problem(
    list(
      refSample = ref_sample, controls = controls, fieldNames = field_names,
      priorWeights = prior_weights
    )
  )
}

ml_problem_by_zone <- function(ref_sample,
                               controls,
                               field_names,
                               prior_weights = NULL,
                               geo_hierarchy) {
  if (is.null(field_names$region)) {
    stop("field_names$zone is not specified.")
  }
  if (is.null(field_names$zone)) {
    stop("field_names$zone is not specified.")
  }
  if (!field_names$zone %in% names(geo_hierarchy)) {
    stop(sprintf("`zone` field {%s} is not in `geo_hierarchy`", field_names$zone))
  }
  if (!field_names$region %in% names(geo_hierarchy)) {
    stop(sprintf("`region` field {%s} is not in `geo_hierarchy`", field_names$region))
  }
  if (!field_names$region %in% names(ref_sample)) {
    stop(sprintf("`region` field {%s} is not in `ref_sample`", field_names$region))
  }
  if (!all(sapply(controls$group, function(x) {
    field_names$zone %in% names(x)
  }))) {
    stop("There are one or more group controls that don't have the zone field.")
  }
  if (!all(sapply(controls$individual, function(x) {
    field_names$zone %in% names(x)
  }))) {
    stop("There are one or more individual controls that don't have the zone field.")
  }

  group_controls <-
    lapply(controls$group, function(x) {
      splits <- split(x, x[[field_names$zone]])
      lapply(splits, function(x) {
        x[, !names(x) %in% field_names$zone]
      })
    })
  individual_controls <- lapply(controls$individual, function(x) {
    splits <- split(x, x[[field_names$zone]])
    lapply(splits, function(x) {
      x[, !names(x) %in% field_names$zone]
    })
  })

  zones_from_group_controls <- unique(unlist(lapply(group_controls, names)))
  zones_from_individual_controls <- unique(unlist(lapply(individual_controls, names)))
  if (!isTRUE(all.equal(zones_from_individual_controls, zones_from_group_controls))) {
    if (requireNamespace("waldo")) {
      comparison <-
        waldo::compare(zones_from_individual_controls, zones_from_group_controls,
          x_arg = "zones from individual controls",
          y_arg = "zones from group controls"
        )
    } else {
      comparison <- "To see the comparison install the `waldo` package and run this again."
    }
    stop("Zone mismatch between individual and group controls:\n", comparison)
  }

  if (!is.null(prior_weights)) {
    warning("Creating ml_problems by zone doesn't utilise `prior_weights`.")
  }

  fitting_problems <- lapply(zones_from_group_controls, function(zone) {
    zone_region <- geo_hierarchy[[field_names$region]][which(geo_hierarchy[[field_names$zone]] == zone)]
    zone_ref_sample <- ref_sample[ref_sample[[field_names$region]] == zone_region, ]
    zone_ref_sample[[field_names$region]] <- NULL
    zone_controls <- list(
      group = unlist(lapply(group_controls, function(x) {
        x[names(x) %in% zone]
      }), recursive = FALSE),
      individual = unlist(lapply(individual_controls, function(x) {
        x[names(x) %in% zone]
      }), recursive = FALSE)
    )
    new_ml_problem(
      list(
        refSample = zone_ref_sample,
        controls = zone_controls,
        fieldNames = field_names,
        priorWeights = NULL,
        zone = zone
      )
    )
  })

  names(fitting_problems) <- zones_from_group_controls

  fitting_problems
}

#' Create a fitting problem
#'
#' Soft-deprecated, new code should use [ml_problem()].
#'
#' @return See [ml_problem()].
#' @importFrom lifecycle deprecate_soft
#' @export
#' @keywords internal
fitting_problem <- function(ref_sample,
                            controls = list(
                              individual = individual_controls,
                              group = group_controls
                            ),
                            field_names,
                            individual_controls, group_controls,
                            prior_weights = NULL) {
  deprecate_soft("0.4.0", "mlfit::fitting_problem()", "mlfit::ml_problem()")
  ml_problem(
    ref_sample, controls,
    field_names,
    individual_controls, group_controls,
    prior_weights
  )
}


new_ml_problem <- make_new("ml_problem")

#' @export
#' @rdname ml_problem
#' @param x An object
#' @return `is_ml_problem()` returns a logical.
is_ml_problem <- make_is("ml_problem")

#' @export
#' @rdname ml_problem
#' @param ... Ignored.
format.ml_problem <- function(x, ...) {
  c(
    "An object of class ml_problem",
    "  Reference sample: " %+% nrow(x$refSample) %+% " observations",
    "  Control totals: " %+% length(x$controls$individual) %+%
      " at individual, and " %+% length(x$controls$group) %+% " at group level",
    if (length(x$algorithms) > 0L) {
      "  Results for algorithms: " %+% paste(x$algorithms, collapse = ", ")
    },
    if (!is.null(x$zone)) {
      "  Zone: " %+% x$zone
    }
  )
}

#' @export
#' @rdname ml_problem
print.ml_problem <- default_print

#' @description
#' The `special_field_names()` function is useful for the `field_names` argument
#' to `ml_problem`.
#'
#' @param groupId,individualId Name of the column that defines the ID of the
#'   group or the individual
#' @param individualsPerGroup Obsolete.
#' @param count Name of control total column in control tables (use first numeric
#'   column in each control by default).
#' @param region,zone Name of the column that defines the region of the reference
#' sample or the zone of the controls. Note that region is a larger area that contains
#' more than one zone.
#'
#' @export
#' @rdname ml_problem
special_field_names <- function(groupId, individualId, individualsPerGroup = NULL,
                                count = NULL, zone = NULL, region = NULL) {
  if (!is.null(individualsPerGroup)) {
    warning("The individualsPerGroup argument is obsolete.", call. = FALSE)
  }
  tibble::lst(groupId, individualId, count, zone, region)
}
