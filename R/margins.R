#' Compute margins for a weighting of a multi-level fitting problem
#'
#' These functions allow checking a fit in terms of the original input data.
#'
#' @details
#' `compute_margins()` computes margins in the format used for the input
#' controls (i.e., as expected by the `controls` parameter of the
#' [ml_problem()] function), based on a reference sample and a weight vector.
#'
#' @param x an `ml_fit` or `ml_problem` object.
#' @param weights A vector with one entry per row of the original reference
#'   sample
#' @param verbose If `TRUE`, print diagnostic output.
#' @param ... Not being used.
#' @return `compute_margins()` returns a named list with two components,
#'   `individual` and `group`. Each contains a list of margins as `data.frame`s.
#'
#' @seealso [ml_fit()]
#' @export
#' @importFrom plyr llply
#' @examples
#' path <- toy_example("Tiny")
#' problem <- readRDS(path)
#' fit <- ml_fit(ml_problem = problem, algorithm = "entropy_o")
#' margins <- compute_margins(problem, fit$weights)
#' margins
#' # Alternatively
#' compute_margins(fit)
compute_margins <- function(x, ...) {
  UseMethod("compute_margins", x)
}

#' @rdname compute_margins
#' @export
compute_margins.ml_fit <- function(x, verbose = FALSE, ...) {
  compute_margins(x = x$flat$ml_problem, weights, weights = x$weights, verbose = verbose)
}

#' @rdname compute_margins
#' @export
compute_margins.ml_problem <- function(x, weights, verbose = FALSE, ...) {
  .check_is_ml_problem(x)
  ref_sample <- x$refSample
  controls <- x$controls
  field_names <- x$fieldNames

  .patch_verbose()

  if (length(weights) != nrow(ref_sample)) {
    stop("For weights, use a vector with the same length as the number of rows in the reference sample.")
  }

  message("Aggregating")
  llply(
    setNames(nm = names(controls)),
    function(control.type) {
      weights_df <- data.frame(
        ..w.. = if (control.type == "individual") {
          weights
        } else {
          ifelse(duplicated(ref_sample[[field_names$groupId]]), 0, weights)
        }
      )
      ref_sample_w <- cbind(weights_df, ref_sample)

      control.list <- controls[[control.type]]
      llply(
        control.list,
        control.type = control.type,
        function(control, control.type) {
          count_name <- get_count_field_name(control, field_names$count, message = message)
          plyr::rename(
            plyr::count(
              ref_sample_w,
              setdiff(names(control), count_name),
              "..w.."
            ),
            c("freq" = count_name)
          )
        }
      )
    }
  )
}

#' @details
#' `margins_to_df()` converts margins to a data frame for easier comparison.
#'
#' @param controls Margins as returned by `compute_margins` or as passed
#'   to the `controls` parameter of [ml_problem()].
#' @param count Name of control total column, autodetected by default.
#'
#' @rdname compute_margins
#' @return `margins_to_df()` returns a data frame with the following columns:
#' \describe{
#'   \item{`..control.type..`}{Type of the control total: either `individual` or `group`.}
#'   \item{`..control.name..`}{Name of the control total, if exists.}
#'   \item{`..id..`}{Name of the control category.}
#'   \item{`..count..`}{Count of the control category.}
#' }
#' @importFrom plyr ldply
#' @export
#' @examples
#'
#' margin_to_df(problem$controls)
#' margin_to_df(margins)
margin_to_df <- function(controls, count = NULL, verbose = FALSE) {
  .patch_verbose()

  message("Converting list structure to data frame")
  ldply(
    setNames(nm = names(controls)),
    function(control.type) {
      control.list <- controls[[control.type]]
      ldply(
        control.list,
        control.type = control.type,
        function(control, control.type) {
          count <- get_count_field_name(control, count, message)
          non_control <- control[sort(setdiff(names(control), count))]
          lex_order <- do.call(order, non_control)
          data.frame(
            ..id.. = interaction(non_control[lex_order, ]),
            ..count.. = control[[count]][lex_order]
          )
        },
        .id = "..control.name.."
      )
    },
    .id = "..control.type.."
  )
}

#' Combine fitted margins and control margins
#' 
#' This function is useful for extracting margins for 
#' further analysis.
#' 
#' @param ml_fit a `ml_fit` object.
#' 
#' @return returns a named list with two components,
#'  `individual` and `group`. Each contains a list of margins 
#'  as `data.frame`s.
#' @export
#' @examples
#' fit <- ml_fit(readRDS(toy_example("Tiny")), "ipu")
#' combine_margins(fit)
combine_margins <- function(ml_fit) {
  stopifnot(is_ml_fit(ml_fit))
  control_margins <- ml_fit$flat$ml_problem$controls
  fit_margins <- compute_margins(ml_fit)
  count_name <- ml_fit$flat$ml_problem$fieldNames$count

  .combine_margin <- function(ctrl_lst, fit_lst, count_name) {
    mapply(
      function(ctrl, fit) {
        res <- cbind(ctrl, ..fit_count.. = fit[[count_name]])
        names(res)[names(res) == count_name] <- "..ctrl_count.."
        res$..residual.. <- res$..ctrl_count.. - res$..fit_count..
        res$..rel_residual.. <- rel_residuals(res$..ctrl_count.., res$..fit_count..)
        res
      },
      ctrl_lst,
      fit_lst,
      SIMPLIFY = FALSE
    )
  }

  ind_margins <- .combine_margin(
    control_margins$individual,
    fit_margins$individual,
    count_name
  )

  group_margins <- .combine_margin(
    control_margins$group,
    fit_margins$group,
    count_name
  )

  list(individual = ind_margins, group = group_margins)
}