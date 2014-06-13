#' Estimate weights using an entropy optimization approach
#'
#' This function reweights a reference sample to match constraints given by
#' aggregate controls using an entropy optimization method.
#'
#' @inheritParams ml_fit
#' @return An object of classes \code{ml_fit_entropy_o} and \code{ml_fit},
#'   essentially a named list.
#' @export
ml_fit_entropy_o <- function(ref_sample, controls, field_names) {
  .patch_ml_fit_args()

  control.terms.list <- llply(
    controls,
    function(control.list) {
      control.columns <- llply(
        control.list,
        function(control) {
          control.names <- setdiff(colnames(control), field_names$count)
          control.levels <- vapply(
            control[control.names], function(f) length(levels(f)), integer(1))
          
          control.term <- paste(control.names[control.levels > 1], collapse=':')
          
          control.mm <- model.matrix(
            as.formula(sprintf("~%s-1", control.term)),
            control)
          
          list(
            term=control.term,
            control=(control[[field_names$count]] %*% control.mm)[1,]
          )
        }
      )
    }
  )

  control.formulae <- llply(
    control.terms.list,
    function(control.terms) {
      paste(laply(control.terms, `[[`, 'term'), collapse='+')
    }
  )

  ref_sample_grp.mm <- as.data.frame(model.matrix(
    as.formula(sprintf("~%s+%s-1", field_names$groupId, control.formulae$group)),
    ref_sample))
  ref_sample_grp.agg <- ref_sample_grp.mm[c(TRUE, diff(ref_sample_grp.mm[[field_names$groupId]]) != 0), ]
  ref_sample_ind.mm <- as.data.frame(model.matrix(
    as.formula(sprintf("~%s+%s-1", field_names$groupId, control.formulae$individual)),
    ref_sample))
  ref_sample_ind.agg <- ddply(
    ref_sample_ind.mm,
    field_names$groupId,
    function(group) {
    quickdf(llply(group[setdiff(colnames(group), field_names$groupId)], sum))
  })

  ref_sample.agg <- merge(ref_sample_ind.agg, ref_sample_grp.agg,
                          by=field_names$groupId)
  ref_sample.agg.m <- t(as.matrix(ref_sample.agg[
    , setdiff(colnames(ref_sample.agg), field_names$groupId)]))

  control.totals.list <- llply(
    control.terms.list,
    function(control.terms) {
      laply(control.terms, `[[`, 'control', .drop=TRUE)
    }
  )
  control.totals <- unlist(unname(control.totals.list), use.names=TRUE)
  if (any(names(control.totals) != rownames(ref_sample.agg.m))) {
    stop("The following controls do not have any corresponding observation in the reference sample:\n  ",
         paste(setdiff(names(control.totals), rownames(ref_sample.agg.m)), collapse=", "), "\n",
         "The following categories in the reference sample do not have a corresponding control:\n  ",
         paste(setdiff(rownames(ref_sample.agg.m), names(control.totals)), collapse=", "), "\n"
    )
  }
  
  par <- rep(0, length(control.totals))
  bbout <- BBsolve(
    par=par,
    fn=dss.objective.m(x=ref_sample.agg.m, control.totals=control.totals, F=exp))
  
  weights <- dss.weights.from.lambda.m(x=ref_sample.agg.m, F=exp)(bbout$par)
  weights.ref_sample <- weights[match(ref_sample[[field_names$groupId]], ref_sample.agg[[field_names$groupId]])]
  
  structure(
    list(
      weights=weights.ref_sample,
      success=(bbout$message == "Successful convergence"),
      residuals=(ref_sample.agg.m %*% weights)[,1] - control.totals,
      ref_sample.agg.m=ref_sample.agg.m,
      control_totals=control.totals,
      bbout=bbout
    ),
    class=c("ml_fit_entropy_o", "ml_fit")
  )
}

# Equation 2.1
dss.weights.from.lambda.m <- function(x, F) {
  function(lambda) {
    apply(x, 2, function(xk) {
      F(sum(xk * lambda))
    })
  }
}

# left-hand side of equation 2.2
# (right-hand side is control totals)
dss.lhs.orig.m <- function(x, F) {
  function(lambda) {
    dss.lhs.matrix <- apply(x, 2, function(xk) {
      F(sum(xk * lambda)) * xk
    })
    apply(dss.lhs.matrix, 1, sum)
  }
}

# left-hand side of equation 2.2 using weights from equation 2.1
# equivalent to dss.lhs.orig.m, but surprisingly a bit faster when using in the solver
dss.lhs.using.w.m <- function(x, F) {
  dss.weights.from.lambda <- dss.weights.from.lambda.m(x, F)
  dss.lhs.using.w.f <- function(lambda) {
    (x %*% dss.weights.from.lambda(lambda))[,1]
  }
}

dss.objective.m <- function(x, control.totals, F, dss.lhs.m=dss.lhs.using.w.m) {
  dss.lhs.orig.f <- dss.lhs.m(x, F)
  function(lambda) {
    dss.lhs.orig.f(lambda) - control.totals
  }
}
