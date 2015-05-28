\name{NEWS}
\title{News for Package 'MultiLevelIPF'}
\encoding{UTF-8}
\section{Changes in MultiLevelIPF version 0.1-7 (2015-05-28)}{
  \itemize{
    \item allow character variables (in addition to factors) as control variables
  }
}
\section{Changes in MultiLevelIPF version 0.1-6 (2015-05-28)}{
  \itemize{
    \item lintr
  }
}
\section{Changes in MultiLevelIPF version 0.1-5 (2015-05-28)}{
  \itemize{
    \item straighten out imports, use \code{importFrom} instead of \code{::}
  }
}
\section{Changes in MultiLevelIPF version 0.1-4 (2015-05-27)}{
  \itemize{
    \item new functions \code{compute_margins} and \code{margins_to_df} for validation
    \item don't alter column names of controls if they are of type \code{data.table} (explicitly convert to \code{data.frame})
  }
}
\section{Changes in MultiLevelIPF version 0.1-3 (2015-05-26)}{
  \itemize{
    \item proper handling of corner cases (reference sample with one row)
  }
}
\section{Changes in MultiLevelIPF version 0.1-2 (2015-05-26)}{
  \itemize{
    \item proper handling of corner cases (grand total controls and dummy controls with only one category)
    \item explicit error message if reference sample is not sorted
  }
}
\section{Changes in MultiLevelIPF version 0.1-1 (2015-05-26)}{
  \itemize{
    \item support specification of prior weights in construction of fitting problems
  }
}
\section{Changes in MultiLevelIPF version 0.1 (2015-05-26)}{
  \itemize{
    \item new functions \code{fitting_problem}, \code{is.fitting_problem}, \code{special_field_names}
    \item all fitting functions now expect an object of class \code{fitting_problem} (as returned by the \code{fitting_problem} and \code{import_IPAF_problem} functions); former calls like \code{ml_fit(ref_sample, controls, field_names)} now need to be written as \code{ml_fit(fitting_problem(...))}
  }
}
\section{Changes in MultiLevelIPF version 0.0-14 (2015-04-15)}{
  \itemize{
    \item use \code{grake} package instead of \code{laeken}
    \item new argument \code{ginv} to \code{ml_fit_dss}, passed down to calibWeights
  }
}
\section{Changes in MultiLevelIPF version 0.0-13 (2015-04-13)}{
  \itemize{
    \item fix example for \code{ml_fit_dss}
  }
}
\section{Changes in MultiLevelIPF version 0.0-12 (2014-11-21)}{
  \itemize{
    \item new function \code{ml_fit_dss} with an implementation very close to the paper by
      Deville et al. (1993); implementation in the \code{laeken} package
    \item normalize weights to get rid of precision problems
    \item allow partly uncontrolled attributes and controls without observations in the reference sample (with a warning, #24)
    \item better error reporting for non-factor controls and existence of group ID column
    \item improve warning and progress messages
  }
}
\section{Changes in MultiLevelIPF version 0.0-11 (2014-07-25)}{
  \itemize{
    \item return correct weights -- regression introduced in v0.0.9
    \item rewrite transformation of weights using sparse matrices and a home-grown
      Moore-Penrose inverse for our (very special) transformation matrix (#17)
    \item warn on missing observations for nonzero controls (#20)
    \item \code{ml_fit_entropy_o} also returns flat weights
    \item allow arbitrary order in control total tables (#19)
    \item remove observations that correspond to zero-valued control totals, with warning;
      don't warn if no corresponding observations need to be removed (#16)
  }
}
\section{Changes in MultiLevelIPF version 0.0-10 (2014-07-04)}{
  \itemize{
    \item support multiple controls at individual or group level, also detect conflicting
      control totals
    \item support fitting one-dimensional problems (where only group-level controls are given)
  }
}
\section{Changes in MultiLevelIPF version 0.0.9 (2014-06-19)}{
  \itemize{
    \item new function \code{flatten_ml_fit_problem}: transform representation as returned
      by \code{import_IPAF_result} into a matrix, a control vector and a weights vector
    \item function \code{ml_fit_entropy_o}: use \code{BB::dfsane} instead of \code{BB::BBsolve} for
      solving the optimization problem; rename argument \code{BBsolve_args} to \code{dfsane_args}
    \item function \code{ml_fit}: new parameter \code{verbose}
    \item aggregate identical household types, implement prior weights (so far only
      internally)
  }
}
\section{Changes in MultiLevelIPF version 0.0.8 (2014-06-17)}{
  \itemize{
    \item Add example for \code{ml_fit} (#11)
    \item allow additional arguments for the algorithms; \code{ml_fit_entropy_o} now accepts
      a named list \code{BBsolve_args} that contains additional arguments to \code{BB::BBsolve}
    \item Faster internal data preparation for \code{ml_fit_entropy_o}
  }
}
\section{Changes in MultiLevelIPF version 0.0.7 (2014-06-17)}{
  \itemize{
    \item Fix dependency issues (#13, #14)
    \item Add example for \code{ml_fit_entropy_o} (#11)
    \item Print more helpful error message if control totals and reference sample
      categories do not overlap (#11)
  }
}
\section{Changes in MultiLevelIPF version 0.0.6 (2014-02-09)}{
  \itemize{
    \item \code{import_IPAF_results} now returns a class of type \code{IPAF_results}
    \item New functions \code{ml_ipf} and \code{ml_ipf_entropy_o}, implementation does not yet
      return the same weights as the Python code
    \item Convert control columns to factors
  }
}
\section{Changes in MultiLevelIPF version 0.0.5 (2014-02-07)}{
  \itemize{
    \item Fix importing configuration files with more than one control of any type
      and with comments in the control definition
    \item New parameter \code{config_name} to \code{import}, defaults to \code{config.xml}
  }
}
\section{Changes in MultiLevelIPF version 0.0.4 (2013-12-06)}{
  \itemize{
    \item Parameter \code{all_weights} to \code{import} that allows importing also intermediate
      weights.  The output format of \code{import} has changed, the weights for each
      algorithm are now always a list of weight vectors, even in the default case
      \code{all_weights == FALSE} (#5).
  }
}
\section{Changes in MultiLevelIPF version 0.0.3 (2013-11-28)}{
  \itemize{
    \item Import results of old Python code (#1).
  }
}
\section{Changes in MultiLevelIPF version 0.0.2 (2013-11-26)}{
  \itemize{
    \item Initial setup
  }
}
