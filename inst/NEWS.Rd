\name{NEWS}
\title{News for Package 'MultiLevelIPF'}
\encoding{UTF-8}
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
