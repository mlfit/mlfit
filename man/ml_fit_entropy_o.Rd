\name{ml_fit_entropy_o}
\alias{ml_fit_entropy_o}
\title{Estimate weights using an entropy optimization approach}
\usage{
ml_fit_entropy_o(ref_sample, controls, field_names)
}
\arguments{
  \item{ref_sample}{Reference sample.  Alternatively, a
  configuration as returned by
  \code{\link{import_IPAF_results}}; in this case, the
  other arguments are ignored.}

  \item{controls}{Control totals}

  \item{field_names}{Names of special columns in the data}
}
\value{
Named list.
}
\description{
This function reweights a reference sample to match constraints given by
aggregate controls using an entropy optimization method.
}

