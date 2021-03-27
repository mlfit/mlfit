#' @details
#' To use this package, you need to:
#'
#' 1. Specify your fitting problem with [fitting_problem()]
#' 1. Optionally, convert the fitting problem to a structure that can be
#'     processed by the algorithms with [flatten_ml_fit_problem()]; this is
#'     helpful if you want to run the same fitting problem with multiple
#'     algorithms and compare results.
#' 1. Compute weights with one of the algorithms provided in this package with
#'     [ml_fit()] or one of the specialized functions
#' 1. Analyze weights or residuals, e.g. with [compute_margins()]
#'
#' @import methods
#' @import dplyr
#' @importFrom Matrix Matrix sparseMatrix sparse.model.matrix
#' @importMethodsFrom Matrix rowSums colSums diag tcrossprod t
#' @importFrom plyr l_ply d_ply
#' @importFrom stats as.formula
#' @importFrom utils head
#' @importFrom forcats fct_inorder
"_PACKAGE"
