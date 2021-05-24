test_that("Calibrating a sparse diagonal matrix against a vector", {
  N <- 5
  X_dense <- diag(N)
  d <- rep(1, N)
  totals <- seq_len(N)
  for (sparse in c(TRUE, FALSE)) {
    X <- Matrix::Matrix(X_dense, sparse = sparse)
    for (method in eval(formals(dss)$method)) {
      g <- dss(X, d, totals, method = method)
      expect_equal(g, totals, info = method, tolerance = 1e-6)
      expect_null(attr(g, "success"))
      expect_equal(length(g), N)
      expect_is(g, "numeric")
    }
  }
})
