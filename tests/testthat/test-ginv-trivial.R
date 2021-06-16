set.seed(20150121)

test_that("Compare ginv results", {
  rows <- 13L
  cols <- 19L
  X <- matrix(runif(rows * cols), rows, cols)
  my_ginv <- gginv()
  expect_equal(MASS::ginv(X), my_ginv(X))
  expect_equal(MASS::ginv(X, tol = 1), gginv(tol = 1)(X))
  expect_equal(MASS::ginv(X, tol = 0.1), gginv(tol = 0.1)(X))
  X <- matrix(
    complex(real = runif(rows * cols), imaginary = runif(rows * cols)),
    rows, cols
  )
  expect_equal(MASS::ginv(X), my_ginv(X))
})

test_that("ginv error messages", {
  my_ginv <- gginv()
  expect_error(my_ginv(array(1:24, dim = 2:4)), NA)
  expect_error(my_ginv(Matrix::sparseMatrix(1:10, 1:10, x = 1:10)), NA)
  expect_error(my_ginv(array(letters[1:6], dim = 2:3)), "numeric or complex")
})
