context("utils")

test_that("shifting away leading zeros", {
  expect_equal(remove_leading_zeros(1:3), 1:3)
  expect_equal(remove_leading_zeros(0:3), 1:3)
  expect_equal(remove_leading_zeros(-1:3), -1:3)
  expect_equal(remove_leading_zeros(c(rep(0, 100), 1:3)), 1:3)
})
