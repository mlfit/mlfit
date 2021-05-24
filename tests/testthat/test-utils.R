test_that("shifting away leading zeros", {
  expect_equal(remove_leading_zeros(1:3), 1:3)
  expect_equal(remove_leading_zeros(0:3), 1:3)
  expect_equal(remove_leading_zeros(-1:3), -1:3)
  expect_equal(remove_leading_zeros(c(rep(0, 100), 1:3)), 1:3)
})


test_that("check integer", {
  expect_true(.check_is_integer(1))
  expect_true(.check_is_integer(1.00))
  expect_false(.check_is_integer(1.001))
  expect_false(.check_is_integer("1"))
  expect_true(.check_is_integer(c(1.00, 100)))
  expect_false(.check_is_integer(c(1.001, 100)))
})
