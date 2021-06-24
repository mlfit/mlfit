test_that("Can enumerate toy examples", {
  all_toys <- toy_example()
  expect_is(all_toys, "character")
  expect_true(all(file.exists(all_toys)))

  some_toys <- toy_example(names(all_toys)[1:3])
  expect_equal(length(some_toys), 3L)

  expect_error(file.exists(toy_example("nonexisting-toy")))
})
