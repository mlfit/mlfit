library(MultiLevelIPF)
library(testthat)

context("flatten")

test_that("forward and reverse maps", {
  require(plyr)
  require(kimisc)
  group_id <- c(1, 2, 2, 3, 3, 3)
  ref_sample <- data.frame(group_id=group_id, ind=letters[1:2], group=LETTERS[group_id])
  controls <- list(
    group = list(
      data.frame(group = LETTERS[1:3], N = 2:4)
    )
  )
  field_names <- list(
    count = "N",
    groupId = "group_id"
  )
  flat <- flatten_ml_fit_problem(ref_sample, controls, field_names)
  expect_equal(as.vector(group_id %*% flat$weights_transform %*% flat$reverse_weights_transform),
               group_id)
})
