context("flatten")

test_that("forward and reverse maps", {
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
  problem <- fitting_problem(ref_sample, controls, field_names)
  flat <- flatten_ml_fit_problem(problem)
  expect_equal(flat$fitting_problem, problem)
  expect_equal(as.vector(group_id %*% flat$weights_transform %*% flat$reverse_weights_transform),
               group_id)
})

test_that("factors", {
  group_id <- c(1, 2, 2, 3, 3, 3)
  ref_sample <- data.frame(group_id=group_id, ind=letters[1:2], group=LETTERS[group_id], stringsAsFactors = FALSE)
  controls <- list(
    group = list(
      data.frame(group = LETTERS[1:3], N = 2:4, stringsAsFactors = FALSE)
    ),
    individual = list(
      data.frame(ind = letters[1:2], N = 4:5, stringsAsFactors = FALSE)
    )
  )
  field_names <- list(
    count = "N",
    groupId = "group_id"
  )
  problem <- fitting_problem(ref_sample, controls, field_names)
  flat <- flatten_ml_fit_problem(problem)
  expect_equal(flat$fitting_problem, problem)
})

test_that("auto controls", {
  group_id <- c(1, 2, 2, 3, 3, 3)
  ref_sample <- data.frame(group_id=group_id, ind=letters[1:2], group=LETTERS[group_id])
  controls <- list(
    group = list(
      data.frame(group = LETTERS[1:3], groups = 2:4)
    ),
    individual = list(
      data.frame(ind = letters[1:2], individuals = 4:5)
    )
  )
  field_names <- list(
    groupId = "group_id"
  )

  problem <- fitting_problem(ref_sample, controls, field_names)
  expect_message(flat <- flatten_ml_fit_problem(problem, verbose = TRUE),
                 "as count column")
  expect_equal(flat$fitting_problem, problem)
  expect_equal(as.vector(group_id %*% flat$weights_transform %*% flat$reverse_weights_transform),
               group_id)
})

test_that("error with auto controls", {
  group_id <- c(1, 2, 2, 3, 3, 3)
  ref_sample <- data.frame(group_id=group_id, ind=letters[1:2], group=LETTERS[group_id])
  controls <- list(
    group = list(
      data.frame(group = LETTERS[1:3])
    ),
    individual = list(
      data.frame(ind = LETTERS[1:2])
    )
  )
  field_names <- list(
    groupId = "group_id"
  )

  problem <- fitting_problem(ref_sample, controls, field_names)
  expect_error(flat <- flatten_ml_fit_problem(problem, verbose = TRUE),
                 "among control columns")
})

test_that("identical households", {
  group_id <- c(1, 2, 2, 3, 3, 3)
  ref_sample <- data.frame(group_id=group_id, ind=letters[1:2], group=LETTERS[group_id])
  ref_sample <- ref_sample %>%
    bind_rows(ref_sample %>% filter(group_id >= 2) %>% mutate(group_id = group_id + 2)) %>%
    bind_rows(ref_sample %>% filter(group_id >= 3) %>% mutate(group_id = group_id + 3))

  controls <- list(
    group = list(
      data.frame(group = LETTERS[1:3], N = 2:4)
    )
  )
  field_names <- list(
    count = "N",
    groupId = "group_id"
  )
  problem <- fitting_problem(ref_sample, controls, field_names)
  flat <- flatten_ml_fit_problem(problem)
  expect_equal(flat$fitting_problem, problem)
  test_weights <- c(1, 2, 2, 3, 3, 3, 2, 2, 3, 3, 3, 3, 3, 3)
  expect_equal(as.vector(test_weights %*% flat$weights_transform %*% flat$reverse_weights_transform),
               test_weights)
})
