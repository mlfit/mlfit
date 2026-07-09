test_that("forward and reverse maps", {
  group_id <- c(1, 2, 2, 3, 3, 3)
  ref_sample <- data.frame(group_id = group_id, ind = letters[1:2], group = LETTERS[group_id])
  controls <- list(
    group = list(
      data.frame(group = LETTERS[1:3], N = 2:4)
    )
  )
  field_names <- list(
    count = "N",
    groupId = "group_id"
  )
  problem <- ml_problem(ref_sample, controls, field_names)
  flat <- flatten_ml_fit_problem(problem)
  expect_equal(flat$ml_problem, problem)
  expect_equal(
    as.vector(group_id %*% flat$weights_transform %*% flat$reverse_weights_transform),
    group_id
  )
})

test_that("factors", {
  group_id <- c(1, 2, 2, 3, 3, 3)
  ref_sample <- data.frame(group_id = group_id, ind = letters[1:2], group = LETTERS[group_id], stringsAsFactors = FALSE)
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
  problem <- ml_problem(ref_sample, controls, field_names)
  flat <- flatten_ml_fit_problem(problem)
  expect_equal(flat$ml_problem, problem)
})

test_that("auto controls", {
  group_id <- c(1, 2, 2, 3, 3, 3)
  ref_sample <- data.frame(group_id = group_id, ind = letters[1:2], group = LETTERS[group_id])
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

  problem <- ml_problem(ref_sample, controls, field_names)
  expect_message(
    flat <- flatten_ml_fit_problem(problem, verbose = TRUE),
    "as count column"
  )
  expect_equal(flat$ml_problem, problem)
  expect_equal(
    as.vector(group_id %*% flat$weights_transform %*% flat$reverse_weights_transform),
    group_id
  )
})

test_that("error with auto controls", {
  group_id <- c(1, 2, 2, 3, 3, 3)
  ref_sample <- data.frame(group_id = group_id, ind = letters[1:2], group = LETTERS[group_id])
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

  problem <- ml_problem(ref_sample, controls, field_names)
  expect_error(
    flat <- flatten_ml_fit_problem(problem, verbose = TRUE),
    "among control columns"
  )
})

test_that("identical households", {
  group_id <- c(1, 2, 2, 3, 3, 3)
  ref_sample <- data.frame(group_id = group_id, ind = letters[1:2], group = LETTERS[group_id])
  ref_sample <- ref_sample %>%
    bind_rows(ref_sample %>% filter(group_id >= 2) %>% mutate(group_id = group_id + 2)) %>%
    bind_rows(ref_sample %>% filter(group_id >= 3) %>% mutate(group_id = group_id + 3)) %>%
    mutate(weight = group_id)

  controls <- list(
    group = list(
      data.frame(group = LETTERS[1:3], N = 2:4)
    )
  )
  field_names <- special_field_names(
    count = "N",
    groupId = "group_id", 
    individualId = "ind",
    prior_weight = "weight"
  )
  problem <- ml_problem(ref_sample, controls, field_names)
  flat <- flatten_ml_fit_problem(problem)
  expect_equal(flat$ml_problem, problem)
  test_weights <- ref_sample$group_id
  test_weights_flat <- test_weights %*% flat$weights_transform %>% as.vector()
  expect_equal(
    as.vector(test_weights_flat %*% flat$reverse_weights_transform %*% flat$weights_transform),
    test_weights_flat
  )
})

test_that("don't need to sort by group id", {
  group_id <- c(1, 2, 3, 3, 2, 3) + 3
  ref_sample <- data.frame(group_id = group_id, ind = letters[1:2], group = LETTERS[group_id]) %>%
    mutate(weight = group_id)

  controls <- list(
    group = list(
      data.frame(group = LETTERS[4:6], N = 1:3)
    ),
    individual = list(
      data.frame(ind = letters[1:2], N = 1:2)
    )
  )

  field_names <- special_field_names(
    count = "N",
    individualId = "ind",
    groupId = "group_id", 
    prior_weight = "weight"
  )

  problem <- ml_problem(ref_sample, controls, field_names)
  problem_sorted <- ml_problem(arrange(ref_sample, group_id), controls, field_names)
  flat <- flatten_ml_fit_problem(problem)
  flat_sorted <- flatten_ml_fit_problem(problem_sorted)

  expect_identical(flat$ref_sample, flat_sorted$ref_sample)
  expect_identical(flat$target_values, flat_sorted$target_values)

  expect_identical(group_id %*% flat$weights_transform, sort(group_id) %*% flat_sorted$weights_transform)
  expect_identical(as.vector(group_id %*% flat$weights_transform %*% flat_sorted$reverse_weights_transform), sort(group_id))
  expect_identical(as.vector(sort(group_id) %*% flat_sorted$weights_transform %*% flat$reverse_weights_transform), group_id)
})

test_that("error if group id is NA", {
  group_id <- c(NA, 2, 3, 3, 2, 3) + 3
  ref_sample <- data.frame(group_id = group_id, ind = letters[1:2], group = LETTERS[group_id])

  controls <- list(
    group = list(
      data.frame(group = LETTERS[4:6], N = 1:3)
    ),
    individual = list(
      data.frame(ind = letters[1:2], N = 1:2)
    )
  )
  field_names <- list(
    count = "N",
    groupId = "group_id"
  )

  problem <- ml_problem(ref_sample, controls, field_names)
  expect_error(flatten_ml_fit_problem(problem), "NA")
})

test_that("auto-removing last observation from reference sample (#30)", {
  ye_problem <- readRDS(toy_example("Tiny"))
  ye_problem$controls$individual$IND$N[[2]] <- 0L

  expect_warning(flatten_ml_fit_problem(ye_problem), "6 distinct entries")
})
