context("import")

test_that("import toy examples", {
  test_names <- c("Tiny", "Single", "dummytoy", "Joint-Grouped")
  test_paths <- toy_example(test_names)
  results <- llply(setNames(test_paths, nm=test_names), readRDS)

  TOL <- list(total=2e-3, individual=0.6, group=0.8)

  llply(
    results,
    function(result) {
      # Check total weights
      sums_of_weights <- laply(result$weights, function(lw) sum(lw[[1]]))
      l_ply(diff(sums_of_weights), function(s) expect_equal(s, 0, tolerance=TOL$total))

      check_control <- function (control, type, weightedRefSample) {
        countVar <- result$fieldNames$count
        controlVars <- setdiff(names(control), countVar)
        expect_true(all(laply(controlVars, function(n)
          is.factor(weightedRefSample[[n]]) && is.factor(control[[n]]))))
        d_ply(
          control,
          controlVars,
          function (control1) {
            subWeightedRefSample <- merge(control1, weightedRefSample)
            subWeights <- if (type == "individual") {
              subWeightedRefSample$w
            } else {
              ifelse(duplicated(subWeightedRefSample[[result$fieldNames$groupId]]), 0, subWeightedRefSample$w)
            }
            expect_equal(sum(subWeights), control1[[countVar]], tolerance=TOL[[type]])
          }
        )
      }

      l_ply(
        result$weights,
        function (lw) {
          weightedRefSample <- result$refSample
          weightedRefSample$w <- lw[[length(lw)]]

          weightedRefSampleSorted <-
            plyr::arrange(weightedRefSample, get(result$fieldNames$groupId))

          group_id <- weightedRefSampleSorted[[result$fieldNames$groupId]]
          expect_identical(
            rle(group_id)$lengths,
            rle(as.integer(interaction(group_id, weightedRefSampleSorted$w)))$lengths)

          l_ply(
            c("individual", "group"),
            function(type) l_ply(
              result$controls[[type]],
              check_control,
              type=type,
              weightedRefSample=weightedRefSample
            )
          )
        }
      )
    }
  )
})

test_that("import all weights", {
  test_names <- c("Tiny", "Single", "dummytoy", "Joint-Grouped")
  test_paths <- toy_example(test_names)
  results <- llply(setNames(test_paths, nm=test_names), readRDS)

  llply(
    results,
    function(result) {
      l_ply(
        result$weights,
        function(lw) {
          if (length(lw) > 1) {
            sum_weights <- laply(lw, sum)
            expect_equal(length(sum_weights), length(lw))
            expect_equal(diff(sum_weights), rep(0, length(lw) - 1))
          }
        }
      )
    }
  )
})
