context("import")

test_that("import toy examples", {
  require(plyr)
  require(kimisc)
  test_names <- c("minitoy", "toy", "dummytoy", "multitoy")
  test_paths <- system.file(file.path("extdata", test_names), package = "MultiLevelIPF")
  results <- llply(setNames(test_paths, nm=test_names), import_IPAF_results)

  TOL <- list(total=2e-3, individual=0.6, group=0.8)

  llply(
    results,
    function(result) {
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
            } else
              subWeightedRefSample$w / subWeightedRefSample[[result$fieldNames$individualsPerGroup]]
            expect_equal(sum(subWeights), control1[[countVar]], tolerance=TOL[[type]])
          }
        )
      }

      l_ply(
        result$weights,
        function (lw) {
          expect_equal(length(lw), 1)
          weightedRefSample <- result$refSample
          weightedRefSample$w <- lw[[1]]
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
  require(plyr)
  require(kimisc)
  test_names <- c("minitoy", "toy", "dummytoy", "multitoy")
  test_paths <- system.file(file.path("extdata", test_names), package = "MultiLevelIPF")
  results <- llply(setNames(test_paths, nm=test_names), import_IPAF_results)

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

test_that("import with more than one control of each type", {
  require(plyr)
  require(kimisc)
  l_ply(
    list(c(1,1), c(2,2), c(1,3), c(2,3)),
    function(gi) {
      dirname <- if (all(gi == 1)) "minitoy" else
        do.call(sprintf, c(list("minitoy-%sx%s"), gi))
      result_path <- system.file(file.path("extdata", dirname), package = "MultiLevelIPF")
      config <- import_IPAF_results(result_path, all_weights = FALSE)
      expect_equal(length(config$controls), 2)
      expect_equal(length(config$controls$individual), gi[[1]])
      expect_equal(length(config$controls$group), gi[[2]])
    }
  )
})
