context("import")

test_that("import minitoy and toy examples", {
  require(plyr)
  require(kimisc)
  results <- llply(setNames(nm=c("minitoy", "toy")), import_IPAF_results)
  
  TOL <- list(total=2e-3, individual=0.6, group=0.8)
  
  llply(
    results,
    function(result) {
      sums_of_weights <- laply(result$weights, sum)
      l_ply(diff(sums_of_weights), function(s) expect_equal(s, 0, tolerance=TOL$total))
      
      check_control <- function (control, type, weightedRefSample) {
        countVar <- result$fieldNames$count
        controlVars <- setdiff(names(control), countVar)
        ddply(
          control,
          controlVars,
          function (control1) {
            subWeightedRefSample <- merge(control1, weightedRefSample)
            subWeights <- if (type == "individual")
              subWeightedRefSample$w
            else
              subWeightedRefSample$w / subWeightedRefSample[[result$fieldNames$individualsPerGroup]]
            expect_equal(sum(subWeights), control1[[countVar]], tolerance=TOL[[type]])
          }
        )
      }
      
      l_ply(
        result$weights,
        function (w) {
          weightedRefSample <- result$refSample
          weightedRefSample$w <- w
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
