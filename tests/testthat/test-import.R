context("import")

test_that("import minitoy example", {
  require(plyr)
  results <- llply(setNames(nm=c("minitoy", "toy")), import_IPAF_results)
  print(results)
})
