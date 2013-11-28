context("import")

test_that("import minitoy and toy examples", {
  require(plyr)
  results <- llply(setNames(nm=c("minitoy", "toy")), import_IPAF_results)
  print(results)
})
