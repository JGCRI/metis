library(testthat)
library(srn)

context("SRN testing")

test_that("srn.colors produces palettes", {
  a<-srn.colors()
  expect_gte(length(a[[1]]),1)
})
