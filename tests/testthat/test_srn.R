library(testthat)
library(srn)

context("SRN testing")

test_that("srn.colors produces palettes to global environment", {
  srn.colors()
  expect_gte(length(cbPalette),1)
})
