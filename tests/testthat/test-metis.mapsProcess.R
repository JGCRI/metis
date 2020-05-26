context("metis.mapsProcess Tests")
library(metis)

test_that("metis.mapsProcess finds correct map for US49 States", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("FL","ID"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^US49$")
  expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for US49 States Full Names", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("FlorIDa","IDaho"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^US49$")
  expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for US49 States ignroing case.", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("fl","iD"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^US49$")
  expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for US49 States unknown regions.", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("fl","AlienPlanet"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^US49$")
  expect_equal(tVal2,1)

})

test_that("metis.mapsProcess finds correct map for US52 States", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("FL","AK"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^US52$")
  expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for US49 Counties", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("manatee","macon"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^US49County$")
  expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for US52 Counties", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("manatee","yauco"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^US52County$")
  expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for US52 + GCAM Region", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("FL","Pakistan"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^GCAM32US52$")
  expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for US52 + Other Country", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("FL","Afghanistan"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^countriesUS52$")
  expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for State other than US52", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("FL","Punjab"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^states$")
  expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for State Full Name other than US52", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("florida","Punjab"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^states$")
  expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for GCAM Basin in US49 only", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("Fraser","Baja_California"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^GCAMBasinsUS49$")
  expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for GCAM Basin in US52 only", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("Fraser","hawaii"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^GCAMBasinsUS52$")
  expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for GCAM Basin Global", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("Fraser","Indus"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^GCAMBasins$")
  expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for GCAM Land in US49 only", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("Fraser","Great_Lakes"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^GCAMBasinsUS49$")
  expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for GCAM Basin in US52 only", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("Caribbean","Hawaii"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^GCAMBasinsUS52$")
  expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for GCAM Basin Global", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("Fraser","Indus"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^GCAMBasins$")
  expect_equal(tVal2,2)

})

test_that("metis.mapsProcess plots multiple years and creates animations", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("TX","TX","TX"),
                    x=c(2020,2030,2040),
                    value=c(1,3,5))
  mapx <- metis.mapsProcess(polygonDataTables=data,dirNameAppend = "multiyear"); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^US49$")
  expect_equal(tVal2,3)

})

test_that("metis.mapsProcess plots multiple scenarios and creates diff plots", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("CA","CA"),
                    x=c(2050,2050),
                    scenario=c("Ref","Scen1"),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data,scenRef="Ref",dirNameAppend = "multiScen"); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^US49$")
  expect_equal(tVal2,4)

})

test_that("metis.mapsProcess plots multi-facets for multi-classes", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("TX","TX"),
                    x=c(2050,2050),
                    class=c("classA","classB"),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data, dirNameAppend = "multiClass"); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  expect_match(tVal1,"^US49$")
  expect_equal(tVal2,2)

})
