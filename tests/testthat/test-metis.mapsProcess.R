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
  testthat::expect_match(tVal1,"^US49$")
  testthat::expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for US49 States Full Names", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("FlorIDa","IDaho"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  testthat::expect_match(tVal1,"^US49$")
  testthat::expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for US49 States ignroing case.", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("fl","iD"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  testthat::expect_match(tVal1,"^US49$")
  testthat::expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for US49 States unknown regions.", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("fl","AlienPlanet"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  testthat::expect_match(tVal1,"^US49$")
  testthat::expect_equal(tVal2,1)

})

test_that("metis.mapsProcess finds correct map for US52 States", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("FL","AK"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  testthat::expect_match(tVal1,"^US52$")
  testthat::expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for US49 Counties", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("manatee","macon"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  testthat::expect_match(tVal1,"^US49County$")
  testthat::expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for US52 Counties", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("manatee","yauco"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  testthat::expect_match(tVal1,"^US52County$")
  testthat::expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for US52 + Country", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("FL","Pakistan"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  testthat::expect_match(tVal1,"^countriesUS52$")
  testthat::expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for US52 + GCAM 32", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("FL","EU-12"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  testthat::expect_match(tVal1,"^GCAM32US52$")
  testthat::expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for State other than US52", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("FL","Punjab"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  testthat::expect_match(tVal1,"^states$")
  testthat::expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for State Full Name other than US52", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("florida","Punjab"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  testthat::expect_match(tVal1,"^states$")
  testthat::expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for GCAM Basin in US49 only", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("Fraser","Baja_California"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  testthat::expect_match(tVal1,"^GCAMBasinsUS49$")
  testthat::expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for GCAM Basin in US52 only", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("Fraser","hawaii"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  testthat::expect_match(tVal1,"^GCAMBasinsUS52$")
  testthat::expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for GCAM Basin Global", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("Fraser","Indus"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  testthat::expect_match(tVal1,"^GCAMBasins$")
  testthat::expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for GCAM Land in US49 only", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("Fraser","Great_Lakes"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  testthat::expect_match(tVal1,"^GCAMBasinsUS49$")
  testthat::expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for GCAM Basin in US52 only", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("Caribbean","Hawaii"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  testthat::expect_match(tVal1,"^GCAMBasinsUS52$")
  testthat::expect_equal(tVal2,2)

})

test_that("metis.mapsProcess finds correct map for GCAM Basin Global", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("Fraser","Indus"),
                    x=c(2050),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  testthat::expect_match(tVal1,"^GCAMBasins$")
  testthat::expect_equal(tVal2,2)

})

test_that("metis.mapsProcess plots multiple years and creates animations", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("TX","TX","TX"),
                    x=c(2020,2030,2040),
                    value=c(1,3,5))
  mapx <- metis.mapsProcess(polygonDataTables=data,folderName = "multiyear"); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  testthat::expect_match(tVal1,"^US49$")
  testthat::expect_equal(tVal2,3)

})

test_that("metis.mapsProcess plots multiple scenarios and creates diff plots", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("CA","CA"),
                    x=c(2050,2050),
                    scenario=c("Ref","Scen1"),
                    value=c(1,3))
  mapx <- metis.mapsProcess(polygonDataTables=data,scenRef="Ref",folderName = "multiScen"); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  tVal3 <- length(unique(mapx$shapeTbl$scenario)[grepl("Diff",unique(mapx$shapeTbl$scenario))])
  testthat::expect_match(tVal1,"^US49$")
  testthat::expect_equal(tVal2,4)
  testthat::expect_equal(tVal3,2) # Check if DiffAbs and DiffPrcnt are created

})


test_that("metis.mapsProcess plots multi-facets for multi-classes", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("TX","TX","TX","TX"),
                    x=c(2030,2030,2050,2050),
                    class=c("classA","classB","classA","classB"),
                    value=c(1,3,2,5))
  mapx <- metis.mapsProcess(polygonDataTables=data, folderName = "multiClass"); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  testthat::expect_match(tVal1,"^US49$")
  testthat::expect_equal(tVal2,4)

})


test_that("metis.mapsProcess uses scaleRange vectors correctly for multi scenario diff plots", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("CA","CA","TX","TX"),
                    x=c(2050,2050,2050,2050),
                    scenario=c("Ref","Scen1","Ref","Scen1"),
                    value=c(10,33,20,65))
  mapx <- metis.mapsProcess(polygonDataTables=data,scenRef="Ref",folderName = "scaleRangeVec",
                            scaleRange=c(0,100),
                            scaleRangeDiffAbs = c(5,70),
                            scaleRangeDiffPrcnt = c(100,300)); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  testthat::expect_match(tVal1,"^US49$")
  testthat::expect_equal(tVal2,8)

})

test_that("metis.mapsProcess uses scaleRange vectors correctly for multi scenario diff plots", {

  testthat::skip_on_cran(); testthat::skip_on_travis()

  data = data.frame(subRegion=c("CA","CA","TX","TX"),
                    x=c(2050,2050,2050,2050),
                    scenario=c("Ref","Scen1","Ref","Scen1"),
                    value=c(10,33,20,65))
  mapx <- metis.mapsProcess(polygonDataTables=data,scenRef="Ref",folderName = "scaleRangeDataFrame",
                            scaleRange=data.frame(param="param",min=0,max=100),
                            scaleRangeDiffAbs = data.frame(param="param",min=5,max=70),
                            scaleRangeDiffPrcnt = data.frame(param="paramx",min=100,max=250)); mapx$shapeTbl
  tVal1 <- unique(mapx$shapeTbl$subRegType); tVal1
  tVal2 <- nrow(mapx$shapeTbl);tVal2
  tVal3 <- length(unique(mapx$shapeTbl$scenario)[grepl("Diff",unique(mapx$shapeTbl$scenario))])
  testthat::expect_match(tVal1,"^US49$")
  testthat::expect_equal(tVal2,8)

})


