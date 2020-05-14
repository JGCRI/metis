context("metis.readgcam Tests")
library(metis)
library(rgcam)


test_that("metis.readgcam reads data from .proj file for regionsSelect='Colombia'", {

  dataGCAM<-metis.readgcam(reReadData = F,
                           dataProjFile = metis::exampleGCAMproj,
                           regionsSelect = "Colombia",
                           saveData = F)
  tVal <- nrow(dataGCAM$data>0)
 expect_gt(tVal,0)

})

test_that("metis.readgcam reads data from .proj file for paramsSelect=c('energy')", {

  dataGCAM<-metis.readgcam(reReadData = F,
                           dataProjFile = metis::exampleGCAMproj,
                           paramsSelect = c("energy","water"),
                           saveData = F)
  tVal <- nrow(dataGCAM$data>0)
  expect_gt(tVal,0)

})

