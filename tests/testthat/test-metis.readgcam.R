context("metis.readgcam Tests")
library(metis)
library(rgcam)

test_that("metis.readgcam reads data from .proj file", {

  dataGCAM<-metis.readgcam(dataProj = metis::exampleGCAMproj,saveData = F)
  tVal <- nrow(dataGCAM$data[!complete.cases(dataGCAM$data),])
  expect_equal(0, tVal)


})

test_that("metis.readgcam reads data from .proj file for regionsSelect='Colombia'", {

  dataGCAM<-metis.readgcam(reReadData = F,
                           dataProjFile = metis::exampleGCAMproj,
                           regionsSelect = "Colombia" ,
                           saveData = F)
  tVal <- nrow(dataGCAM$data[!complete.cases(dataGCAM$data),])
  unlink(paste(getwd(),"/outputs",sep=""))
  expect_equal(0, tVal)


})

test_that("metis.readgcam reads data from .proj file for paramsSelect=c('energy','water')", {

  dataGCAM<-metis.readgcam(reReadData = F,
                           dataProjFile = metis::exampleGCAMproj,
                           paramsSelect = c("energy","water"),
                           saveData = F)
  tVal <- nrow(dataGCAM$data[!complete.cases(dataGCAM$data),])
  unlink(paste(getwd(),"/outputs",sep=""))
  expect_equal(0, tVal)


})

