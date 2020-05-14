#' metis.printPdfPng
#'
#' This function prints figure to pdf or png.
#'
#' @param figure Default=NULL. Figure to be printed
#' @param dir Default = getwd(). Directory to print figure
#' @param filename Default = "plot". File name
#' @param figWidth Default=13.
#' @param figHeight Default=9.
#' @param pdfpng Default="png". Either "pdf" or "png"
#' @param transparent Default=F
#' @keywords charts, diffplots
#' @return Prints out graphic
#' @export

#-------------
# Print to PDF or PNG
#-------------

metis.printPdfPng <- function(figure = NULL,
                              dir = getwd(),
                              filename = "plot",
                              figWidth = 13,
                              figHeight = 9,
                              pdfpng="png",
                              transparent=F){

    if(is.null(figure)){print("No figure provided.")}else{


    if(pdfpng=='pdf'){
      grDevices::pdf(paste(dir,"/",filename,".pdf",sep=""),width=figWidth,height=figHeight)
      print(figure)
      grDevices::dev.off()
      print(gsub("//","/",paste("Figure saved as: ",dir,"/",filename,".pdf", sep="")))
    }
    if(pdfpng=='png'){
      if(transparent){
        grDevices::png(paste(dir,"/",filename,".png",sep=""),bg="transparent", width=figWidth,height=figHeight, units="in",res=300)
      }else{
      grDevices::png(paste(dir,"/",filename,".png",sep=""),width=figWidth,height=figHeight, units="in",res=300)
      }
      print(figure)
      grDevices::dev.off()
      fnameTempImage=paste(dir,"/",filename,".png",sep="")
      tempImage<-magick::image_read(fnameTempImage)
      croppedImage<-magick::image_trim(tempImage,fuzz=0)
      magick::image_write(croppedImage,fnameTempImage)
      print(gsub("//","/",paste("Figure saved as: ",dir,"/",filename,".png", sep="")))
      }
    if(pdfpng=='both'){
      grDevices::pdf(paste(dir,"/",filename,".pdf",sep=""),width=figWidth,height=figHeight)
      print(figure)
      grDevices::dev.off()
      if(transparent){
        grDevices::png(paste(dir,"/",filename,".png",sep=""),bg="transparent", width=figWidth,height=figHeight, units="in",res=300)
      }else{
        grDevices::png(paste(dir,"/",filename,".png",sep=""),width=figWidth,height=figHeight, units="in",res=300)
      }
      print(figure)
      grDevices::dev.off()
      fnameTempImage=paste(dir,"/",filename,".png",sep="")
      tempImage<-magick::image_read(fnameTempImage)
      croppedImage<-magick::image_trim(tempImage,fuzz=0)
      magick::image_write(croppedImage,fnameTempImage)
      print(gsub("//","/",paste("Figure saved as: ",dir,"/",filename,".png", sep="")))
      print(gsub("//","/",paste("Figure saved as: ",dir,"/",filename,".pdf", sep="")))
    }
    }
}
