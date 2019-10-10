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
                              pdfpng="png"){

    if(is.null(figure)){print("No figure provided.")}else{

    if(pdfpng=='pdf'){grDevices::pdf(paste(dir,"/",filename,".pdf",sep=""),width=figWidth,height=figHeight)
      print(figure)
      grDevices::dev.off()
      fnameTempImage=paste(dir,"/",filename,".pdf",sep="")
      tempImage<-magick::image_read(fnameTempImage)
      croppedImage<-magick::image_trim(tempImage,fuzz=0);
      image_write(croppedImage,fnameTempImage)
    }
    if(pdfpng=='png'){grDevices::png(paste(dir,"/",filename,".png",sep=""),width=figWidth,height=figHeight, units="in",res=300)
      print(figure)
      grDevices::dev.off()
      fnameTempImage=paste(dir,"/",filename,".png",sep="")
      tempImage<-magick::image_read(fnameTempImage)
      croppedImage<-magick::image_trim(tempImage,fuzz=0);
      image_write(croppedImage,fnameTempImage)}
    if(pdfpng=='both'){
      grDevices::pdf(paste(dir,"/",filename,".pdf",sep=""),width=figWidth,height=figHeight)
      print(figure)
      grDevices::dev.off()
      fnameTempImage=paste(dir,"/",filename,".pdf",sep="")
      tempImage<-magick::image_read(fnameTempImage)
      croppedImage<-magick::image_trim(tempImage,fuzz=0);
      image_write(croppedImage,fnameTempImage)
      grDevices::png(paste(dir,"/",filename,".png",sep=""),width=figWidth,height=figHeight, units="in",res=300)
      print(figure)
      grDevices::dev.off()
      fnameTempImage=paste(dir,"/",filename,".png",sep="")
      tempImage<-magick::image_read(fnameTempImage)
      croppedImage<-magick::image_trim(tempImage,fuzz=0);
      image_write(croppedImage,fnameTempImage)
    }
    }
}
