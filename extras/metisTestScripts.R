



test_readgcam <- function (data){

  print("Original")
  print(data)
  data = data %>% dplyr::mutate(class1 = gsub("moto-x","motoX",class1))

  print("New")
  return(data)

}


data = data.frame(class1=c("moto-x-1","moto_x_2"))

test_readgcam(data)

# --------- Magick
install.packages("magick")
library(magick)
str(magick::magick_config())

newlogo <- image_scale(image_read("https://jeroen.github.io/images/Rlogo.png"), "x150")
oldlogo <- image_scale(image_read("https://developer.r-project.org/Logo/Rlogo-3.png"), "x150")
frames <- image_morph(c(oldlogo, newlogo), frames = 10)
frames <- c(oldlogo, newlogo)
animation <- image_animate(frames, fps=4)
image_write(animation, "Rlogo.gif")



