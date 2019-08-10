



test_readgcam <- function (data){

  print("Original")
  print(data)
  data = data %>% dplyr::mutate(class1 = gsub("moto-x","motoX",class1))

  print("New")
  return(data)

}


data = data.frame(class1=c("moto-x-1","moto_x_2"))

test_readgcam(data)
