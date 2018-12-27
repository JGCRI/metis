#' metis.io
#'
#' This function prepares gridded data for use with other metis modules.
#' @param dirOutputs Default =paste(getwd(),"/outputs",sep=""),
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @export

metis.io<- function(
                        dirOutputs=paste(getwd(),"/outputs",sep="")
                        ){

#------------------
# Load required Libraries
# -----------------
requireNamespace("tibble",quietly = T)
requireNamespace("dplyr",quietly = T)

#----------------
# Initialize variables by setting to NULL
#----------------

NULL->year->sector->domestic->export->total->V1->value->.

#------------------
# Create Folders if needed
#------------------
if (!dir.exists(dirOutputs)){
    dir.create(dirOutputs)}
if (!dir.exists(paste(dirOutputs, "/IO", sep = ""))){
    dir.create(paste(dirOutputs, "/IO", sep = ""))}


#------------------
# Function for adding any missing columns if needed
# -----------------

addMissing<-function(data){
  if(!"scenario"%in%names(data)){data<-data%>%dplyr::mutate(scenario="scenario")}
  if(!"x"%in%names(data)){if("year"%in%names(data)){
    data<-data%>%dplyr::mutate(x=year)}else{data<-data%>%dplyr::mutate(x="x")}}
  if(!"region"%in%names(data)){data<-data%>%dplyr::mutate(region="region")}
  if(!"classPalette"%in%names(data)){data<-data%>%dplyr::mutate(classPalette="pal_hot")}
  if(!"param"%in%names(data)){data<-data%>%dplyr::mutate(param="param")}
  return(data)
}


#------------------
# Read in shape Tables (Either csv tables or R table
#------------------

ioTblEg=tibble::tribble(
  ~sector ,    ~wat,         ~ag,  ~domesticOther, ~export,
  "wat"   ,    1600,    72000,      -33600,      0,
  "ag"    ,       0,     4000,       96000,      0
)

ioTblEg<-ioTblEg%>%dplyr::mutate(total=rowSums(dplyr::select(., -("sector"))));utils::head(ioTblEg)

# Calculate technological co-efficient matrix A
A<-as.matrix(ioTblEg%>%dplyr::select(-sector,-domestic,-export,-total)) %*% as.matrix(ioTblEg$total^-1*diag(nrow(ioTblEg)));
A

# Calculate Leontief Coefficient Matrix L = (I-A)^-1
# X = AX +D  where:
# X is the production of each individual sectors. Once L is calculated X = L*D
# XA is the internal production
# D is the external production

L<-solve(diag(nrow(ioTblEg))-A);
L

# Given external Demand D what is X
#D <- ioTblEg$domestic; D
D<-c(33600,0);D
X <- tibble::as_tibble(L%*%D)%>%
  dplyr::bind_cols(ioTblEg%>%dplyr::select(sector),tibble::as_tibble(D))%>%
  dplyr::rename(production=V1,external=value);X
OwnFlow<-L%*%D-D;OwnFlow


# Testing Inverse Calculation for Coefficient Matrix
# start_time <- Sys.time()
# as.matrix(ioTblEg%>%dplyr::select(-sector,-domesticOther,-export,-total)) %*% as.matrix(ioTblEg$total^-1*diag(nrow(ioTblEg)));
# end_time <- Sys.time()
# t1=end_time - start_time
#
# start_time <- Sys.time()
# sweep(x=ioTblEg%>%dplyr::select(-sector,-domestic,-export,-total),
#       MARGIN=2,
#       STATS=ioTblEg$total,
#       FUN="/",
#       check.margin=T)
# end_time <- Sys.time()
# t2=end_time - start_time
# t1;t2


} # Close Function
