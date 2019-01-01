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
requireNamespace("magrittr",quietly = T)

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

met.io<-function(Z0=NULL,D0=NULL,X0=NULL,D=NULL){

  # Z0 is intermediate flows
  # D0 is inital external demand
  # X0 Total production in each sector
  # A is the coefficient matrix Z0/X0
  # L Leontief inverse Matrix L = (I-A)^-1
  # D is the new external demand

  # Check Columns
  if(is.null(Z0) & is.null(D0) & is.null(X0)){stop("Need to provide atleast two of Z0, X0, D0")}

  if(!is.null(Z0)){
    if(!"sector" %in% names(Z0)){stop(
      paste("Need to format Z0 to have a 'sector' column corresponding to each sector. eg. ",
            tibble(sector=c("ag","wat"),ag=c(5,0),wat=c(10,2)),sep=""))}
  if(is.null(D0)){
    names(X0)="total"
    external=(X0-rowSums(Z0%>%dplyr::select(-sector)))%>%dplyr::rename(external=names(X0))
    D0=external
    ioTbl<-dplyr::bind_cols(Z0,external,X0)
  }else{
    names(D0)="external"
    total=(D0+rowSums(Z0%>%dplyr::select(-sector)))%>%dplyr::rename(total=names(D0))
    ioTbl<-dplyr::bind_cols(Z0,D0,total)};ioTbl
  }else{stop("No intermediate flow matrix provided")}

  # Calculate technological co-efficient matrix A
  A<-as.matrix(Z0%>%dplyr::select(-sector)) %*% as.matrix(ioTbl$total^-1*diag(nrow(Z0)));
  L<-solve(diag(nrow(ioTbl))-A);

  X <- tibble::as_tibble(L%*%as.matrix(D0)) # Total Production
  names(X)="total";X
  Z<-A%*%diag(x=(as.vector(L%*%as.matrix(D0))));Z  # Internal flows
  solOrig<-tibble::as_tibble(Z)
  names(solOrig)=ioTbl$sector;
  intermediateOutput=tibble::tibble(intermediateOutput=rowSums(solOrig))
  valueAdded=tibble::as_tibble(X-t(as.matrix(rep(1,nrow(Z))))%*%Z);names(valueAdded)="valueAdded"
  solOrig<-solOrig%>%
    dplyr::mutate(sector=ioTbl$sector)%>%
    dplyr::bind_cols(X,D0,intermediateOutput,valueAdded)%>%
    dplyr::select(sector,ioTbl$sector,external,total,intermediateOutput,valueAdded)

  if(!is.null(D)){D=tibble::as_tibble(D);names(D)="external";
  X <- tibble::as_tibble(L%*%as.matrix(D)) # Total Production
  names(X)="total";X
  Z<-A%*%diag(x=(as.vector(L%*%as.matrix(D))));Z  # Internal flows
  sol<-tibble::as_tibble(Z)
  names(sol)=ioTbl$sector;sol
  intermediateOutput=tibble::tibble(intermediateOutput=rowSums(sol))
  valueAdded=tibble::as_tibble(X-t(as.matrix(rep(1,nrow(Z))))%*%Z);names(valueAdded)="valueAdded"
  sol<-sol%>%
    dplyr::mutate(sector=ioTbl$sector)%>%
    dplyr::bind_cols(X,D,intermediateOutput,valueAdded)%>%
    dplyr::select(sector,ioTbl$sector,external,total,intermediateOutput,valueAdded)
  }else{
      sol<-"No change from original solOrig"}

  print(list(A=A,L=L,solOrig=solOrig,sol=sol))

  return(list(A=A,L=L,Z=Z,solOrig=solOrig,sol=sol))
}

# Blair and Miller Problems Chapter 2 - Simple I/O

# Problem 2.1
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~ag,         ~man,
  "ag"     ,    500,         350,
  "man"     ,    320,        360);Z0


X0=tibble::tribble( # Initial total demand
  ~total,
  1000,
  800
);X0


io<-met.io(Z0=Z0,X0=X0,D=c(200,100))
io$A
io$L

#Problem 2.1b
(diag(nrow(io$ioTbl)) + io$A + io$A%*%io$A + io$A%*%io$A%*%io$A + io$A%*%io$A%*%io$A%*%io$A)%*%as.matrix(io$D)
#i. [794.9863,613.7669]
#ii. [1139,844]

#Problem 2.2
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~S1,         ~S2, ~S3,
  "S1"     ,    350,         0, 0,
  "S2"     ,    50,        250, 150,
  "S3"     ,    200,       150, 550);Z0


X0=tibble::tribble( # Initial total demand
  ~total,
  1000,
  500,
  1000
);X0


io<-met.io(Z0=Z0,X0=X0,D=c(1300,100,200))
io$A
io$L


#Problem 2.3
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~ag,         ~man, ~hhc,
  "ag"     ,    500,         350, 90,
  "man"     ,    320,        360, 50,
  "hhc"    ,     100,         60, 40);Z0


X0=tibble::tribble( # Initial total demand
  ~total,
  1000,
  800,
  300
);X0


io<-met.io(Z0=Z0,X0=X0,D=c(110,50,100))
io$A
io$L

# Problem 2.4
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~lum,         ~pap, ~mach,
  "lum"     ,    50*0.05,     50*0.2, 50*0.05,
  "pap"     ,    50*0.05,        50*0.1, 50*0.05,
  "mach"    ,     100*0.3,         100*0.3, 100*0.15);Z0


X0=tibble::tribble( # Initial total demand
  ~total,
  50,
  50,
  100
);X0


io<-met.io(Z0=Z0,X0=X0,D=c(35*0.75,40*0.9,25*0.95))
io$A
io$L

# Problem 2.5
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~A,         ~B,
  "A"     ,    2,         8,
  "B"     ,    6,        4);Z0


D0=tibble::tribble( # Initial total demand
  ~external,
  20,
  20
);D0


io<-met.io(Z0=Z0,D0=D0,D=c(15,18))
io$A
io$L

# Problem 2.6
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~A,         ~B,
  "A"     ,    6,         2,
  "B"     ,    4,        2);Z0


X0=tibble::tribble( # Initial total demand
  ~total,
  20,
  15
);X0


io<-met.io(Z0=Z0,X0=X0)
io$A
io$L

# Problem 2.11
Z0=tibble::tribble( # Initial Flows
  ~sector ,    ~ag,         ~sv, ~comp,
  "ag"     ,    2,         2, 1,
  "sv"     ,    1,         0, 0,
  "comp"   ,    2,         0, 1);Z0


X0=tibble::tribble( # Initial total demand
  ~total,
  5,
  2,
  2
);X0


io<-met.io(Z0=Z0,X0=X0)
io$A
io$L




#---------------------------
# Multi Regional I/O
#--------------------------
# Miller and Blair Chapter 3.4
# Introducing Input-output analysis at the regional level: basic notions and specific issues. Ana Lucia Marto Sargento. REAL


mrioTblEg=tibble::tribble(
  ~sector ,    ~r1_c,         ~r2_c,  ~other,
  "r1_c"     ,    20,         30,       50,
  "r2_c"     ,    40,         10,       50
)

mrioTblEg<-mrioTblEg%>%dplyr::mutate(total=rowSums(dplyr::select(., -("sector"))));utils::head(mrioTblEg)

# Calculate technological co-efficient matrix A
A<-as.matrix(mrioTblEg%>%dplyr::select(-sector,-other,-total)) %*% as.matrix(mrioTblEg$total^-1*diag(nrow(mrioTblEg)));
A

# Calculate Leontief Coefficient Matrix L = (I-A)^-1
# X = AX +D  where:
# X is the production of each individual sectors. Once L is calculated X = L*D
# XA is the internal production
# D is the external production
# Note: L can also be expanded as L=(I-A)^-1= I + A + A^2 + A^3 etc.

L<-solve(diag(nrow(mrioTblEg))-A);
L

# Given external Demand D what is X total production
#D <- ioTblEg$domestic; D
D<-c(100,0);D
X <- tibble::as_tibble(L%*%D)%>%
  dplyr::bind_cols(mrioTblEg%>%dplyr::select(sector),tibble::as_tibble(D))%>%
  dplyr::rename(production=V1,external=value)%>%
  dplyr::mutate(internal=production-external);X
Z<-A%*%diag(x=(as.vector(L%*%D)));Z  # Z gives the distribution of the interna; production




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
