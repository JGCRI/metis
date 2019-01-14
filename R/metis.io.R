#' metis.io
#'
#' This function prepares gridded data for use with other metis modules.
#' @param dirOutputs Default =paste(getwd(),"/outputs",sep=""),
#' @param Z0 Default = NULL,
#' @param D0 Default = NULL,
#' @param X0 Default = NULL,
#' @param D Default = NULL,
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @export


met.io<-function(Z0=NULL,
                 D0=NULL,
                 X0=NULL,
                 D=NULL,
                 dirOutputs=paste(getwd(),"/outputs",sep="")
                        ){

#----------------
# Initialize variables by setting to NULL
#----------------

NULL->year->sector->domestic->export->total->V1->value->.->other->production->external

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


  # Check Columns
  if(is.null(Z0) & is.null(D0) & is.null(X0)){stop("Need to provide atleast two of Z0, X0, D0")}

  if(!is.null(Z0)){
    if(!"sector" %in% names(Z0)){stop(
      paste("Need to format Z0 to have a 'sector' column corresponding to each sector. eg. ",
            tibble::tibble(sector=c("ag","wat"),ag=c(5,0),wat=c(10,2)),sep=""))}
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

} # Close Function
