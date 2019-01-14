#' metis.mrio
#'
#' This function prepares gridded data for use with other metis modules.
#' @param dirOutputs Default =paste(getwd(),"/outputs",sep=""),
#' @param Z0 Initial intermediate flow matrix. All diagnol matrices 0. Default = NULL,
#' @param Q0 Initial trade matrix. Columns sum to 100. Default = NULL,
#' @param D0 Initital External demand. Default = NULL,
#' @param X0 Initial total Demand internal and external. Default = NULL,
#' @param D External demand or Household demand. Default = NULL,
#' @param n_regions Number of regions. Default = NULL,
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @export


met.mrio<-function(Z0=NULL,
                   Q0=NULL,
                 D0=NULL,
                 X0=NULL,
                 D=NULL,
                 n_regions=2,
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
  if(is.null(Z0) & is.null(D0) & is.null(X0)){stop("Need to provide atleast two of Z0, Q0, X0, D0")}

  if(!is.null(Z0) & !is.null(Q0)){
    if(!any(names(Z0) %in% names(Q0))){stop("Q0 and Z0 should have similar columns")}
    if(nrow(Z0)!=nrow(Q0)){stop("Q0 and Z0 should have similar rows")}
    if(!"sector" %in% names(Z0)){stop(
      paste("Need to format Z0 to have a 'sector' column corresponding to each sector. eg. ",
            tibble::tibble(sector=c("ag","wat"),ag=c(5,0),wat=c(10,2)),sep=""))}
    if(!"sector" %in% names(Q0)){stop(
      paste("Need to format Q0 to have a 'sector' column corresponding to each sector. eg. ",
            tibble::tibble(sector=c("ag","wat"),ag=c(5,0),wat=c(10,2)),sep=""))}
    X0=tibble::tibble(total=rowSums(Q0%>%dplyr::select(-sector)))
    }else{stop("No intermediate flow matrix provided")}

  # Calculate technological co-efficient matrix A
  A<-as.matrix(Z0%>%dplyr::select(-sector)) %*% as.matrix(X0$total^-1*diag(nrow(Z0)));
  C<-as.matrix(Q0%>%dplyr::select(-sector)) %*% as.matrix(colSums(Q0%>%dplyr::select(-sector))^-1*diag(nrow(Q0)));
  L<-solve(diag(nrow(Z0))-C%*%A)%*%C;
  n_sectors=length(unique(Z0$sector))/n_regions

  D0 <- tibble::as_tibble(solve(L)%*%as.numeric(unlist(X0))) # Total Production
  names(D0)="external"
  Z=Z0
  for(region_j in 1:n_regions){
    for(region_i in 1:n_regions){
  if(region_i!=region_j){
  Z[(1+n_sectors*(region_j-1)):(n_sectors+n_sectors*(region_j-1)),
     (2+n_sectors*(region_i-1)):(n_sectors+1+n_sectors*(region_i-1))]=
  cbind(C[(1+n_sectors*(region_j-1)):(n_sectors+n_sectors*(region_j-1)),
          (1+n_sectors*(region_j-1)):(n_sectors+n_sectors*(region_j-1))]%*%as.numeric(unlist(D0))[(1+n_sectors*(region_j-1)):(n_sectors+n_sectors*(region_j-1))],
        C[(1+n_sectors*(region_i-1)):(n_sectors+n_sectors*(region_i-1)),
          (1+n_sectors*(region_j-1)):(n_sectors+n_sectors*(region_j-1))]%*%as.numeric(unlist(D0))[(1+n_sectors*(region_j-1)):(n_sectors+n_sectors*(region_j-1))])
      }
  }
  };Z
  ioTbl<-dplyr::bind_cols(Z,D0,X0)
  solOrig<-tibble::as_tibble(ioTbl)
  intermediateOutput=tibble::tibble(intermediateOutput=rowSums(Z%>%dplyr::select(-sector)))
  valueAdded=tibble::as_tibble(X0-t(as.matrix(rep(1,nrow(Z))))%*%as.matrix(Z%>%dplyr::select(-sector)));names(valueAdded)="valueAdded"
  solOrig<-solOrig%>%
    dplyr::mutate(sector=Z$sector)%>%
    dplyr::bind_cols(X0,D0,intermediateOutput,valueAdded)%>%
    dplyr::select(sector,Z$sector,external,total,intermediateOutput,valueAdded)

  if(!is.null(D)){D=tibble::as_tibble(D);names(D)="external";
  Z=Z0
  for(region_j in 1:n_regions){
    for(region_i in 1:n_regions){
      if(region_i!=region_j){
        Z[(1+n_sectors*(region_j-1)):(n_sectors+n_sectors*(region_j-1)),
          (2+n_sectors*(region_i-1)):(n_sectors+1+n_sectors*(region_i-1))]=
          cbind(C[(1+n_sectors*(region_j-1)):(n_sectors+n_sectors*(region_j-1)),
                  (1+n_sectors*(region_j-1)):(n_sectors+n_sectors*(region_j-1))]%*%as.numeric(unlist(D))[(1+n_sectors*(region_j-1)):(n_sectors+n_sectors*(region_j-1))],
                C[(1+n_sectors*(region_i-1)):(n_sectors+n_sectors*(region_i-1)),
                  (1+n_sectors*(region_j-1)):(n_sectors+n_sectors*(region_j-1))]%*%as.numeric(unlist(D))[(1+n_sectors*(region_j-1)):(n_sectors+n_sectors*(region_j-1))])
      }
    }
  };Z
  X=as.vector(L%*%as.numeric(unlist(D)));X=tibble::tibble(total=X)
  ioTbl<-dplyr::bind_cols(Z,D,X)
  sol<-tibble::as_tibble(ioTbl)
  intermediateOutput=tibble::tibble(intermediateOutput=rowSums(Z%>%dplyr::select(-sector)))
  valueAdded=tibble::as_tibble(X-t(as.matrix(rep(1,nrow(Z))))%*%as.matrix(Z%>%dplyr::select(-sector)));names(valueAdded)="valueAdded"
  sol<-sol%>%
    dplyr::mutate(sector=Z$sector)%>%
    dplyr::bind_cols(X,D,intermediateOutput,valueAdded)%>%
    dplyr::select(sector,Z$sector,external,total,intermediateOutput,valueAdded)
  }else{
      sol<-"No change from original solOrig"}

  print(list(A=A,C=C,L=L,Q0=Q0,solOrig=solOrig,sol=sol))

  return(list(A=A,C=C,L=L,Z=Z,Q0=Q0,solOrig=solOrig,sol=sol))

} # Close Function
