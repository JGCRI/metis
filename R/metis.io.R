#' metis.io
#'
#' This function prepares gridded data for use with domestic metis modules.
#' @param dirOutputs Default =paste(getwd(),"/outputs",sep=""),
#' @param Z0 Initial Nexus Flows (i.e. Supply sectors which also have demands).Default = NULL,
#' @param D0 Intiial Other flows. (All other sectors which have demands but do not supply resources). Default = NULL,
#' @param X0 Initial Total Demands. Default = NULL,
#' @param A0 Initial Intensity Matrix. Default = NULL,
#' @param priorityZvsA Default = c(Z0=1, A0=2),
#' @param priorityXvsCap Default = c(X0=1, Cap0=2)
#' @param Import0 Default =NULL,
#' @param Export0 Default =NULL,
#' @param Cap0 Capacity. Default =NULL,
#' @param nameAppend Modified intensity matrix. Default =NULL,
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @export


metis.io<-function(Z0=NULL,
                   D0=NULL,
                   X0=NULL,
                   A0=NULL,
                   priorityZvsA = c(Z0=1, A0=2),
                   priorityXvsCap = c(X0=1, Cap0=2),
                   Import0=NULL,
                   Export0=NULL,
                   Cap0=NULL,
                   dirOutputs=paste(getwd(),"/outputs",sep=""),
                   nameAppend=""
                        ){

  # Z0=NULL
  # D0=NULL
  # X0=NULL
  # A0=NULL
  # priorityZvsA = c(Z0=1, A0=2)
  # priorityXvsCap = c(X0=1, Cap0=2)
  # Import0=NULL
  # Export0=NULL
  # Cap0=NULL
  # dirOutputs=paste(getwd(),"/outputs",sep="")
  # nameAppend=""


#----------------
# Key Assumptions:
#----------------

# 1. Imports
# Imported commodities will be used to satisfy local demands before any local commodities.
# Imported commodities will be equally distributed to each demand in the same ratio as total imports to total demands

# 2. LocalProduction X
# 2a. Z specified witout X
#    If X is not specified and Z (nexus flows are specified) it will be assumed that
#   the nexus flows specified are met by local demand and all other demands can be met by imports.
#   If we replace all local production by imports, nexus flows should be 0, but since this is not the case
#   we set this arbitrary minimum.
# 2a. A specified witout X
#    In this case we allow all local production to be replaced by imports.
#     Nexus flows should then be 0.

#----------------
# Initialize variables by setting to NULL
#----------------

NULL-> colsNumeric -> Z -> D -> A -> X -> Import -> Export -> . ->
    Var1->Var2->Var3->Var4->cap->check->export->import->
    localProduction->nexusTotal->otherTotal->scenario->sector->sectorFrom->
    sectorTo->subRegion->surplus->totalDemands->value->x->year

#----------------
# Choose inputs based on priorityZvsA
#----------------

# The model needs atleast two out of the four key input variables (Z0, A0, X0, D0)
  # If given all four it chooses the top two based on the provided priority
  # Default priority is 1. Z0 2. D0 3. X0 4. A0 (But this can be changed)

if(!all(c("Z0","A0") %in% names(priorityZvsA))){
    print(paste("Incorrect priorityZvsA input column names: ", paste(names(priorityZvsA),collapse=", "),sep=""))
    print(paste("Using default priorityZvsA: ", paste(names(priorityZvsA),collapse=", "),sep=""))
    priorityZvsA = c(Z0=1, A0=2)
    print(priorityZvsA)
}

if(!all(c("X0","Cap0") %in% names(priorityXvsCap))){
    print(paste("Incorrect priorityXvsCap input column names: ", paste(names(priorityZvsA),collapse=", "),sep=""))
    print(paste("Using default priorityXvsCap: ", paste(names(priorityXvsCap),collapse=", "),sep=""))
    priorityXvsCap = c(X0=1, Cap0=2)
    print(priorityXvsCap)
  }

if(is.null(Z0) & is.null(A0)){stop("Need to provide atleast one of Z0, A0")}
if(is.null(X0) & is.null(D0)){stop("Need to provide atleast two of X0, D0")}


if(all(!is.null(Z0) & !is.null(A0))){
  if(priorityZvsA[["Z0"]]>priorityZvsA[["A0"]]){Z=NULL}else{Z=Z0};
  if(priorityZvsA[["A0"]]>priorityZvsA[["Z0"]]){A=NULL}else{A=A0};
  print(paste("Z0 & A0 provided. Only using priority inputs: ", sep=""))
  print(priorityZvsA[priorityZvsA<2])
} else {
  if(!is.null(Z0)){Z=Z0}
  if(!is.null(A0)){A=A0}
}

D=D0;
X=X0;
Import <- Import0;
Export <- Export0;
Cap <- Cap0;


#------------------
# Create Folders if needed
#------------------
if (!dir.exists(dirOutputs)){
    dir.create(dirOutputs)}
if (!dir.exists(paste(dirOutputs, "/IO", sep = ""))){
    dir.create(paste(dirOutputs, "/IO", sep = ""))}
if (!dir.exists(paste(dirOutputs, "/IO/CombSubReg", sep = ""))){
    dir.create(paste(dirOutputs, "/IO/CombSubReg", sep = ""))}

dir<-paste(dirOutputs, "/IO/CombSubReg", sep = "")


#------------------
# Small Internal Functions
# -----------------

addMissing<-function(data){
  if(!"scenario"%in%names(data)){data<-data%>%dplyr::mutate(scenario="scenario")}
  if(!"x"%in%names(data)){if("year"%in%names(data)){
    data<-data%>%dplyr::mutate(x=year)}else{data<-data%>%dplyr::mutate(x=000)}}
  if(!"region"%in%names(data)){data<-data%>%dplyr::mutate(region="region")}
  if(!"subRegion"%in%names(data)){data<-data%>%dplyr::mutate(subRegion="subRegion")}
  if(!"unit"%in%names(data)){data<-data%>%dplyr::mutate(unit="unit")}
  if(!"classPalette"%in%names(data)){data<-data%>%dplyr::mutate(classPalette="pal_hot")}
  if(!"param"%in%names(data)){data<-data%>%dplyr::mutate(param="param")}
  if(!"class"%in%names(data)){data<-data%>%dplyr::mutate(class="class")}
  return(data)
}

addedColumns <- names(addMissing(data.frame()))


#---------

ioTblSolOrganize <- function (solTbl=NULL,colsNumeric=NULL,addedColumns=NULL){

  NULL -> solTblorg -> D_Tmp -> Dorg-> Z_Tmp -> Zorg-> A_Tmp -> Aorg ->
  L_Tmp -> Lorg -> X_Tmp -> Xorg -> ioTbl_Tmp -> ioTbl -> ioTblImports_Tmp ->
  ioTblImports -> sol_Tmp -> solTblorg


  if(length(colsNumeric)>0){


    solTblorg <- solTbl; solTbl%>%as.data.frame()

    ioTbl <- solTblorg %>%
      dplyr::select(sector,unique(solTblorg$sector),colsNumeric,export,totalDemands, import, addedColumns);ioTbl

    importTbl <- solTblorg %>%
      dplyr::select(sector,ioTbl$sector,colsNumeric,export, totalDemands, import, addedColumns) %>%
      dplyr::mutate_at(.funs=dplyr::funs(.*import/totalDemands), .vars=c(ioTbl$sector,colsNumeric,"export","totalDemands")) %>%
      dplyr::mutate(sector=paste("import_",sector,sep="")) %>%
      dplyr::select(-import); importTbl %>%as.data.frame()

    ioTblRemainder <- solTblorg %>%
      dplyr::select(sector,ioTbl$sector,colsNumeric, export, totalDemands, import, addedColumns) %>%
      dplyr::mutate_at(.funs=dplyr::funs(.-(.*import/totalDemands)), .vars=c(ioTbl$sector,colsNumeric,"export","totalDemands")) %>%
      dplyr::select(-import); ioTblRemainder %>%as.data.frame()

    ioTblImports <- ioTblRemainder %>%
      dplyr::bind_rows(importTbl); ioTblImports %>%as.data.frame()

    Dorg = solTblorg %>%
      dplyr::select(sector,colsNumeric,export,otherTotal,addedColumns)

  } else {

    solTblorg <- solTbl

    ioTbl <- solTblorg %>%
      dplyr::select(sector,unique(solTblorg$sector),otherTotal,export, totalDemands, import, addedColumns); ioTbl

    importTbl <- solTblorg %>%
      dplyr::select(sector,ioTbl$sector,otherTotal,export, totalDemands, import, addedColumns) %>%
      dplyr::mutate_at(.funs=dplyr::funs(.*import/totalDemands), .vars=c(ioTbl$sector,"otherTotal","export","totalDemands")) %>%
      dplyr::mutate(sector=paste("import_",sector,sep="")) %>%
      dplyr::select(-import); importTbl %>%as.data.frame()

    ioTblRemainder <- solTblorg %>%
      dplyr::select(sector,ioTbl$sector,otherTotal,export, totalDemands, import, addedColumns) %>%
      dplyr::mutate_at(.funs=dplyr::funs(.-(.*import/totalDemands)), .vars=c(ioTbl$sector,"otherTotal","export","totalDemands")) %>%
      dplyr::select(-import); ioTblRemainder %>%as.data.frame()

    ioTblImports <- ioTblRemainder %>%
      dplyr::bind_rows(importTbl); ioTblImports %>%as.data.frame()

    Dorg = solTblorg %>%
      dplyr::select(sector,export,otherTotal,addedColumns)

  }

  Zorg = solTblorg %>%
    dplyr::select(sector,unique(solTblorg$sector),addedColumns); Zorg
  Xorg = solTblorg %>%
    dplyr::select(sector,totalDemands,localProduction,cap,import,surplus); Xorg
  Aorg <-as.matrix(Zorg%>%dplyr::select(c(unique(Zorg$sector)))) %*% as.matrix(Xorg$localProduction^-1*diag(nrow(Zorg)));Aorg
  Lorg <-solve(diag(nrow(Aorg))-Aorg);
  Aorg <- tibble::as_tibble(Aorg); Lorg <- tibble::as_tibble(Lorg)
  names(Aorg) <- names(Lorg) <- unique(Zorg$sector)
  Aorg <- Aorg %>% dplyr::bind_cols(Zorg %>% dplyr::select(sector,addedColumns));Aorg
  Lorg <- Lorg %>% dplyr::bind_cols(Zorg %>% dplyr::select(sector,addedColumns));Lorg

  D_Tmp <-Dorg;
  Z_Tmp <-Zorg;
  A_Tmp <-Aorg;
  L_Tmp <-Lorg;
  X_Tmp <-Xorg;
  ioTbl_Tmp <-ioTbl;
  ioTblImports_Tmp <-ioTblImports;
  sol_Tmp <- solTblorg %>%
    dplyr::select(c(sector,solTblorg$sector,nexusTotal,colsNumeric, otherTotal, export,
                    totalDemands, cap,import, localProduction, surplus, addedColumns));


  return(list(D_Tmp=D_Tmp,
              Z_Tmp=Z_Tmp,
              A_Tmp=A_Tmp,
              L_Tmp=L_Tmp,
              X_Tmp=X_Tmp,
              ioTbl_Tmp=ioTbl_Tmp,
              ioTblImports_Tmp=ioTblImports_Tmp,
              sol_Tmp=sol_Tmp))

}


  # ------------------
  # Check Columns
  # ------------------
if(T){


  if(!is.null(Z)){
    Z <- addMissing(Z)
    if(!any(grepl("sector",colnames(Z)))){
      stop(print(paste("Column names in Z: ",paste(colnames(Z),collapse=", ")," must include 'sector'.",sep="")))}
    Z <- Z %>% dplyr::arrange (sector)
    sectors <- Z %>% dplyr::select(sector) %>% unique()
    subRegions <- unique(Z$subRegion)
    years <- unique(Z$x)
    scenarios <- unique(Z$scenario)
  }


  if(!is.null(D)){
   if(!any(grepl(c("sector"),colnames(D)))){
      stop(print(paste("Column names in D: ",paste(colnames(D),collapse=", ")," must include 'sector'.",sep="")))}
  if(!any(grepl(c("miscAdjust"),colnames(D)))){
      print(paste("Column names in D: ",paste(colnames(D),collapse=", ")," missing 'miscAdjust'. Adding miscAdjust.",sep=""))
      D <- D %>% dplyr::mutate(miscAdjust=0)}
  # Remove any columns which are not numeric
   colsNumeric <- names(D %>% dplyr::select_if(is.numeric))[!names(D %>% dplyr::select_if(is.numeric)) %in% c("year","x")]
   if(length(colsNumeric)>0){
   D <- D %>%
     dplyr::mutate(otherTotal=rowSums(.[colsNumeric], na.rm=T))
   }
   D <- D %>% dplyr::arrange (sector)
   D <- addMissing(D)
   sectors <- D %>% dplyr::select(sector) %>% unique()
   subRegions <- unique(D$subRegion)
   years <- unique(D$x)
   scenarios <- unique(D$scenario)
   }

  if(!is.null(X)){
    X <- addMissing(X)
    if(!any(grepl("sector",colnames(X)))){
      stop(print(paste("Column names in X: ",paste(colnames(X),collapse=", ")," must include 'sector'.",sep="")))}
    X <- X %>% dplyr::arrange (sector)
    sectors <- X %>% dplyr::select(sector) %>% unique()
    subRegions <- unique(X$subRegion)
    years <- unique(X$x)
    scenarios <- unique(X$scenario)
  }


  if(!is.null(A)){
  if(!any(grepl("sector",colnames(A)))){
    stop(print(paste("Column names in A: ",paste(colnames(A),collapse=", ")," must include 'sector'.",sep="")))}
      A <- addMissing(A)
      A <- A %>% dplyr::arrange (sector)
      sectors <- A %>% dplyr::select(sector) %>% unique()
      subRegions <- unique(A$subRegion)
      years <- unique(A$x)
      scenarios <- unique(A$scenario)
    }


  if(!is.null(Z) & !is.null(D) ){
    if(!identical(unique(Z %>%
                         dplyr::select(sector,unit)),
                  unique(D %>%
                         dplyr::select(sector,unit)))){
      print(Z);print(D)
      stop(print(paste("Z0 and D0 do not have identical sectors and units.",sep="")))
    }}


  if(!is.null(Import)){
    Import <- addMissing(Import)
    if(!any(grepl("sector",colnames(Import)))){
      stop(print(paste("Column names in Import: ",paste(colnames(Import),collapse=", ")," must include 'sector'.",sep="")))}
    if(!any(grepl(c("import"),colnames(Import)))){
      stop(print(paste("Column names in Import: ",paste(colnames(Import),collapse=", "),"must include 'import' where import represents imports by sector.",sep="")))}
    Import <- Import %>% dplyr::arrange (sector)
  } else {
    Import <- expand.grid(sectors$sector,years,scenarios,subRegions) %>%
      dplyr::rename(sector=Var1,x=Var2,scenario=Var3,subRegion=Var4) %>%
        dplyr::mutate(import=0)
  }

  if(!is.null(Export)){
    Export <- addMissing(Export)
    if(!any(grepl("sector",colnames(Export)))){
      stop(print(paste("Column names in Export: ",paste(colnames(Export),collapse=", ")," must include 'sector'.",sep="")))}
    if(!any(grepl(c("export"),colnames(Export)))){
      stop(print(paste("Column names in Export: ",paste(colnames(Export),collapse=", "),"must include 'export' where export represents exports by sector.",sep="")))}
    Export <- Export %>% dplyr::arrange (sector)
  } else {
    Export <- expand.grid(sectors$sector,years,scenarios,subRegions) %>%
      dplyr::rename(sector=Var1,x=Var2,scenario=Var3,subRegion=Var4) %>%
      dplyr::mutate(export=0)
  }


    if(!is.null(Cap)){
      Cap <- addMissing(Cap)
      if(!any(grepl("sector",colnames(Cap)))){
        stop(print(paste("Column names in Cap: ",paste(colnames(Cap),collapse=", ")," must include 'sector'.",sep="")))}
      if(!any(grepl("cap",colnames(Cap)))){
        stop(print(paste("Column names in Cap: ",paste(colnames(Cap),collapse=", ")," must include 'cap'.",sep="")))}
      Cap <- Cap %>% dplyr::arrange (sector)
      sectors <- Cap %>% dplyr::select(sector) %>% unique()
    } else {
      Cap <- expand.grid(sectors$sector,years,scenarios,subRegions) %>%
        dplyr::rename(sector=Var1,x=Var2,scenario=Var3,subRegion=Var4) %>%
        dplyr::mutate(cap=0)
    }


  # Adjust X and Cap based on priorities provided

  if(!is.null(Cap) & !is.null(X)){
    checkIX <- X %>%
      dplyr::left_join(Cap) %>%
      dplyr::mutate(check = cap - localProduction); checkIX

    if(any(checkIX$check < 0)){
      print(paste("Local production X0 provided is less than capacity provided Cap0.",sep=""))


    if(priorityXvsCap[["X0"]]<priorityXvsCap[["Cap0"]]){

      print(paste("Using X0 based on priorityXvsCap: ", sep=""))
        print(priorityXvsCap)

      print(paste("Adjusting Capacity to atleast local production values. Adjusted Cap = ",sep=""))
      Cap <- checkIX %>%
        dplyr::mutate(cap = dplyr::case_when(check<0 ~ (localProduction),
                                               TRUE ~ cap)) %>%
        dplyr::select(-localProduction,-check)
      print(Cap)
    } else {
      print(paste("Using Cap0 based on priorityXvsCap: ", sep=""))
      print(priorityXvsCap)

      print(paste("Adjusting localProduction X0 to Cap values. Adjusted X0 = ",sep=""))
      X <- checkIX %>%
        dplyr::mutate(localProduction = dplyr::case_when(check<0 ~ (cap),
                                      TRUE ~ localProduction)) %>%
        dplyr::select(-cap,-check)
      print(X)
    }

    }
    }

}


if(length(colsNumeric)<1){colsNumeric=NULL}

#------------------------------------
# For each year, suregion and scenario
#------------------------------------

tibble::tibble()->A_Output -> L_Output->sol_Output->ioTbl_Output -> ioTblImports_Output

for(scenario_i in scenarios){
  for(subRegion_i in subRegions){
    for(year_i in years){


print(paste("Solving for scenario: ", scenario_i, ", year:", year_i, " and sub-region:", subRegion_i," ...",sep=""))


      # Subset Data (D0i,X0i,A0i,Z0i,Cap0i,Import0i,Export0i)
      NULL -> D0i ->  X0i -> A0i -> Z0i -> Cap0i -> Import0i -> Export0i

      if(T){
      #if(!is.null(D0imp)){D0i<-D0imp %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(D)){D0i<-D %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(X)){X0i<-X %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(A)){A0i<-A %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(Z)){Z0i<-Z %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(Cap)){Cap0i<-Cap %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(Import)){Import0i<-Import %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(Export)){Export0i<-Export %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      }


  #------------------------------------
  # Calculate flows based on data provided
  #------------------------------------


    if(!is.null(Z0i)){

      print("Calculating IO table based on Z0.")

      Zi      <- Z0i %>% dplyr::mutate(nexusTotal= Z0i %>% dplyr::select(c(Z0i$sector)) %>% rowSums); Zi
      Capi    <- Cap0i %>% dplyr::select(sector,cap); Capi
      Importi <- Import0i %>% dplyr::select(sector,import); Importi
      Exporti <- Export0i %>% dplyr::select(sector,export); Exporti


      if(!is.null(D0i) & !is.null(X0i)){
      Di      <- D0i %>% dplyr::select(sector,colsNumeric,otherTotal); Di
      Xi      <- X0i %>% dplyr::select(sector,localProduction); Xi
      } else {
        if(!is.null(D0i)){
          Di      <- D0i %>% dplyr::select(sector,colsNumeric,otherTotal); Di
          Xi      <- Di %>%
            dplyr::left_join(Zi) %>%
            dplyr::left_join(Capi) %>%
            dplyr::left_join(Importi) %>%
            dplyr::left_join(Exporti) %>%
            dplyr::mutate(localProduction = pmin(cap,pmax((nexusTotal+otherTotal+export-import),nexusTotal))) %>%
            dplyr::select(sector,localProduction); Xi %>% as.data.frame()

        }
        if(!is.null(X0i)){
          Xi      <- X0i %>% dplyr::select(sector,localProduction); Xi
          Di      <- Xi %>%
            dplyr::left_join(Zi) %>%
            dplyr::left_join(Capi) %>%
            dplyr::left_join(Importi) %>%
            dplyr::left_join(Exporti) %>%
            dplyr::mutate(otherTotal = pmax(localProduction - nexusTotal - (export-import),0)) %>%
            dplyr::select(sector,otherTotal); Di %>% as.data.frame()
        }
      }

      solTbl <- Zi %>%
        dplyr::left_join(Di) %>%
        dplyr::left_join(Xi) %>%
        dplyr::left_join(Capi) %>%
        dplyr::left_join(Importi) %>%
        dplyr::left_join(Exporti) %>%
        dplyr::mutate(
          export = dplyr::case_when((localProduction-nexusTotal-otherTotal)>export~export+(localProduction-nexusTotal-otherTotal),
                             TRUE~export),
          export = dplyr::case_when(import>export~import,
                             TRUE~export),
          totalDemands = nexusTotal + otherTotal + export,
          import = pmax(export-(localProduction-nexusTotal-otherTotal),0),
          surplus = cap-localProduction) %>%
        addMissing(); solTbl %>% as.data.frame

      tmpSol <- ioTblSolOrganize(solTbl,colsNumeric,addedColumns)
      tmpSol$sol_Tmp%>%as.data.frame
      tmpSol$ioTblImports_Tmp %>%as.data.frame
      tmpSol$A_Tmp
      tmpSol$Z_Tmp
      tmpSol$X_Tmp
      tmpSol$D_Tmp
    }


  if(!is.null(A0i)){

    print("Calculating IO table based on A0.")

    Ai <- as.matrix(A0i%>%dplyr::select(c(A0i$sector))); Ai
    Li <- solve(diag(nrow(Ai))-Ai); Li
    Capi    <- Cap0i %>% dplyr::select(sector,cap); Capi
    Importi <- Import0i %>% dplyr::select(sector,import); Importi
    Exporti <- Export0i %>% dplyr::select(sector,export); Exporti


    if(!is.null(D0i) & !is.null(X0i)){
      Di <- D0i %>% dplyr::select(sector,colsNumeric,otherTotal); Di
      Xi <- X0i %>% dplyr::select(sector,localProduction); Xi
      Zi <- Ai %*% diag(as.vector(t(as.matrix(Xi$localProduction))))
      Zi <- dplyr::bind_cols(Xi%>%dplyr::select(sector),tibble::as_tibble(Zi))
      names(Zi)<-c("sector",Xi$sector)
      Zi <- Zi %>%
        dplyr::mutate(nexusTotal= Zi %>%
                        dplyr::select(c(Zi$sector)) %>%
                        rowSums); Zi
    } else {
      if(!is.null(D0i)){
        Di      <- D0i %>% dplyr::select(sector,colsNumeric,otherTotal); Di
        # With just D0, Cap, Imports and Exports, we make an initial assumption of the localProduction
        # Then we calculate the nexus flows required for this localProduction and then we add those to the total
        Xi      <- Di %>%
          dplyr::left_join(Capi) %>%
          dplyr::left_join(Importi) %>%
          dplyr::left_join(Exporti) %>%
          dplyr::mutate(localProduction = pmin(pmax((otherTotal + export - import),0),cap)) %>%
          dplyr::select(sector,localProduction); Xi %>% as.data.frame()
        Zi <- Ai %*% diag(as.vector(t(as.matrix(Xi$localProduction))))
        Zi <- dplyr::bind_cols(Xi%>%dplyr::select(sector),tibble::as_tibble(Zi))
        names(Zi)<-c("sector",Xi$sector)
        Zi <- Zi %>%
          dplyr::mutate(nexusTotal= Zi %>%
                          dplyr::select(c(Zi$sector)) %>%
                          rowSums); Zi

      }
      if(!is.null(X0i)){

        Xi      <- X0i %>% dplyr::select(sector,localProduction); Xi

        Zi <- Ai %*% diag(as.vector(t(as.matrix(Xi$localProduction))))
        Zi <- dplyr::bind_cols(Xi%>%dplyr::select(sector),tibble::as_tibble(Zi))
        names(Zi)<-c("sector",Xi$sector)
        Zi <- Zi %>%
          dplyr::mutate(nexusTotal= Zi %>%
                          dplyr::select(c(Zi$sector)) %>%
                          rowSums); Zi

        Di      <- Xi %>%
          dplyr::left_join(Zi) %>%
          dplyr::left_join(Capi) %>%
          dplyr::left_join(Importi) %>%
          dplyr::left_join(Exporti) %>%
          dplyr::mutate(otherTotal = pmax(localProduction - nexusTotal - (export-import),0)) %>%
          dplyr::select(sector,otherTotal); Di %>% as.data.frame()
      }
    }

    solTbl <- Zi %>%
      dplyr::left_join(Di) %>%
      dplyr::left_join(Xi) %>%
      dplyr::left_join(Capi) %>%
      dplyr::left_join(Importi) %>%
      dplyr::left_join(Exporti) %>%
      dplyr::mutate(
        export = dplyr::case_when((localProduction-nexusTotal-otherTotal)>export~export+(localProduction-nexusTotal-otherTotal),
                           TRUE~export),
        export = dplyr::case_when(import>export~import,
                           TRUE~export),
        totalDemands = nexusTotal + otherTotal + export,
        import = pmax(export-(localProduction-nexusTotal-otherTotal),0),
        surplus = cap-localProduction) %>%
      addMissing(); solTbl %>% as.data.frame

    tmpSol <- ioTblSolOrganize(solTbl,colsNumeric,addedColumns)
    tmpSol$sol_Tmp%>%as.data.frame
    tmpSol$ioTblImports_Tmp %>%as.data.frame
    tmpSol$A_Tmp
    tmpSol$Z_Tmp
    tmpSol$X_Tmp
    tmpSol$D_Tmp
  }




      # Prepare final data list

      A_Output_tmp = tmpSol$A_Tmp
      L_Output_tmp = tmpSol$L_Tmp
      ioTbl_Output_tmp = tmpSol$ioTbl_Tmp
      ioTblImports_Output_tmp = tmpSol$ioTblImports_Tmp
      sol_Output_tmp = tmpSol$sol_Tmp


     if(TRUE){
      A_Output = A_Output %>% dplyr::bind_rows(A_Output_tmp%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      L_Output = L_Output %>% dplyr::bind_rows(L_Output_tmp%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      ioTbl_Output = ioTbl_Output %>% dplyr::bind_rows(ioTbl_Output_tmp%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      ioTblImports_Output = ioTblImports_Output %>% dplyr::bind_rows(ioTblImports_Output_tmp%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      sol_Output = sol_Output %>% dplyr::bind_rows(sol_Output_tmp%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
     }

    } # Close loop scenario
  } # close loop subRegion
} # close loop year

    sol_list<-list(A_Output=A_Output,
                   L_Output=L_Output,
                   ioTbl_Output=ioTbl_Output,
                   ioTblImports_Output=ioTblImports_Output,
                   sol_Output=sol_Output
    )

  sol_list<-sol_list[sapply(sol_list, function(x) dim(x)[1]) > 0]


  #-----------------------
  # IO Visualization (Eventually Move this to separate function?)
  #-----------------------


  # Print Figure Function
  printf <- function(printFig=T,
                     fileName="file",
                     dir,
                     figure,
                     figWidth=13,
                     figHeight=9,
                     pdfpng="png"){
    if(printFig!=F){
    fname<-paste(fileName,sep="")
    if(!dir.exists(dir)){
      print(paste("directory provided: ",dir," does not exist. Saving to: ", getwd(),sep=""))
      dir=getwd()}else{
        metis.printPdfPng(figure=figure,
                          dir=dir,
                          filename=fname,
                          figWidth=figWidth,
                          figHeight=figHeight,
                          pdfpng=pdfpng)

        print(paste("Figure saved as: ",fileName,".",pdfpng," in folder: ", paste(dirOutputs,sep=""),sep=""))
      }}else{print("printFig set to F so no figure will be saved.")}
  }



  for (scenario_i in scenarios){

    #---------------------
    # sol_Output
    #---------------------


  # A Intensity Matrix

  A_mat <- sol_list$A_Output %>%
    dplyr::filter(scenario==scenario_i); A_mat

  A_matx <- A_mat %>%
    tidyr::gather(-c("sector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=sector) %>%
    dplyr::arrange(sectorFrom) %>%
    dplyr::filter(!is.nan(value),!is.infinite(value),value!=0, !is.na(value)); A_matx

  sectorFromOrder <- sort(unique(A_matx$sectorFrom)); sectorFromOrder
  sectorToOrder <-  sectorFromOrder; sectorToOrder


  # A Intensity Matrix

  if(nrow(A_matx)>0){

  fname = paste(scenario_i,"_A_abs_bubble",nameAppend,sep="")
  ga <- ggplot(A_matx, aes(y = sectorFrom, x = sectorTo)) +
    theme_bw() +
    labs(title=fname) +
    geom_point(data=A_matx%>%dplyr::filter(sectorTo!="totalDemands"),aes(col=value, size=value)) +
    scale_color_gradient(low = "white", high = "indianred1", guide="none") +
    geom_text(aes(label=round(value,2)),col="black", size=5) +
    coord_fixed(ratio = 1) +
    scale_x_discrete(limits = sectorToOrder, expand = c(0.1,0.1)) +
    scale_y_discrete(limits = rev(sectorFromOrder), expand = c(0.1,0.1)) +
    scale_size_continuous(range = c(1,20), guide="none") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 15),
          axis.text.y = element_text(size = 15),
          strip.text  = element_text(size = 15),
          axis.title = element_text(size = 15))+
    facet_grid(x~subRegion); ga

  printf(figure=ga,fileName = fname, dir=dir)

  }
  # ioTable normalized

  sol<-sol_list$ioTblImports_Output %>%
    dplyr::filter(scenario==scenario_i)


  df_Mn<-sol %>%
    #dplyr::select (-localProduction,localProduction) %>% # to place localProduction last for following function
    dplyr::mutate_at(vars(-c("sector",addedColumns[addedColumns %in% names(sol)])),dplyr::funs(./totalDemands)); df_Mn

  solx <- sol %>%
    tidyr::gather(-c("sector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=sector) %>%
    dplyr::arrange(sectorFrom)  %>%
    dplyr::filter(!is.nan(value),value!=0, !is.na(value));solx


  df_Mnx <- df_Mn %>%
    tidyr::gather(-c("sector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=sector) %>%
    dplyr::arrange(sectorFrom) %>%
    dplyr::filter(!is.nan(value),!is.infinite(value),value!=0, !is.na(value)); df_Mnx

  sectorFromOrder <- NULL
  sectorFromOrder_local <- sort(unique(df_Mnx$sectorFrom)[!grepl("import_",unique(df_Mnx$sectorFrom))]); sectorFromOrder_local
  sectorFromOrder_import <- sort(unique(df_Mnx$sectorFrom)[grepl("import_",unique(df_Mnx$sectorFrom))]); sectorFromOrder_import
  for(i in (1:max(length(sectorFromOrder_local),length(sectorFromOrder_import)))) {
    if(length(sectorFromOrder_local)>=i){sectorFromOrder <- c(sectorFromOrder,sectorFromOrder_local[i])}
    if(length(sectorFromOrder_import)>=i){sectorFromOrder <- c(sectorFromOrder,sectorFromOrder_import[i])}
  }; sectorFromOrder

  sectorToOrder <-
    c(sort(unique(df_Mnx$sectorTo)[unique(df_Mnx$sectorTo) %in% unique(sol$sector)]),
      sort(unique(df_Mnx$sectorTo)[!unique(c(df_Mnx$sectorTo)) %in% c(unique(sol$sector),"otherTotal","export","totalDemands")]),
      unique(df_Mnx$sectorTo)[unique(c(df_Mnx$sectorTo)) %in% c("otherTotal")],
      unique(df_Mnx$sectorTo)[unique(c(df_Mnx$sectorTo)) %in% c("export")],
      unique(df_Mnx$sectorTo)[unique(c(df_Mnx$sectorTo)) %in% c("totalDemands")]
    ); sectorToOrder


  # ioTable normalized bubbles

  if(nrow(df_Mnx)>0){

  fname = paste(scenario_i,"_solOrig1_flowTbl_norm_bubble",nameAppend,sep="")
  ga <- ggplot(df_Mnx, aes(y = sectorFrom, x = sectorTo)) +
    theme_bw() +
    labs(title=fname) +
    geom_point(data=df_Mnx%>%dplyr::filter(sectorTo!="totalDemands"),aes(col=value, size=value)) +
    scale_color_gradient(low = "white", high = "indianred1", guide="none") +
    geom_text(aes(label=round(value,2)),col="black", size=5) +
    coord_fixed(ratio = 1) +
    scale_x_discrete(limits = sectorToOrder, expand = c(0.1,0.1)) +
    scale_y_discrete(limits = rev(sectorFromOrder), expand = c(0.1,0.1)) +
    scale_size_continuous(range = c(1,20), guide="none") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 15),
          axis.text.y = element_text(size = 15),
          strip.text  = element_text(size = 15),
          axis.title = element_text(size = 15))+
    facet_grid(x~subRegion); ga

  printf(figure=ga,fileName = fname, dir=dir)
  }

  # ioTable absolute bubbles

  if(nrow(solx)>0){

  fname = paste(scenario_i,"_solOrig1_flowTbl_abs_bubble",nameAppend,sep="")
  ga <- ggplot(solx, aes(y = sectorFrom, x = sectorTo)) +
    theme_bw() +
    labs(title=fname) +
    geom_point(data=df_Mnx%>%dplyr::filter(sectorTo!="totalDemands"),aes(col=value, size=value)) +
    scale_color_gradient(low = "white", high = "indianred1", guide="none") +
    geom_text(aes(label=round(value,2)),col="black", size=5) +
    coord_fixed(ratio = 1) +
    scale_x_discrete(limits = sectorToOrder, expand = c(0.1,0.1)) +
    scale_y_discrete(limits = rev(sectorFromOrder), expand = c(0.1,0.1)) +
    scale_size_continuous(range = c(1,20), guide="none") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 15),
          axis.text.y = element_text(size = 15),
          strip.text  = element_text(size = 15),
          axis.title = element_text(size = 15))+
    facet_grid(x~subRegion); ga

  printf(figure=ga,fileName = fname, dir=dir)

  }


  #-----------------
  # Sankey
  #--------------

  if(any(!is.na(sol %>%
     dplyr::select(colsNumeric[!colsNumeric %in% c("totalDemands","localTotal","otherTotal","x",addedColumns)])))){
  solFlows <- sol %>%
    dplyr::select(-!!names(sol)[names(sol) %in% c("totalDemands","localTotal","otherTotal")])} else {
      solFlows <- sol %>%
        dplyr::select(-!!names(sol)[names(sol) %in% c("totalDemands","localTotal")])
    }
  df <- solFlows;df

  dfx <- df %>%
    tidyr::gather(-c("sector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=sector) %>%
    dplyr::filter(value>0) %>%
    unique() %>%
    dplyr::arrange(sectorFrom); dfx

  sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx$sectorFrom)]; sectorFromOrderSankey
  sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx$sectorTo)]; sectorToOrderSankey

  dfx$sectorFrom <- factor( as.character(dfx$sectorFrom), levels=sectorFromOrderSankey )
  dfx$sectorTo <- factor( as.character(dfx$sectorTo), levels=sectorToOrderSankey )

  if(all(unique(dfx$sectorFrom) %in% names(metis.colors()$pal_sankey))){
    fillcolors = metis.colors()$pal_sankey} else {
      fillcolors = metis.colors()$pal_Basic
    }; fillcolors


  if(nrow(dfx)>0){

  fname = paste(scenario_i,"_solOrig1_sankey_abs_DisAggDemands",nameAppend,sep="")
  g<-ggplot(as.data.frame(dfx%>%dplyr::filter(value!=0)),
            aes(y = value, axis1 = sectorFrom, axis2 = sectorTo, group=subRegion)) +
    theme_bw() +
    ggalluvial::geom_alluvium(aes(fill = sectorFrom), width = 1/12, color="black", alpha=0.6) +
    ggalluvial::geom_stratum(width = 1/12, fill = "grey30", color = "grey", alpha=1) +
    geom_label(stat = "stratum", label.strata = TRUE) +
    scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
    #scale_fill_brewer(type = "qual", palette = "Set1", name="From") +
    scale_fill_manual(values=fillcolors, name="From") +
    facet_grid(x~subRegion) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 15),
          axis.text.y = element_text(size = 15),
          strip.text  = element_text(size = 15),
          axis.title = element_text(size = 15))+
    ggtitle(fname);g


  printf(figure=g,fileName = fname, dir=dir)

  }


  #-------- Aggregated Demands

  solFlows <- sol %>%
    dplyr::select(!!c("sector",names(sol)[names(sol) %in% c("totalDemands",addedColumns)]))
  df <- solFlows;df

  dfx <- df %>%
    tidyr::gather(-c("sector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=sector) %>%
    dplyr::filter(value>0) %>%
    unique() %>%
    dplyr::arrange(sectorFrom); dfx

  sectorFromOrderSankey <- sectorFromOrder[sectorFromOrder %in% unique(dfx$sectorFrom)]; sectorFromOrderSankey
  sectorToOrderSankey <- sectorToOrder[sectorToOrder %in% unique(dfx$sectorTo)]; sectorToOrderSankey

  dfx$sectorFrom <- factor( as.character(dfx$sectorFrom), levels=sectorFromOrderSankey )
  dfx$sectorTo <- factor( as.character(dfx$sectorTo), levels=sectorToOrderSankey )

  if(all(unique(dfx$sectorFrom) %in% names(metis.colors()$pal_sankey))){
    fillcolors = metis.colors()$pal_sankey} else {
      fillcolors = metis.colors()$pal_Basic
    }; fillcolors

  if(nrow(dfx)>0){

  fname = paste(scenario_i,"_solOrig1_sankey_abs_AggDemands",nameAppend,sep="")
  g<-ggplot(as.data.frame(dfx%>%dplyr::filter(value!=0)),
            aes(y = value, axis1 = sectorFrom, axis2 = sectorTo, group=subRegion)) +
    theme_bw() +
    ggalluvial::geom_alluvium(aes(fill = sectorFrom), width = 1/12, color="black", alpha=0.6) +
    ggalluvial::geom_stratum(width = 1/12, fill = "grey30", color = "grey", alpha=1) +
    geom_label(stat = "stratum", label.strata = TRUE) +
    scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
    #scale_fill_brewer(type = "qual", palette = "Set1", name="From") +
    scale_fill_manual(values=fillcolors, name="From") +
    facet_grid(x~subRegion) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 15),
          axis.text.y = element_text(size = 15),
          strip.text  = element_text(size = 15),
          axis.title = element_text(size = 15))+
    ggtitle(fname);g


  printf(figure=g,fileName = fname, dir=dir)
  }


  } # For Scenario i

  return(sol_list)

} # Close Function
