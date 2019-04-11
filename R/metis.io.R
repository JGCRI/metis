#' metis.io
#'
#' This function prepares gridded data for use with domestic metis modules.
#' @param dirOutputs Default =paste(getwd(),"/outputs",sep=""),
#' @param Z0 Initial Nexus Flows (i.e. Supply sectors which also have demands).Default = NULL,
#' @param D0 Intiial Other flows. (All other sectors which have demands but do not supply resources). Default = NULL,
#' @param X0 Initial Total Demands. Default = NULL,
#' @param A0 Initial Intensity Matrix. Default = NULL,
#' @param Import0 Default =NULL,
#' @param Cap0 Capacity. Default =NULL,
#' @param ZPartial Nexus flows to replace in calibrated dataset. Default =NULL,
#' @param DNew Modified other flows. Default =NULL,
#' @param XNew Modified total demands. Default =NULL,
#' @param ZNew Modified Nexus flows. Default =NULL,
#' @param ANew Modified intensity matrix. Default =NULL,
#' @param nameAppend Modified intensity matrix. Default =NULL,
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @export


metis.io<-function(Z0=NULL,
                   D0=NULL,
                   X0=NULL,
                   A0=NULL,
                   Import0=NULL,
                   Cap0=NULL,
                   DNew=NULL,
                   XNew=NULL,
                   ZNew=NULL,
                   ANew=NULL,
                   ZPartial=NULL,
                   dirOutputs=paste(getwd(),"/outputs",sep=""),
                   nameAppend=""
                        ){

  # Z0=NULL
  # D0=NULL
  # X0=NULL
  # A0=NULL
  # Import0=NULL
  # Cap0=NULL
  # DNew=NULL
  # XNew=NULL
  # ZNew=NULL
  # ANew=NULL
  # ZPartial=NULL
  # dirOutputs=paste(getwd(),"/outputs",sep="")
  # nameAppend=""

#----------------
# Initialize variables by setting to NULL
#----------------

NULL->year->sector->domestic->otherTotal->export->processed->V1->value->.->production->
    sol_Orig->Z->A->X->D-> classPalette -> nexusTotal -> param -> region -> scenario -> x ->
    Var1 -> Var2 -> Var3 -> Var4 -> cap -> sectorFrom -> sectorTo -> subRegion -> surplus -> import ->
    valueP ->
    A_Orig -> L_Orig -> sol_Orig -> ioTbl_Orig -> ioTblImports_Orig ->
    A_ANew -> L_ANew -> sol_ANew ->
    A_DNew -> L_DNew -> sol_DNew ->
    A_ZNew -> L_ZNew -> sol_ZNew ->
    A_XNew -> L_XNew -> sol_XNew ->
    A_Calibratedi -> L_Calibratedi -> sol_Calibratedi->
    A_ANewi -> L_ANewi -> sol_ANewi ->
    A_DNewi -> L_DNewi -> sol_DNewi ->
    A_ZNewi -> L_ZNewi -> sol_ZNewi ->
    A_XNewi -> L_XNewi -> sol_XNewi ->
    A_Calibrated -> L_Calibrated -> sol_Calibrated->ioTbl_Calibrated -> ioTblImports_Calibrated ->
    X0iD0i->ioTblNewi->
    D0i->X0i->A0i->Z0i->Cap0i->Import0i->XNewi->DNewi->ANewi->ZNewi->ZPartiali->sol_list->colsNumeric->
    totalDemands -> nexusTotali -> importi -> D_Orig -> D0imp


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

ioTblSolOrganize <- function (solTbl,D0i,colsNumeric,addedColumns){

  NULL -> solTblorg -> D_Tmp ->Dorg->
    Z_Tmp ->Zorg->A_Tmp ->Aorg->
    L_Tmp ->Lorg->
    X_Tmp ->Xorg->
    ioTbl_Tmp ->ioTbl->
    ioTblImports_Tmp ->ioTblImports->
    sol_Tmp -> solTblorg


  if(!is.null(D0i) & length(colsNumeric)>0){


    solTblorg <- solTbl %>% dplyr::left_join(D0i %>% dplyr::select(sector,colsNumeric))

    ioTbl <- solTblorg %>%
      dplyr::select(sector,unique(solTblorg$sector),colsNumeric,totalDemands,addedColumns);ioTbl

    importTbl <- solTblorg %>%
      dplyr::select(sector,Z$sector,colsNumeric,totalDemands, import, addedColumns) %>%
      dplyr::mutate_at(.funs=dplyr::funs(.*import/totalDemands), .vars=c(Z$sector,colsNumeric,"totalDemands")) %>%
      dplyr::mutate(sector=paste("import_",sector,sep="")) %>%
      dplyr::select(-import); importTbl

    ioTblRemainder <- solTblorg %>%
      dplyr::select(sector,Z$sector,colsNumeric,totalDemands, import, addedColumns) %>%
      dplyr::mutate_at(.funs=dplyr::funs(.-(.*import/totalDemands)), .vars=c(Z$sector,colsNumeric,"totalDemands")) %>%
      dplyr::select(-import); ioTblRemainder

    ioTblImports <- ioTblRemainder %>%
      dplyr::bind_rows(importTbl); ioTblImports

    Dorg = solTblorg %>%
      dplyr::select(sector,colsNumeric,otherTotal,addedColumns)

  } else {

    solTblorg <- solTbl

    ioTbl <- solTblorg %>%
      dplyr::select(sector,unique(solTblorg$sector),otherTotal,totalDemands, addedColumns); ioTbl

    importTbl <- solTblorg %>%
      dplyr::select(sector,Z$sector,otherTotal,totalDemands, import, addedColumns) %>%
      dplyr::mutate_at(.funs=dplyr::funs(.*import/totalDemands), .vars=c(Z$sector,"otherTotal","totalDemands")) %>%
      dplyr::mutate(sector=paste("import_",sector,sep="")) %>%
      dplyr::select(-import); importTbl

    ioTblRemainder <- solTblorg %>%
      dplyr::select(sector,Z$sector,otherTotal,totalDemands, import, addedColumns) %>%
      dplyr::mutate_at(.funs=dplyr::funs(.-(.*import/totalDemands)), .vars=c(Z$sector,"otherTotal","totalDemands")) %>%
      dplyr::select(-import); ioTblRemainder

    ioTblImports <- ioTblRemainder %>%
      dplyr::bind_rows(importTbl); ioTblImports

    Dorg = solTblorg %>%
      dplyr::select(sector,otherTotal,addedColumns)

  }

  Zorg = solTblorg %>%
    dplyr::select(sector,unique(solTblorg$sector),addedColumns)
  Xorg = solTblorg %>%
    dplyr::select(sector,totalDemands,processed,cap,import,surplus)
  Aorg <-as.matrix(Zorg%>%dplyr::select(c(unique(Zorg$sector)))) %*% as.matrix(Xorg$processed^-1*diag(nrow(Zorg)))
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
    dplyr::select(c(sector,solTblorg$sector,nexusTotal,colsNumeric, otherTotal,
                    totalDemands, cap,import, processed, surplus, addedColumns));


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

  if(is.null(Z0) & is.null(D0) & is.null(X0) & is.null(A0)){stop("Need to provide atleast two of Z0, X0, D0, A0")}


  if(!is.null(Z0)){
    Z0 <- addMissing(Z0)
    if(!any(grepl("sector",colnames(Z0)))){
      stop(print(paste("Column names in Z0: ",paste(colnames(Z0),collapse=", ")," must include 'sector'.",sep="")))}
    Z0 <- Z0 %>% dplyr::arrange (sector)
    sectors <- Z0 %>% dplyr::select(sector) %>% unique()
    subRegions <- unique(Z0$subRegion)
    years <- unique(Z0$x)
    scenarios <- unique(Z0$scenario)
  }

  if(!is.null(ZNew)){
    ZNew <- addMissing(ZNew)
    if(!any(grepl("sector",colnames(ZNew)))){
      stop(print(paste("Column names in ZNew: ",paste(colnames(ZNew),collapse=", ")," must include 'sector'.",sep="")))}
    ZNew <- ZNew %>% dplyr::arrange (sector)
    }

  if(!is.null(ZPartial)){
    ZPartial <- addMissing(ZPartial)
      if(!any(grepl("sector",colnames(ZPartial)))){
    stop(print(paste("Column names in ZPartial: ",paste(colnames(ZPartial),collapse=", ")," must include 'sector'.",sep="")))}
    ZPartial <- ZPartial %>% dplyr::arrange (sector)
    }

  if(!is.null(D0)){
   if(!any(grepl(c("sector"),colnames(D0)))){
      stop(print(paste("Column names in D0: ",paste(colnames(D0),collapse=", ")," must include 'sector'.",sep="")))}
  if(!any(grepl(c("miscAdjust"),colnames(D0)))){
      print(paste("Column names in D0: ",paste(colnames(D0),collapse=", ")," must include 'sector'.",sep=""))
      D0 <- D0 %>% dplyr::mutate(miscAdjust=0)}
   # Remove any columns which are not numeric
   colsNumeric <- names(D0 %>% dplyr::select_if(is.numeric))[!names(D0 %>% dplyr::select_if(is.numeric)) %in% c("year","x")]
   if(length(colsNumeric)>0){
   D0 <- D0 %>%
     dplyr::mutate(otherTotal=rowSums(.[colsNumeric], na.rm=T))
   }
   D0 <- D0 %>% dplyr::arrange (sector)
   D0 <- addMissing(D0)
   sectors <- D0 %>% dplyr::select(sector) %>% unique()
   subRegions <- unique(D0$subRegion)
   years <- unique(D0$x)
   scenarios <- unique(D0$scenario)
   }

  if(!is.null(DNew)){
    if(!any(grepl(c("sector"),colnames(DNew)))){
      stop(print(paste("Column names in DNew: ",paste(colnames(DNew),collapse=", ")," must include 'sector'.",sep="")))}
    # Remove any columns which are not numeric
    if(!any(grepl(c("miscAdjust"),colnames(DNew)))){
      print(paste("Column names in D0: ",paste(colnames(D0),collapse=", ")," must include 'sector'.",sep=""))
      DNew <- DNew %>% dplyr::mutate(miscAdjust=0)}
    colsNumericNew <- names(D0 %>% dplyr::select_if(is.numeric))[!names(D0 %>% dplyr::select_if(is.numeric)) %in% c("year","x")]
    if(length(colsNumericNew)>0){
      DNew <- DNew %>%
        dplyr::mutate(otherTotal=rowSums(.[colsNumericNew], na.rm=T))
    }
    DNew <- addMissing(DNew)
    DNew <- DNew %>% dplyr::arrange (sector)
    }


  if(!is.null(X0)){
    X0 <- addMissing(X0)
    if(!any(grepl("sector",colnames(X0)))){
      stop(print(paste("Column names in X0: ",paste(colnames(X0),collapse=", ")," must include 'sector'.",sep="")))}
    X0 <- X0 %>% dplyr::arrange (sector)
    sectors <- X0 %>% dplyr::select(sector) %>% unique()
    subRegions <- unique(X0$subRegion)
    years <- unique(X0$x)
    scenarios <- unique(X0$scenario)
  }

  if(!is.null(XNew)){
    XNew <- addMissing(XNew)
    if(!any(grepl("sector",colnames(XNew)))){
      stop(print(paste("Column names in XNew: ",paste(colnames(XNew),collapse=", ")," must include 'sector'.",sep="")))}
    XNew <- XNew %>% dplyr::arrange (sector)
  }


  if(!is.null(A0)){
  if(!any(grepl("sector",colnames(A0)))){
    stop(print(paste("Column names in ANew: ",paste(colnames(ANew),collapse=", ")," must include 'sector'.",sep="")))}
      A0 <- addMissing(A0)
      A0 <- A0 %>% dplyr::arrange (sector)
      sectors <- A0 %>% dplyr::select(sector) %>% unique()
      subRegions <- unique(A0$subRegion)
      years <- unique(A0$x)
      scenarios <- unique(A0$scenario)
    }


  if(!is.null(ANew)){
    ANew <- addMissing(ANew)
    if(!any(grepl("sector",colnames(ANew)))){
      stop(print(paste("Column names in ANew: ",paste(colnames(ANew),collapse=", ")," must include 'sector'.",sep="")))}
    ANew <- ANew %>% dplyr::arrange (sector)
  }

  if(!is.null(Z0) & !is.null(D0) ){
    if(!identical(unique(Z0 %>%
                         dplyr::select(sector,unit)),
                  unique(D0 %>%
                         dplyr::select(sector,unit)))){
      print(Z0);print(D0)
      stop(print(paste("Z0 and D0 do not have identical sectors and units.",sep="")))
    }}

  if(!is.null(Import0)){
    Import0 <- addMissing(Import0)
    if(!any(grepl("sector",colnames(Import0)))){
      stop(print(paste("Column names in Import0: ",paste(colnames(Import0),collapse=", ")," must include 'sector'.",sep="")))}
    if(!any(grepl(c("import"),colnames(Import0)))){
      stop(print(paste("Column names in Import0: ",paste(colnames(DNew),collapse=", "),"must include 'import' where domestic represents internal demands for each sector from domestic non-nexus sectors.",sep="")))}
    Import0 <- Import0 %>% dplyr::arrange (sector)
  } else {
    Import0 <- expand.grid(sectors$sector,years,scenarios,subRegions) %>%
      dplyr::rename(sector=Var1,x=Var2,scenario=Var3,subRegion=Var4) %>%
        dplyr::mutate(import=0)
  }

    if(!is.null(Cap0)){
      Cap0 <- addMissing(Cap0)
      if(!any(grepl("sector",colnames(Cap0)))){
        stop(print(paste("Column names in Cap0: ",paste(colnames(Cap0),collapse=", ")," must include 'sector'.",sep="")))}
      if(!any(grepl("cap",colnames(Cap0)))){
        stop(print(paste("Column names in Cap0: ",paste(colnames(Cap0),collapse=", ")," must include 'cap'.",sep="")))}
      Cap0 <- Cap0 %>% dplyr::arrange (sector)
      sectors <- Cap0 %>% dplyr::select(sector) %>% unique()
    } else {
      Cap0 <- expand.grid(sectors$sector,years,scenarios,subRegions) %>%
        dplyr::rename(sector=Var1,x=Var2,scenario=Var3,subRegion=Var4) %>%
        dplyr::mutate(cap=-1)
    }


 if(!is.null(D0)){
   if(!is.null(Import0)){
   D0imp <- D0 %>%
     dplyr::left_join(Import0) %>%
     dplyr::mutate(miscAdjust = dplyr::case_when((otherTotal-import)<0~miscAdjust+(import-otherTotal),
                                          TRUE~miscAdjust),
                   otherTotal = dplyr::case_when((otherTotal-import)<0~otherTotal+(import-otherTotal),
                                          TRUE~otherTotal),
                   localTotal = otherTotal - import) %>%
     dplyr::select(-import)

   } else {
     D0imp <- D0 %>%
       dplyr::mutate(localTotal = otherTotal)
   }
 }

}


if(length(colsNumeric)<1){colsNumeric=NULL}

#------------------------------------
# For each year, suregion and scenario
#------------------------------------

tibble::tibble()->A_ANew1->L_ANew1->sol_ANew1->A_DNew1->L_DNew1 ->sol_DNew1 ->
  A_ZNew1 ->L_ZNew1 ->sol_ZNew1 ->A_XNew1->L_XNew1->sol_XNew1 ->A_Calibrated1 ->
  L_Calibrated1 ->sol_Calibrated1 ->ioTbl_Calibrated1 -> ioTblImports_Calibrated1 ->
  A_Orig1 -> L_Orig1->sol_Orig1->ioTbl_Orig1 -> ioTblImports_Orig1

for(scenario_i in scenarios){
  for(subRegion_i in subRegions){
    for(year_i in years){

      # scenario_i=scenarios[1]
      # subRegion_i=subRegions[1]
      # year_i=years[1]

print(paste("Solving for scenario: ", scenario_i, ", year:", year_i, " and sub-region:", subRegion_i," ...",sep=""))


      # Subset Data (D0i,X0i,A0i,Z0i,Cap0i,Import0i,XNewi,DNewi,ANewi,ZNewi,ZPartial)
      if(T){
      if(!is.null(D0imp)){D0i<-D0imp %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(X0)){X0i<-X0 %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(A0)){A0i<-A0 %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(Z0)){Z0i<-Z0 %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(Cap0)){Cap0i<-Cap0 %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(Import0)){Import0i<-Import0 %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(XNew)){XNewi<-XNew %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(DNew)){DNewi<-DNew %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(ANew)){ANewi<-ANew %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(ZNew)){ZNewi<-ZNew %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(ZPartial)){ZPartial<-ZPartial %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      }


  # A is intensity coefficients
  # D is local demands + exports - imports
  # X is resources processed
  # Cap is capacity or availability of commodity
  # Surpluss is additional processing capacity

  #---------------------------------------
  # IF A0i is provided with either X0i or D0i. Z0i will be ignored. Unless Import is provided it will be calcualted.
  # ----  A0i + D0i + Import0i
  # i. Calculate L and then initial X -> L x (D0i + Import)
  # ii. Calculate if X < cap then import = X-Cap; else import = 0 and surpluss = Cap - X.
  # iii. Calculate X adjusted for Capacity
  # iv. Calculate: ioTbl, Z, D, Import, X, , Surpluss, A, L
  # ---- A0i + X0i + Import0i
  # i. Calculate L and then initial X -> X0i
  # ii. Calculate if X < cap then import = X-Cap; else import = 0i and surpluss = Cap - X.
  # iii. Calculate X adjusted for Capacity
  # iv. Calculate: ioTbl, Z, D, Import, X, , Surpluss, A, L
  #---------------------------------------

  if(!is.null(A0i)){

    if(is.null(X0i) & is.null(D0i)){
      stop("Need to provide atleast one either X0i or D0i")
    } else {

      A<-as.matrix(A0i%>%dplyr::select(c(A0i$sector)))
      L<-solve(diag(nrow(A))-A);


      if(!is.null(D0i)){
         # Domestic and import
         print("Calculating X based on D0i provided and Import")
          Xi <-(L%*%as.matrix(D0i$localTotal))%>% tibble::as_tibble()
          names(Xi)<-"processed"
          Xi <- Xi %>% dplyr::bind_cols(D0i%>%dplyr::select(sector,addedColumns))

          # X = If Xi is greater than Cap then adjust Xi to Cap and allocate rest to imports
          # Surpluss = Cap - Xi

          X <- Xi %>%
            dplyr::left_join(Cap0i%>%dplyr::select(sector,cap), by=c("sector")) %>%
            dplyr::left_join(Import0i%>%dplyr::select(sector,import), by=c("sector")) %>%
            dplyr::mutate(cap = dplyr::case_when(cap<0~processed,
                                          TRUE~cap),
                          import = dplyr::case_when(processed>cap~(processed-cap)+import,
                                                TRUE~import),
                          processed = dplyr::case_when(processed>cap~cap,
                                                TRUE~processed),
                          surplus = cap-processed)

         print(paste("Recaclulating Z based on A0i and D0i provided. Orig Z0i provided = ",sep=""));

          # Original Nexus flows, before adjusting for capacity
          Zi <- A %*% diag(as.vector(t(as.matrix(Xi$processed))))
          Zi <- dplyr::bind_cols(A0i%>%dplyr::select(sector),tibble::as_tibble(Zi))
          names(Zi)<-c("sector",A0i$sector)
          Zi <- Zi %>%
            dplyr::mutate(nexusTotali= Zi %>%
                            dplyr::select(c(Zi$sector)) %>%
                            rowSums); Zi

          # New Z values after removing imports
          Z <- A %*% diag(as.vector(t(as.matrix(X$processed))))
          Z <- dplyr::bind_cols(A0i%>%dplyr::select(sector),tibble::as_tibble(Z))
          names(Z)<-c("sector",A0i$sector)
          Z <- Z %>%
            dplyr::mutate(nexusTotal= Z %>%
                            dplyr::select(c(Z$sector)) %>%
                            rowSums);Z

          # Calculate ioTbl
          solTbl <- Z %>%
            dplyr::left_join(Zi %>% dplyr::select(sector,nexusTotali)) %>%
            dplyr::left_join(X) %>%
            dplyr::mutate(import = dplyr::case_when(import>(nexusTotali-nexusTotal)~import - (nexusTotali-nexusTotal),
                                             TRUE~import),
                          processed = dplyr::case_when(import<(nexusTotali-nexusTotal)~ processed - (nexusTotali-nexusTotal),
                                                TRUE~processed),
                          surplus = cap-processed,
                          otherTotal = processed - nexusTotal + import,
                          totalDemands = nexusTotal+otherTotal) %>%
            dplyr::select(-nexusTotali); solTbl

          tmpSol <- ioTblSolOrganize(solTbl,D0i,colsNumeric,addedColumns); tmpSol$sol_Tmp



      } else {

        if(!is.null(X0i)){

          Xi <- X0i; print("Using X0i provided")

          # X = If Xi is greater than Cap then adjust Xi to Cap and allocate rest to imports
          # Surpluss = Cap - Xi

          X <- Xi %>%
            dplyr::left_join(Cap0i%>%dplyr::select(sector,cap), by=c("sector")) %>%
            dplyr::left_join(Import0i%>%dplyr::select(sector,import), by=c("sector")) %>%
            dplyr::mutate(cap = dplyr::case_when(cap<0~processed,
                                          TRUE~cap),
                          import = dplyr::case_when(processed>cap~(processed-cap)+import,
                                            TRUE~import),
                          processed = dplyr::case_when(processed>cap~cap,
                                                TRUE~processed),
                          surplus = cap-processed)


        # Calculate z0i based on X0i
        print(paste("Recaclulating Z based on A0i and X0i provided",sep=""))

        # New Z values after removing imports
        Z <- A %*% diag(as.vector(t(as.matrix(X$processed))))
        Z <- dplyr::bind_cols(A0i%>%dplyr::select(sector),tibble::as_tibble(Z))
        names(Z)<-c("sector",A0i$sector)
        Z <- Z %>%
          dplyr::mutate(nexusTotal= Z %>%
                          dplyr::select(c(Z$sector)) %>%
                          rowSums);Z

        # Calculate ioTbl
        solTbl <- Z %>%
          dplyr::select(c("sector",Z$sector,"nexusTotal")) %>%
          dplyr::left_join(X) %>%
          dplyr::mutate(otherTotal = dplyr::case_when((processed + import)>nexusTotal~(processed + import - nexusTotal),
                                               TRUE~0),
                        import = dplyr::case_when((processed + import)<nexusTotal~(nexusTotal-(processed + import)),
                                           TRUE~import),
                        totalDemands = otherTotal + nexusTotal); solTbl %>% as.data.frame

        tmpSol <- ioTblSolOrganize(solTbl,D0i,colsNumeric,addedColumns); tmpSol$sol_Tmp%>%as.data.frame


        } # Close X0i loop

      } # Close D0i loop


    } # Close A0i loop

  } else {

   #---------------------------------------
   # IF A0i is not provided. Need Z0i and either X0i or D0i/Import0i.
   # If X0i provided D is calculated, else if D0i/Import0i provided X is calculated.
   # If Import0i is not provided it is assumed as 0i.
   # i. Calculate L and then initial X -> L x (D0i + Import)
   # ii. Calculate if X < cap then import = X-Cap; else import = 0i and surpluss = Cap - X.
   # iii. Calculate X adjusted for Capacity
   # iv. Calculate: solTbl, Z, D, Import, X, , Surpluss, A, L
   # Calculate: solTbl, Z, D, X, A, L
   #---------------------------------------

  if(!is.null(Z0i)){

    Z = Z0i

    if(!is.null(X0i)){

      Xi <- X0i; print("Using X0i provided")

      # X = If Xi is greater than Cap then adjust Xi to Cap and allocate rest to imports
      # Surpluss = Cap - Xi

      X <- Xi %>%
        dplyr::left_join(Cap0i%>%dplyr::select(sector,cap), by=c("sector")) %>%
        dplyr::left_join(Import0i%>%dplyr::select(sector,import), by=c("sector")) %>%
        dplyr::mutate(cap = dplyr::case_when(cap<0~processed,
                                      TRUE~cap),
                      import = dplyr::case_when(processed>cap~(processed-cap)+import,
                                        TRUE~import),
                      processed = dplyr::case_when(processed>cap~cap,
                                            TRUE~processed),
                      surplus = cap-processed)

      Z <- Z %>%
        dplyr::mutate(nexusTotal= Z %>%
                        dplyr::select(c(Z$sector)) %>%
                        rowSums);Z

      # Calculate ioTbl
      solTbl <- Z %>%
        dplyr::select(c("sector",Z$sector,"nexusTotal")) %>%
        dplyr::left_join(X) %>%
        dplyr::mutate(otherTotal = dplyr::case_when((processed + import)>nexusTotal~(processed + import - nexusTotal),
                                             TRUE~0),
                      import = dplyr::case_when((processed + import)<nexusTotal~(nexusTotal-(processed + import)),
                                         TRUE~import),
                      totalDemands = processed + import); solTbl %>% as.data.frame

      tmpSol <- ioTblSolOrganize(solTbl,D0i,colsNumeric,addedColumns); tmpSol$sol_Tmp%>%as.data.frame



      }

    if(!is.null(D0i)){


      X <- Z %>%
        dplyr::mutate(nexusTotal= Z %>%
                      dplyr::select(c(Z$sector)) %>%
                      rowSums) %>%
        dplyr::left_join(D0i %>%
                         dplyr::select(sector,otherTotal)) %>%
        dplyr::mutate(processed = otherTotal + nexusTotal) %>%
        dplyr::left_join(Cap0i%>%dplyr::select(sector,cap), by=c("sector")) %>%
        dplyr::left_join(Import0i%>%dplyr::select(sector,import), by=c("sector")) %>%
        dplyr::mutate(cap = dplyr::case_when(cap<0~processed,
                                      TRUE~cap),
                      import = dplyr::case_when(processed>cap~(processed-cap)+import,
                                        TRUE~import),
                      processed = dplyr::case_when(processed>cap~cap,
                                            TRUE~processed),
                      surplus = cap-processed)

      # Calculate ioTbl
      solTbl <- X %>%
        dplyr::mutate(totalDemands = nexusTotal+otherTotal) %>%
        addMissing() ; solTbl


      tmpSol <- ioTblSolOrganize(solTbl,D0i,colsNumeric,addedColumns)


    }

  } else { stop("No A0i or Z0i provided.")}


    } # if A0i not provided loop.


    #---------------------------------------
    # Calibrated Solution Based on ZPartial
    #---------------------------------------

    if(!is.null(ZPartial)){

      Z_Origl<-tidyr::gather(tmpSol$Z_Tmp,-sector,-addedColumns,key="To",value="value");Z_Origl
      ZPartiall<-tidyr::gather(ZPartial,-sector,-addedColumns,key="To",value="valueP");ZPartiall
      Z_Calibl<-dplyr::left_join(Z_Origl,ZPartiall) %>%
        dplyr::mutate(value=dplyr::case_when(!is.na(valueP)~as.numeric(valueP),
                                      TRUE~as.numeric(value))) %>%
        dplyr::select(-valueP);Z_Calibl

      Z_Calib <- tidyr::spread(Z_Calibl, key="To",value="value");Z_Calib

      Z = Z_Calib
      # Calculate ioTbl
      solTbl_Calib <- Z %>%
        dplyr::mutate(nexusTotal= Z %>%
                        dplyr::select(c(Z$sector)) %>%
                        rowSums) %>%
        dplyr::bind_cols(X) %>%
        dplyr::mutate(local = processed - nexusTotal,
                      otherTotal = local + import,
                      totalDemands = nexusTotal+otherTotal) %>%
        addMissing()

      tmpSol_Calib <- ioTblSolOrganize(solTbl_Calib,D0i,colsNumeric)

    }




    #---------------------------------------
    # New solutions based on DNewi, XNewi, ANewi or ZNewi
    #---------------------------------------

    #---------------------------------------
    # DNewi
    # Will calculate new X, Z and solTblNewi based on existing A and L
    #---------------------------------------

  if(!is.null(DNewi)){

      D = tibble::as_tibble(DNewi);
      Z <- tibble::as_tibble(A_Orig%*%diag(x=(as.vector(L_Orig%*%as.matrix(D$otherTotal)))))
      names(Z)=solTbl$sector;
      Z <- Z %>% dplyr::bind_cols(D %>% dplyr::select(sector))

      X <- Z %>%
        dplyr::mutate(nexusTotal= Z %>%
                        dplyr::select(c(Z$sector)) %>%
                        rowSums) %>%
        dplyr::left_join(D %>%
                           dplyr::select(otherTotal,sector)) %>%
        dplyr::mutate(processed = otherTotal + nexusTotal) %>%
        dplyr::left_join(Cap0i%>%dplyr::select(sector,cap), by=c("sector")) %>%
        dplyr::left_join(Import0i%>%dplyr::select(sector,import), by=c("sector")) %>%
        dplyr::mutate(cap = dplyr::case_when(cap<0~processed,
                                      TRUE~cap),
                      import = dplyr::case_when(processed>cap~(processed-cap)+import,
                                        TRUE~import),
                      processed = dplyr::case_when(processed>cap~cap,
                                            TRUE~processed),
                      surplus = cap-processed)

      # Calculate solTbl
      solTblNewi <- Z %>%
        dplyr::mutate(nexusTotal= Z %>%
                        dplyr::select(c(Z$sector)) %>%
                        rowSums) %>%
        dplyr::bind_cols(X) %>%
        dplyr::mutate(local = processed - nexusTotal,
                      otherTotal = local + import) %>%
        addMissing()


      A_DNewi <-as.matrix(Z%>%dplyr::select(c(Z$sector))) %*% as.matrix(solTblNewi$processed^-1*diag(nrow(Z)))
      L_DNewi <-solve(diag(nrow(A_DNewi))-A_DNewi);
      D_DNewi <-DNewi %>% dplyr::select(sector,otherTotal);
      Z_DNewi <-Z %>% dplyr::select(c(sector,Z$sector));
      X_DNewi <-solTblNewi %>% dplyr::select(sector,processed);
      sol_DNewi <-solTblNewi %>%
        dplyr::select(c(sector,solTblNewi$sector,nexusTotal,local, processed, cap, surplus, import, otherTotal,
                        names(solTblNewi)[!names(solTblNewi) %in% c("sector",solTblNewi$sector,
                                                                nexusTotal,local, processed, cap, surplus, import, otherTotal)]));
  }


    #---------------------------------------
    # ANewi
    # Will calculate new L, Z, X and solTblNewi based on existing D
    #---------------------------------------

    if(!is.null(ANewi)){

      A1 <- as.matrix(ANewi %>% dplyr::select(c(solTbl$sector)))
      L1 <- solve(diag(nrow(A1))-A1);
      D <- D_Orig
      Z <- tibble::as_tibble(A1%*%diag(x=(as.vector(L1%*%as.matrix(D$otherTotal)))))
      names(Z)=solTbl$sector;
      Z <- Z %>% dplyr::bind_cols(D %>% dplyr::select(sector))

      X <- Z %>%
        dplyr::mutate(nexusTotal= Z %>%
                        dplyr::select(c(Z$sector)) %>%
                        rowSums) %>%
        dplyr::left_join(D %>%
                           dplyr::select(otherTotal,sector)) %>%
        dplyr::mutate(processed = otherTotal + nexusTotal) %>%
        dplyr::left_join(Cap0i%>%dplyr::select(sector,cap), by=c("sector")) %>%
        dplyr::left_join(Import0i%>%dplyr::select(sector,import), by=c("sector")) %>%
        dplyr::mutate(cap = dplyr::case_when(cap<0~processed,
                                      TRUE~cap),
                      import = dplyr::case_when(processed>cap~(processed-cap)+import,
                                        TRUE~import),
                      processed = dplyr::case_when(processed>cap~cap,
                                            TRUE~processed),
                      surplus = cap-processed)

      # Calculate solTbl
      solTblNewi <- Z %>%
        dplyr::mutate(nexusTotal= Z %>%
                        dplyr::select(c(Z$sector)) %>%
                        rowSums) %>%
        dplyr::left_join(X) %>%
        dplyr::mutate(local = processed - nexusTotal,
                      otherTotal = local + import) %>%
        addMissing()

      A_ANewi <-as.matrix(Z%>%dplyr::select(c(Z$sector))) %*% as.matrix(solTblNewi$processed^-1*diag(nrow(Z)))
      L_ANewi <-solve(diag(nrow(A_ANewi))-A_ANewi);
      D_ANewi <-solTblNewi %>% dplyr::select(sector,otherTotal);
      Z_ANewi <-Z %>% dplyr::select(c(sector,Z$sector));
      X_ANewi <-solTblNewi %>% dplyr::select(sector,processed);
      sol_ANewi <- solTblNewi %>%
        dplyr::select(c(sector,solTblNewi$sector,nexusTotal,local, processed, cap, surplus, import, otherTotal,
                        names(solTblNewi)[!names(solTblNewi) %in% c("sector",solTblNewi$sector,
                                                                nexusTotal,local, processed, cap, surplus, import, otherTotal)]));

    }

    #---------------------------------------
    # ZNewi
    # Will calculate new A, L, Z, X and solTblNewi based on existing D
    #---------------------------------------

    if(!is.null(ZNewi)){

      Z = ZNewi

      X <- Z %>%
        dplyr::mutate(nexusTotal= Z %>%
                        dplyr::select(c(Z$sector)) %>%
                        rowSums) %>%
        dplyr::left_join(D_Orig %>%
                           dplyr::select(otherTotal,sector)) %>%
        dplyr::mutate(processed = otherTotal + nexusTotal) %>%
        dplyr::left_join(Cap0i%>%dplyr::select(sector,cap), by=c("sector")) %>%
        dplyr::left_join(Import0i%>%dplyr::select(sector,import), by=c("sector")) %>%
        dplyr::mutate(cap = dplyr::case_when(cap<0~processed,
                                      TRUE~cap),
                      import = dplyr::case_when(processed>cap~(processed-cap)+import,
                                        TRUE~import),
                      processed = dplyr::case_when(processed>cap~cap,
                                            TRUE~processed),
                      surplus = cap-processed)

      # Calculate solTbl
      solTblNewi <- Z %>%
        dplyr::mutate(nexusTotal= Z %>%
                        dplyr::select(c(Z$sector)) %>%
                        rowSums) %>%
        dplyr::left_join(X) %>%
        dplyr::mutate(local = processed - nexusTotal,
                      otherTotal = local + import) %>%
        addMissing()


        A_ZNewi<-as.matrix(Z%>%dplyr::select(c(Z$sector))) %*% as.matrix(solTblNewi$processed^-1*diag(nrow(Z)))
        L_ZNewi<-solve(diag(nrow(A_ZNewi))-A_ZNewi);
        D_ZNewi<-solTblNewi %>% dplyr::select(sector,otherTotal);
        Z_ZNewi<- Z %>% dplyr::select(c(sector,Z$sector));
        X_ZNewi<-solTblNewi %>% dplyr::select(sector,processed);
        sol_ZNewi <- solTblNewi %>%
          dplyr::select(c(sector,solTblNewi$sector,nexusTotal,local, processed, cap, surplus, import, otherTotal,
                          names(solTblNewi)[!names(solTblNewi) %in% c("sector",solTblNewi$sector,
                                                                  nexusTotal,local, processed, cap, surplus, import, otherTotal)]));

    }

      #---------------------------------------
      # XNewi
      # Will calculate new Z, A, L and solTblNewi based on existing D, A and L
      #---------------------------------------

      if(!is.null(XNewi)){

        D = tibble::as_tibble(D_Orig);
        Z <- (A_Orig %*% diag(as.vector(t(as.matrix(XNewi$processed))))) %>% tibble::as_tibble()
        names(Z)<-sol_Orig$sector
        Z <- dplyr::bind_cols(sol_Orig%>%dplyr::select(sector),Z)

        X <- Z %>%
          dplyr::mutate(nexusTotal= Z %>%
                          dplyr::select(c(Z$sector)) %>%
                          rowSums) %>%
          dplyr::left_join(D_Orig %>%
                             dplyr::select(otherTotal,sector)) %>%
          dplyr::mutate(processed = otherTotal + nexusTotal) %>%
          dplyr::left_join(Cap0i%>%dplyr::select(sector,cap), by=c("sector")) %>%
          dplyr::left_join(Import0i%>%dplyr::select(sector,import), by=c("sector")) %>%
          dplyr::mutate(cap = dplyr::case_when(cap<0~processed,
                                        TRUE~cap),
                        import = dplyr::case_when(processed>cap~(processed-cap)+import,
                                          TRUE~import),
                        processed = dplyr::case_when(processed>cap~cap,
                                              TRUE~processed),
                        surplus = cap-processed)

        # Calculate solTbl
        solTblNewi <- Z %>%
          dplyr::mutate(nexusTotal= Z %>%
                          dplyr::select(c(Z$sector)) %>%
                          rowSums) %>%
          dplyr::left_join(X %>%
                             dplyr::select(processed, cap, import, surplus,sector)) %>%
          dplyr::mutate(local = processed - nexusTotal,
                        otherTotal = local + import) %>%
          addMissing()

        A_XNewi <-as.matrix(Z%>%dplyr::select(c(Z$sector))) %*% as.matrix(solTblNewi$processed^-1*diag(nrow(Z)))
        L_XNewi <-solve(diag(nrow(A_XNewi))-A_XNewi);
        D_XNewi <-XNewi %>% dplyr::select(sector,otherTotal);
        Z_XNewi <-Z %>% dplyr::select(c(sector,Z$sector));
        X_XNewi <-solTblNewi %>% dplyr::select(sector,processed);
        sol_XNewi <- solTblNewi %>%
          dplyr::select(c(sector,solTblNewi$sector,nexusTotal,local, processed, cap, surplus, import, otherTotal,
                          names(solTblNewi)[!names(solTblNewi) %in% c("sector",solTblNewi$sector,
                                                                  nexusTotal,local, processed, cap, surplus, import, otherTotal)]));

      }


      # Prepare final data list

      A_Orig = tmpSol$A_Tmp
      L_Orig = tmpSol$L_Tmp
      ioTbl_Orig = tmpSol$ioTbl_Tmp
      ioTblImports_Orig = tmpSol$ioTblImports_Tmp
      sol_Orig = tmpSol$sol_Tmp

      if(!is.null(ZPartial)){
      A_Calibrated = tmpSol_Calib$A_Tmp
      L_Calibrated = tmpSol_Calib$L_Tmp
      ioTbl_Calibrated = tmpSol_Calib$ioTbl_Tmp
      ioTblImports_Calibrated = tmpSol_Calib$ioTblImports_Tmp
      sol_Calibrated = tmpSol_Calib$sol_Tmp}


     if(TRUE){
      A_ANew1= A_ANew1 %>% dplyr::bind_rows(A_ANewi %>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      L_ANew1 = L_ANew1 %>% dplyr::bind_rows(L_ANewi%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      sol_ANew1 = sol_ANew1 %>% dplyr::bind_rows(sol_ANewi%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      A_DNew1 = A_DNew1 %>% dplyr::bind_rows(A_DNewi%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      L_DNew1 = L_DNew1 %>% dplyr::bind_rows(A_DNewi%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      sol_DNew1 = sol_DNew1 %>% dplyr::bind_rows(sol_DNewi%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      A_ZNew1 = A_ZNew1 %>% dplyr::bind_rows(A_ZNewi%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      L_ZNew1 = L_ZNew1 %>% dplyr::bind_rows(L_ZNewi%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      sol_ZNew1 = sol_ZNew1 %>% dplyr::bind_rows(sol_ZNewi%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      A_XNew1 = A_XNew1 %>% dplyr::bind_rows(A_XNewi%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      L_XNew1 = L_XNew1 %>% dplyr::bind_rows(L_XNewi%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      sol_XNew1 = sol_XNew1 %>% dplyr::bind_rows(sol_XNewi%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      A_Calibrated1 = A_Calibrated1 %>% dplyr::bind_rows(A_Calibrated%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      L_Calibrated1 = L_Calibrated1 %>% dplyr::bind_rows(L_Calibrated%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      sol_Calibrated1 = sol_Calibrated1 %>% dplyr::bind_rows(sol_Calibrated%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      ioTbl_Calibrated1 = ioTbl_Calibrated1 %>% dplyr::bind_rows(ioTbl_Calibrated%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      ioTblImports_Calibrated1 = ioTblImports_Calibrated1 %>% dplyr::bind_rows(ioTblImports_Calibrated%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      A_Orig1 = A_Orig1 %>% dplyr::bind_rows(A_Orig%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      L_Orig1 = L_Orig1 %>% dplyr::bind_rows(L_Orig%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      ioTbl_Orig1 = ioTbl_Orig1 %>% dplyr::bind_rows(ioTbl_Orig%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      ioTblImports_Orig1 = ioTblImports_Orig1 %>% dplyr::bind_rows(ioTblImports_Orig%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      sol_Orig1 = sol_Orig1 %>% dplyr::bind_rows(sol_Orig%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
     }

    } # Close loop scenario
  } # close loop subRegion
} # close loop year

    sol_list<-list(A_ANew1=A_ANew1, L_ANew1=L_ANew1, sol_ANew1=sol_ANew1,
                   A_DNew1=A_DNew1, L_DNew1=L_DNew1, sol_DNew1=sol_DNew1,
                   A_ZNew1=A_ZNew1, L_ZNew1=L_ZNew1, sol_ZNew1=sol_ZNew1,
                   A_XNew1=A_XNew1, L_XNew1=L_XNew1, sol_XNew1=sol_XNew1,
                   A_Calibrated1=A_Calibrated1, L_Calibrated1=L_Calibrated1, sol_Calibrated1=sol_Calibrated1,
                   ioTbl_Calibrated1=ioTbl_Calibrated1, ioTblImports_Calibrated1=ioTblImports_Calibrated1,
                   A_Orig1=A_Orig1, L_Orig1=L_Orig1, ioTbl_Orig1=ioTbl_Orig1, ioTblImports_Orig1=ioTblImports_Orig1, sol_Orig1=sol_Orig1
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
    # sol_Orig1
    #---------------------

  # ioTable normalized

  sol<-sol_list$ioTblImports_Orig1 %>%
    dplyr::filter(scenario==scenario_i)


  df_Mn<-sol %>%
    #dplyr::select (-processed,processed) %>% # to place processed last for following function
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
    c(sort(unique(df_Mnx$sectorTo)[unique(df_Mnx$sectorTo) %in% sectorFromOrder]),
      sort(unique(df_Mnx$sectorTo)[!unique(c(df_Mnx$sectorTo)) %in% c(sectorFromOrder,"otherTotal","totalDemands")]),
      unique(df_Mnx$sectorTo)[unique(c(df_Mnx$sectorTo)) %in% c("otherTotal","totalDemands")]
    ); sectorToOrder


  # ioTable normalized bubbles

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


  # ioTable absolute bubbles

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


  #---------------------
  # sol_Calibrated1
  #---------------------


  if(nrow(sol_Calibrated1)>0){

    # ioTable normalized

    sol<-sol_list$ioTblImports_Calibrated1 %>%
      dplyr::filter(scenario==scenario_i)

    df_Mn<-sol %>%
      #dplyr::select (-processed,processed) %>% # to place processed last for following function
      dplyr::mutate_at(vars(-c("sector",addedColumns[addedColumns %in% names(sol)])),dplyr::funs(./totalDemands)); df_Mn

    solx <- sol %>%
      tidyr::gather(-c("sector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
      dplyr::rename (sectorFrom=sector) %>%
      dplyr::arrange(sectorFrom)  %>%
      dplyr::filter(!is.nan(value),value!=0, !is.na(value));

    df_Mnx <- df_Mn %>%
      tidyr::gather(-c("sector",addedColumns[addedColumns %in% names(df_Mn)]),key="sectorTo",value="value") %>%
      dplyr::rename (sectorFrom=sector) %>%
      dplyr::arrange(sectorFrom) %>%
      dplyr::filter(!is.nan(value),value!=0, !is.na(value)); df_Mnx

    sectorFromOrder <- NULL
    sectorFromOrder_local <- sort(unique(df_Mnx$sectorFrom)[!grepl("import_",unique(df_Mnx$sectorFrom))]); sectorFromOrder_local
    sectorFromOrder_import <- sort(unique(df_Mnx$sectorFrom)[grepl("import_",unique(df_Mnx$sectorFrom))]); sectorFromOrder_import
    for(i in (1:max(length(sectorFromOrder_local),length(sectorFromOrder_import)))) {
      if(length(sectorFromOrder_local)>=i){sectorFromOrder <- c(sectorFromOrder,sectorFromOrder_local[i])}
      if(length(sectorFromOrder_import)>=i){sectorFromOrder <- c(sectorFromOrder,sectorFromOrder_import[i])}
    }; sectorFromOrder

    sectorToOrder <-
      c(sort(unique(df_Mnx$sectorTo)[unique(df_Mnx$sectorTo) %in% sectorFromOrder]),
        sort(unique(df_Mnx$sectorTo)[!unique(c(df_Mnx$sectorTo)) %in% c(sectorFromOrder,"otherTotal","totalDemands")]),
        unique(df_Mnx$sectorTo)[unique(c(df_Mnx$sectorTo)) %in% c("otherTotal","totalDemands")]
      ); sectorToOrder


    # ioTable normalized bubbles

    fname = paste(scenario_i,"_solCalibrated1_flowTbl_norm_bubble",nameAppend,sep="")
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


    # ioTable absolute bubbles

    fname = paste(scenario_i,"_solCalibrated1_flowTbl_abs_bubble",nameAppend,sep="")
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


    fname = paste(scenario_i,"_solCalibrated1_sankey_abs_DisAggDemands",nameAppend,sep="")
    g<-ggplot(as.data.frame(dfx%>%dplyr::filter(value!=0)),
              aes(y = value, axis1 = sectorFrom, axis2 = sectorTo, group=subRegion)) +
      theme_bw() +
      ggalluvial::geom_alluvium(aes(fill = sectorFrom), width = 1/12, color="black") +
      ggalluvial::geom_stratum(width = 1/12, fill = "grey30", color = "grey", alpha=1) +
      geom_label(stat = "stratum", label.strata = TRUE) +
      scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
      scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
      #scale_fill_brewer(type = "qual", palette = "Set1", name="From") +
      facet_grid(x~subRegion) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 15),
            axis.text.y = element_text(size = 15),
            strip.text  = element_text(size = 15),
            axis.title = element_text(size = 15))+
      ggtitle(fname);g


    printf(figure=g,fileName = fname, dir=dir)


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


    fname = paste(scenario_i,"_solOrig1_sankey_abs_AggDemands",nameAppend,sep="")
    g<-ggplot(as.data.frame(dfx%>%dplyr::filter(value!=0)),
              aes(y = value, axis1 = sectorFrom, axis2 = sectorTo, group=subRegion)) +
      theme_bw() +
      ggalluvial::geom_alluvium(aes(fill = sectorFrom), width = 1/12, color="black") +
      ggalluvial::geom_stratum(width = 1/12, fill = "grey30", color = "grey", alpha=1) +
      geom_label(stat = "stratum", label.strata = TRUE) +
      scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
      scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
      #scale_fill_brewer(type = "qual", palette = "Set1", name="From") +
      facet_grid(x~subRegion) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 15),
            axis.text.y = element_text(size = 15),
            strip.text  = element_text(size = 15),
            axis.title = element_text(size = 15))+
      ggtitle(fname);g


    printf(figure=g,fileName = fname, dir=dir)

  } # If sol_Calibrated1 is null

  } # For Scenario i

  return(sol_list)

} # Close Function
