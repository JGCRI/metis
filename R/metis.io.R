#' metis.io
#'
#' This function prepares gridded data for use with domestic metis modules.
#' @param dirOutputs Default =paste(getwd(),"/outputs",sep=""),
#' @param Z0i Default = NULL,
#' @param D0i Default = NULL,
#' @param X0i Default = NULL,
#' @param A0i Default = NULL,
#' @param Trade0i Default =NULL,
#' @param Cap0i Default =NULL,
#' @param DNewi Default =NULL,
#' @param XNewi Default =NULL,
#' @param ZNewi Default =NULL,
#' @param ANewi Default =NULL,
#' @param ZPartiali Default =NULL,
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @export


metis.io<-function(Z0i=NULL,
                 D0i=NULL,
                 X0i=NULL,
                 A0i=NULL,
                 Trade0i=NULL,
                 Cap0i=NULL,
                 DNewi=NULL,
                 XNewi=NULL,
                 ZNewi=NULL,
                 ANewi=NULL,
                 ZPartiali=NULL,
                 dirOutputs=paste(getwd(),"/outputs",sep="")
                        ){

  # Z0i=NULL
  # D0i=NULL
  # X0i=NULL
  # A0i=NULL
  # Trade0i=NULL
  # Cap0i=NULL
  # DNewi=NULL
  # XNewi=NULL
  # ZNewi=NULL
  # ANewi=NULL
  # ZPartiali=NULL
  # dirOutputs=paste(getwd(),"/outputs",sep="")

#----------------
# Initialize variables by setting to NULL
#----------------

NULL->year->sector->domestic->export->processed->V1->value->.->domestic->production->domestic->
    sol_Orig->Z->A->X->D-> classPalette -> internal -> param -> region -> scenario -> x ->
    Var1 -> Var2 -> Var3 -> Var4 -> cap -> sectorFrom -> sectorTo -> subRegion -> surplus -> trade ->
    valueP ->
    A_Orig -> L_Orig -> sol_Orig ->
    A_ANewi -> L_ANewi -> sol_ANewi ->
    A_DNewi -> L_DNewi -> sol_DNewi ->
    A_ZNewi -> L_ZNewi -> sol_ZNewi ->
    A_XNewi -> L_XNewi -> sol_XNewi ->
    A_ZPartiali -> L_ZPartiali -> sol_ZPartiali->
    A_ANew -> L_ANew -> sol_ANew ->
    A_DNew -> L_DNew -> sol_DNew ->
    A_ZNew -> L_ZNew -> sol_ZNew ->
    A_XNew -> L_XNew -> sol_XNew ->
    A_ZPartial -> L_ZPartial -> sol_ZPartial->
    X0D0->ioTblNew->
    D0->X0->A0->Z0->Cap0->Trade0->XNew->DNew->ANew->ZNew->ZPartial


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
# Function for adding any missing columns if needed
# -----------------

addMissing<-function(data){
  if(!"scenario"%in%names(data)){data<-data%>%dplyr::mutate(scenario="scenario")}
  if(!"x"%in%names(data)){if("year"%in%names(data)){
    data<-data%>%dplyr::mutate(x=year)}else{data<-data%>%dplyr::mutate(x=0)}}
  if(!"region"%in%names(data)){data<-data%>%dplyr::mutate(region="region")}
  if(!"subRegion"%in%names(data)){data<-data%>%dplyr::mutate(subRegion="subRegion")}
  if(!"unit"%in%names(data)){data<-data%>%dplyr::mutate(unit="unit")}
  if(!"classPalette"%in%names(data)){data<-data%>%dplyr::mutate(classPalette="pal_hot")}
  if(!"param"%in%names(data)){data<-data%>%dplyr::mutate(param="param")}
  if(!"class"%in%names(data)){data<-data%>%dplyr::mutate(class="class")}
  return(data)
}


  # ------------------
  # Check Columns
  # ------------------
if(T){

  if(is.null(Z0i) & is.null(D0i) & is.null(X0i) & is.null(A0i)){stop("Need to provide atleast two of Z0i, X0i, D0i, A0i")}


  if(!is.null(Z0i)){
    Z0i <- addMissing(Z0i)
    if(!any(grepl("sector",colnames(Z0i)))){
      stop(print(paste("Column names in Z0i: ",paste(colnames(Z0i),collapse=", ")," must include 'sector'.",sep="")))}
    Z0i <- Z0i %>% dplyr::arrange (sector)
    sectors <- Z0i %>% dplyr::select(sector) %>% unique()
    subRegions <- unique(Z0i$subRegion)
    years <- unique(Z0i$x)
    scenarios <- unique(Z0i$scenario)
  }

  if(!is.null(ZNewi)){
    ZNewi <- addMissing(ZNewi)
    if(!any(grepl("sector",colnames(ZNewi)))){
      stop(print(paste("Column names in ZNewi: ",paste(colnames(ZNewi),collapse=", ")," must include 'sector'.",sep="")))}
    ZNewi <- ZNewi %>% dplyr::arrange (sector)
    }

  if(!is.null(ZPartiali)){
    ZPartiali <- addMissing(ZPartiali)
      if(!any(grepl("sector",colnames(ZPartiali)))){
    stop(print(paste("Column names in ZPartiali: ",paste(colnames(ZPartiali),collapse=", ")," must include 'sector'.",sep="")))}
    ZPartiali <- ZPartiali %>% dplyr::arrange (sector)
    }

  if(!is.null(D0i)){
   D0i <- addMissing(D0i)
   if(!any(grepl(c("sector"),colnames(D0i)))){
      stop(print(paste("Column names in D0i: ",paste(colnames(D0i),collapse=", ")," must include 'sector'.",sep="")))}
    if(!any(grepl(c("domestic"),colnames(D0i)))){
      stop(print(paste("Column names in D0i: ",paste(colnames(D0i),collapse=", "),"must include 'domestic' where domestic represents internal demands for each sector from domestic non-nexus sectors.",sep="")))}
   D0i <- D0i %>% dplyr::arrange (sector)
   sectors <- D0i %>% dplyr::select(sector) %>% unique()
   subRegions <- unique(D0i$subRegion)
   years <- unique(D0i$x)
   scenarios <- unique(D0i$scenario)
   }

  if(!is.null(DNewi)){
    DNewi <- addMissing(DNewi)
    if(!any(grepl(c("sector"),colnames(DNewi)))){
      stop(print(paste("Column names in DNewi: ",paste(colnames(DNewi),collapse=", ")," must include 'sector'.",sep="")))}
    if(!any(grepl(c("domestic"),colnames(DNewi)))){
      stop(print(paste("Column names in DNewi: ",paste(colnames(DNewi),collapse=", "),"must include 'domestic' where domestic represents internal demands for each sector from domestic non-nexus sectors.",sep="")))}
    DNewi <- DNewi %>% dplyr::arrange (sector)
    }


  if(!is.null(X0i)){
    X0i <- addMissing(X0i)
    if(!any(grepl("sector",colnames(X0i)))){
      stop(print(paste("Column names in X0i: ",paste(colnames(X0i),collapse=", ")," must include 'sector'.",sep="")))}
    X0i <- X0i %>% dplyr::arrange (sector)
    sectors <- X0i %>% dplyr::select(sector) %>% unique()
    subRegions <- unique(X0i$subRegion)
    years <- unique(X0i$x)
    scenarios <- unique(X0i$scenario)
  }

  if(!is.null(XNewi)){
    XNewi <- addMissing(XNewi)
    if(!any(grepl("sector",colnames(XNewi)))){
      stop(print(paste("Column names in XNewi: ",paste(colnames(XNewi),collapse=", ")," must include 'sector'.",sep="")))}
    XNewi <- XNewi %>% dplyr::arrange (sector)
  }


  if(!is.null(A0i)){
  if(!any(grepl("sector",colnames(A0i)))){
    stop(print(paste("Column names in ANewi: ",paste(colnames(ANewi),collapse=", ")," must include 'sector'.",sep="")))}
      A0i <- addMissing(A0i)
      A0i <- A0i %>% dplyr::arrange (sector)
      sectors <- A0i %>% dplyr::select(sector) %>% unique()
      subRegions <- unique(A0i$subRegion)
      years <- unique(A0i$x)
      scenarios <- unique(A0i$scenario)
    }


  if(!is.null(ANewi)){
    ANewi <- addMissing(ANewi)
    if(!any(grepl("sector",colnames(ANewi)))){
      stop(print(paste("Column names in ANewi: ",paste(colnames(ANewi),collapse=", ")," must include 'sector'.",sep="")))}
    ANewi <- ANewi %>% dplyr::arrange (sector)
  }

  if(!is.null(Z0i) & !is.null(D0i) ){
    if(!identical(unique(Z0i %>%
                         dplyr::select(sector,unit)),
                  unique(D0i %>%
                         dplyr::select(sector,unit)))){
      print(Z0i);print(D0i)
      stop(print(paste("Z0i and D0i do not have identical sectors and units.",sep="")))
    }}

  if(!is.null(Trade0i)){
    Trade0i <- addMissing(Trade0i)
    if(!any(grepl("sector",colnames(Trade0i)))){
      stop(print(paste("Column names in Trade0i: ",paste(colnames(Trade0i),collapse=", ")," must include 'sector'.",sep="")))}
    if(!any(grepl(c("trade"),colnames(Trade0i)))){
      stop(print(paste("Column names in DNewi: ",paste(colnames(DNewi),collapse=", "),"must include 'trade' where domestic represents internal demands for each sector from domestic non-nexus sectors.",sep="")))}
    Trade0i <- Trade0i %>% dplyr::arrange (sector)
  } else {
    Trade0i <- expand.grid(sectors$sector,years,scenarios,subRegions) %>%
      dplyr::rename(sector=Var1,x=Var2,scenario=Var3,subRegion=Var4) %>%
        dplyr::mutate(trade=0)
  }

    if(!is.null(Cap0i)){
      Cap0i <- addMissing(Cap0i)
      if(!any(grepl("sector",colnames(Cap0i)))){
        stop(print(paste("Column names in Cap0i: ",paste(colnames(Cap0i),collapse=", ")," must include 'sector'.",sep="")))}
      if(!any(grepl("cap",colnames(Cap0i)))){
        stop(print(paste("Column names in Cap0i: ",paste(colnames(Cap0i),collapse=", ")," must include 'cap'.",sep="")))}
      Cap0i <- Cap0i %>% dplyr::arrange (sector)
      sectors <- Cap0i %>% dplyr::select(sector) %>% unique()
    } else {
      Cap0i <- expand.grid(sectors$sector,years,scenarios,subRegions) %>%
        dplyr::rename(sector=Var1,x=Var2,scenario=Var3,subRegion=Var4) %>%
        dplyr::mutate(cap=-1)
    }


 if(!is.null(D0i) & !is.null(Trade0i)){
   D0i <- D0i %>%
     dplyr::left_join(Trade0i %>% dplyr::select(sector,trade),by=c("sector")) %>%
     dplyr::mutate(domestic = domestic - trade) %>%
     dplyr::select(-trade)
 }

}

#------------------------------------
# For each year, suregion and scenario
#------------------------------------

tibble::tibble()->A_ANew1->L_ANew1->sol_ANew1->A_DNew1->L_DNew1 ->sol_DNew1 ->
  A_ZNew1 ->L_ZNew1 ->sol_ZNew1 ->A_XNew1->L_XNew1->sol_XNew1 ->A_ZPartial1 ->
  L_ZPartial1 ->sol_ZPartial1 ->A_Orig1 ->L_Orig1->sol_Orig1

for(scenario_i in scenarios){
  for(subRegion_i in subRegions){
    for(year_i in years){

      # scenario_i=scenarios[1]
      # subRegion_i=subRegions[1]
      # year_i=years[1]

print(paste("Solving for scenario: ", scenario_i, ", year:", year_i, " and sub-region:", subRegion_i," ...",sep=""))


      # Subset Data (D0,X0,A0,Z0,Cap0,Trade0,XNew,DNew,ANew,ZNew,ZPartial)
      if(T){
      if(!is.null(D0i)){D0<-D0i %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(X0i)){X0<-X0i %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(A0i)){A0<-A0i %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(Z0i)){Z0<-Z0i %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(Cap0i)){Cap0<-Cap0i %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(Trade0i)){Trade0<-Trade0i %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(XNewi)){XNew<-XNewi %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(DNewi)){DNew<-DNewi %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(ANewi)){ANew<-ANewi %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(ZNewi)){ZNew<-ZNewi %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      if(!is.null(ZPartiali)){ZPartial<-ZPartiali %>% dplyr::filter(scenario==scenario_i,subRegion==subRegion_i,x==year_i)}
      }


  # A is intensity coefficients
  # D is local demands + trade (exports +ve, imports -ve)
  # X is resources processed
  # Cap is capacity or availability of commodity
  # Surpluss is additional processing capacity

  #---------------------------------------
  # IF A0 is provided with either X0 or D0. Z0 will be ignored. Unless Trade is provided it will be calcualted.
  # ----  A0 + D0 + Trade0
  # i. Calculate L and then initial X -> L x (D0 + Trade)
  # ii. Calculate if X < cap then trade = X-Cap; else trade = 0 and surpluss = Cap - X.
  # iii. Calculate X adjusted for Capacity
  # iv. Calculate: ioTbl, Z, D, Trade, X, , Surpluss, A, L
  # ---- A0 + X0 + Trade0
  # i. Calculate L and then initial X -> X0
  # ii. Calculate if X < cap then trade = X-Cap; else trade = 0 and surpluss = Cap - X.
  # iii. Calculate X adjusted for Capacity
  # iv. Calculate: ioTbl, Z, D, Trade, X, , Surpluss, A, L
  #---------------------------------------

  if(!is.null(A0)){

    if(is.null(X0) & is.null(D0)){
      stop("Need to provide atleast one either X0 or D0")
    } else {

      A<-as.matrix(A0%>%dplyr::select(c(A0$sector)))
      L<-solve(diag(nrow(A))-A);



      if(!is.null(D0)){
         # Domestic and trade
         Dt <- D0 %>% dplyr::left_join(Trade0,by=c("sector")) %>% dplyr::mutate(domTrade=domestic+trade)
         print("Calculating X based on D0 provided and Trade")
          Xi <-(L%*%as.matrix(Dt$domTrade))%>% tibble::as_tibble()
          names(Xi)<-"processed"
          Xi <- Xi %>% dplyr::bind_cols(Dt%>%dplyr::select(sector))

          # X = If Xi is greater than Cap then adjust Xi to Cap and allocate rest to imports
          # Surpluss = Cap - Xi

          X <- Xi %>%
            dplyr::left_join(Cap0%>%dplyr::select(sector,cap), by=c("sector")) %>%
            dplyr::left_join(Trade0%>%dplyr::select(sector,trade), by=c("sector")) %>%
            dplyr::mutate(cap = dplyr::case_when(cap<0~processed,
                                          TRUE~cap),
                          trade = dplyr::case_when(processed>cap~(processed-cap)-trade,
                                                TRUE~trade),
                          processed = dplyr::case_when(processed>cap~cap,
                                                TRUE~processed),
                          surplus = cap-processed)


      } else {
        if(!is.null(X0)){

          Xi <- X0; print("Using X0 provided")

          # X = If Xi is greater than Cap then adjust Xi to Cap and allocate rest to imports
          # Surpluss = Cap - Xi

          X <- Xi %>%
            dplyr::left_join(Cap0%>%dplyr::select(sector,cap), by=c("sector")) %>%
            dplyr::left_join(Trade0%>%dplyr::select(sector,trade), by=c("sector")) %>%
            dplyr::mutate(cap = dplyr::case_when(cap<0~processed,
                                          TRUE~cap),
                          trade = dplyr::case_when(processed>cap~(processed-cap)-trade,
                                            TRUE~trade),
                          processed = dplyr::case_when(processed>cap~cap,
                                                TRUE~processed),
                          surplus = cap-processed)
          }
      }

    # Calculate z0 based on X0
    if(!is.null(Z0)){
      if(!is.null(X0)){print(paste("Recaclulating Z based on A0 and X0 provided. Orig Z0 provided = ",sep=""));
                       X0D0="X0"}
      if(!is.null(D0)){print(paste("Recaclulating Z based on A0 and D0 provided. Orig Z0 provided = ",sep=""));
                       X0D0="D0"}
      print(Z0 %>% dplyr::select(-scenario,-x,-region,-unit,-classPalette,-param,-class))}

    Z <- A %*% diag(as.vector(t(as.matrix(X$processed))))
    if(!"sector" %in% names(A0)){stop(
      paste("Need to format A0 to have a 'sector' column corresponding to each sector. eg. ",
            tibble::tibble(sector=c("ag","wat"),ag=c(5,0),wat=c(10,2)),sep=""))}
    Z <- dplyr::bind_cols(A0%>%dplyr::select(sector),tibble::as_tibble(Z))
    names(Z)<-c("sector",A0$sector)
    print(paste("Calculated Z based on A0 and ",X0D0," provided  = ",sep=""))
    print("Z");print(Z);print("A0");print(A0);
    if(!is.null(X0)){print("X0");print(X0)}
    if(!is.null(D0)){print("D0");print(D0)}

    # Calculate ioTbl
    ioTbl <- Z %>%
      dplyr::mutate(internal= Z %>%
                      dplyr::select(c(Z$sector)) %>%
                      rowSums) %>%
      dplyr::bind_cols(X %>%
                         dplyr::select(processed, cap, trade, surplus)) %>%
      dplyr::mutate(local = processed - internal,
                    domestic = local + trade) %>%
      addMissing()

    D = ioTbl %>%
      dplyr::select(-c(internal,processed,cap,surplus,Z$sector))

    D_Orig <-D;
    Z_Orig <-Z;
    A_Orig <-A;
    L_Orig <-L;
    X_Orig <-X;
    sol_Orig <- ioTbl %>% dplyr::select(c(sector,ioTbl$sector,internal,local, processed, cap, surplus, trade, domestic,
                                          names(ioTblNew)[!names(ioTblNew) %in% c("sector",ioTblNew$sector,
                                                                                  internal,local, processed, cap, surplus, trade, domestic)]));


    }

  } else {

   #---------------------------------------
   # IF A0 is not provided. Need Z0 and either X0 or D0/Trade0.
   # If X0 provided D is calculated, else if D0/Trade0 provided X is calculated.
   # If Trade0 is not provided it is assumed as 0.
   # i. Calculate L and then initial X -> L x (D0 + Trade)
   # ii. Calculate if X < cap then trade = X-Cap; else trade = 0 and surpluss = Cap - X.
   # iii. Calculate X adjusted for Capacity
   # iv. Calculate: ioTbl, Z, D, Trade, X, , Surpluss, A, L
   # Calculate: ioTbl, Z, D, X, A, L
   #---------------------------------------

  if(!is.null(Z0)){

    Z = Z0

    if(!is.null(X0)){

      Xi <- X0; print("Using X0 provided")

      # X = If Xi is greater than Cap then adjust Xi to Cap and allocate rest to imports
      # Surpluss = Cap - Xi

      X <- Xi %>%
        dplyr::left_join(Cap0%>%dplyr::select(sector,cap), by=c("sector")) %>%
        dplyr::left_join(Trade0%>%dplyr::select(sector,trade), by=c("sector")) %>%
        dplyr::mutate(cap = dplyr::case_when(cap<0~processed,
                                      TRUE~cap),
                      trade = dplyr::case_when(processed>cap~(processed-cap)-trade,
                                        TRUE~trade),
                      processed = dplyr::case_when(processed>cap~cap,
                                            TRUE~processed),
                      surplus = cap-processed)

      # Calculate ioTbl
      ioTbl <- Z %>%
        dplyr::mutate(internal= Z %>%
                        dplyr::select(c(Z$sector)) %>%
                        rowSums) %>%
        dplyr::bind_cols(X %>%
                           dplyr::select(processed, cap, trade, surplus)) %>%
        dplyr::mutate(local = processed - internal,
                      domestic = local + trade) %>%
        addMissing()

      D = ioTbl %>%
        dplyr::select(-c(internal,processed,cap,surplus,Z$sector))

      }

    if(!is.null(D0)){


      X <- Z %>%
        dplyr::mutate(internal= Z %>%
                      dplyr::select(c(Z$sector)) %>%
                      rowSums) %>%
        dplyr::bind_cols(D0 %>%
                         dplyr::select(domestic)) %>%
        dplyr::mutate(processed = domestic + internal) %>%
        dplyr::left_join(Cap0%>%dplyr::select(sector,cap), by=c("sector")) %>%
        dplyr::left_join(Trade0%>%dplyr::select(sector,trade), by=c("sector")) %>%
        dplyr::mutate(cap = dplyr::case_when(cap<0~processed,
                                      TRUE~cap),
                      trade = dplyr::case_when(processed>cap~(processed-cap)-trade,
                                        TRUE~trade),
                      processed = dplyr::case_when(processed>cap~cap,
                                            TRUE~processed),
                      surplus = cap-processed)

      # Calculate ioTbl
      ioTbl <- Z %>%
        dplyr::mutate(internal= Z %>%
                        dplyr::select(c(Z$sector)) %>%
                        rowSums) %>%
        dplyr::bind_cols(X %>%
                           dplyr::select(processed, cap, trade, surplus)) %>%
        dplyr::mutate(local = processed - internal,
                      domestic = local + trade) %>%
        addMissing()

      D = ioTbl %>%
        dplyr::select(-c(internal,processed,cap,surplus,Z$sector))

    }

    # Calculate A and L
    A<-as.matrix(Z%>%dplyr::select(c(Z$sector))) %*% as.matrix(ioTbl$processed^-1*diag(nrow(Z)))
    L<-solve(diag(nrow(A))-A);

  } else { stop("No A0 or Z0 provided.")}

    D_Orig <-D %>% dplyr::select(sector,domestic);
    Z_Orig <-Z;
    A_Orig <-A;
    L_Orig <-L;
    X_Orig <-X;
    sol_Orig <- ioTbl %>% dplyr::select(c(sector,ioTbl$sector,internal,local, processed, cap, surplus, trade, domestic,
                                          names(ioTblNew)[!names(ioTblNew) %in% c("sector",ioTblNew$sector,
                                                                                  internal,local, processed, cap, surplus, trade, domestic)]));


    } # if A0 not provided loop.


    #---------------------------------------
    # Calibrated Solution Based on ZPartial
    #---------------------------------------

    if(!is.null(ZPartial)){

      Z_Origl<-tidyr::gather(Z_Orig,-sector,key="To",value="value");Z_Origl
      ZPartiall<-tidyr::gather(ZPartial,-sector,key="To",value="valueP");ZPartiall
      Z_Calibl<-dplyr::left_join(Z_Origl,ZPartiall,by=c("sector","To")) %>%
        dplyr::mutate(value=dplyr::case_when(!is.na(valueP)~as.numeric(valueP),
                                      TRUE~as.numeric(value))) %>%
        dplyr::select(-valueP);

      Z_Calib <- tidyr::spread(Z_Calibl, key="To",value="value");Z_Calib

      Z = Z_Calib
      ioTblNew <- Z %>%
        dplyr::mutate(internal= Z %>%
                        dplyr::select(c(Z$sector)) %>%
                        rowSums) %>%
        dplyr::left_join(X_Orig %>%
                           dplyr::select(processed,trade, cap, surplus,sector)) %>%
        dplyr::mutate(local = processed - internal,
                      domestic = local + trade) %>%
        addMissing()

      A_ZPartial<-as.matrix(Z%>%dplyr::select(c(Z$sector))) %*% as.matrix(ioTblNew$processed^-1*diag(nrow(Z)))
      L_ZPartial<-solve(diag(nrow(A_ZPartial))-A_ZPartial);
      D_ZPartial<-ioTblNew %>% dplyr::select(sector,domestic);
      Z_ZPartial<- Z %>% dplyr::select(c(sector,Z$sector));
      X_ZPartial<-ioTblNew %>% dplyr::select(sector,processed);
      sol_ZPartial <- ioTblNew %>%
        dplyr::select(c(sector,ioTblNew$sector,internal,local, processed, cap, surplus, trade, domestic,
                        names(ioTblNew)[!names(ioTblNew) %in% c("sector",ioTblNew$sector,
                                                                internal,local, processed, cap, surplus, trade, domestic)]));

    }




    #---------------------------------------
    # New solutions based on DNew, XNew, ANew or ZNew
    #---------------------------------------

    #---------------------------------------
    # DNew
    # Will calculate new X, Z and ioTblNew based on existing A and L
    #---------------------------------------

  if(!is.null(DNew)){

      D = tibble::as_tibble(DNew);
      Z <- tibble::as_tibble(A_Orig%*%diag(x=(as.vector(L_Orig%*%as.matrix(D$domestic)))))
      names(Z)=ioTbl$sector;
      Z <- Z %>% dplyr::bind_cols(D %>% dplyr::select(sector))

      X <- Z %>%
        dplyr::mutate(internal= Z %>%
                        dplyr::select(c(Z$sector)) %>%
                        rowSums) %>%
        dplyr::left_join(D %>%
                           dplyr::select(domestic,sector)) %>%
        dplyr::mutate(processed = domestic + internal) %>%
        dplyr::left_join(Cap0%>%dplyr::select(sector,cap), by=c("sector")) %>%
        dplyr::left_join(Trade0%>%dplyr::select(sector,trade), by=c("sector")) %>%
        dplyr::mutate(cap = dplyr::case_when(cap<0~processed,
                                      TRUE~cap),
                      trade = dplyr::case_when(processed>cap~(processed-cap)-trade,
                                        TRUE~trade),
                      processed = dplyr::case_when(processed>cap~cap,
                                            TRUE~processed),
                      surplus = cap-processed)

      # Calculate ioTbl
      ioTblNew <- Z %>%
        dplyr::mutate(internal= Z %>%
                        dplyr::select(c(Z$sector)) %>%
                        rowSums) %>%
        dplyr::bind_cols(X %>%
                           dplyr::select(processed, cap, trade, surplus)) %>%
        dplyr::mutate(local = processed - internal,
                      domestic = local + trade) %>%
        addMissing()


      A_DNew <-as.matrix(Z%>%dplyr::select(c(Z$sector))) %*% as.matrix(ioTblNew$processed^-1*diag(nrow(Z)))
      L_DNew <-solve(diag(nrow(A_DNew))-A_DNew);
      D_DNew <-DNew %>% dplyr::select(sector,domestic);
      Z_DNew <-Z %>% dplyr::select(c(sector,Z$sector));
      X_DNew <-ioTblNew %>% dplyr::select(sector,processed);
      sol_DNew <-ioTblNew %>%
        dplyr::select(c(sector,ioTblNew$sector,internal,local, processed, cap, surplus, trade, domestic,
                        names(ioTblNew)[!names(ioTblNew) %in% c("sector",ioTblNew$sector,
                                                                internal,local, processed, cap, surplus, trade, domestic)]));
  }


    #---------------------------------------
    # ANew
    # Will calculate new L, Z, X and ioTblNew based on existing D
    #---------------------------------------

    if(!is.null(ANew)){

      A1 <- as.matrix(ANew %>% dplyr::select(c(ioTbl$sector)))
      L1 <- solve(diag(nrow(A1))-A1);
      D <- D_Orig
      Z <- tibble::as_tibble(A1%*%diag(x=(as.vector(L1%*%as.matrix(D$domestic)))))
      names(Z)=ioTbl$sector;
      Z <- Z %>% dplyr::bind_cols(D %>% dplyr::select(sector))

      X <- Z %>%
        dplyr::mutate(internal= Z %>%
                        dplyr::select(c(Z$sector)) %>%
                        rowSums) %>%
        dplyr::left_join(D %>%
                           dplyr::select(domestic,sector)) %>%
        dplyr::mutate(processed = domestic + internal) %>%
        dplyr::left_join(Cap0%>%dplyr::select(sector,cap), by=c("sector")) %>%
        dplyr::left_join(Trade0%>%dplyr::select(sector,trade), by=c("sector")) %>%
        dplyr::mutate(cap = dplyr::case_when(cap<0~processed,
                                      TRUE~cap),
                      trade = dplyr::case_when(processed>cap~(processed-cap)-trade,
                                        TRUE~trade),
                      processed = dplyr::case_when(processed>cap~cap,
                                            TRUE~processed),
                      surplus = cap-processed)

      # Calculate ioTbl
      ioTblNew <- Z %>%
        dplyr::mutate(internal= Z %>%
                        dplyr::select(c(Z$sector)) %>%
                        rowSums) %>%
        dplyr::left_join(X %>%
                           dplyr::select(processed, cap, trade, surplus,sector)) %>%
        dplyr::mutate(local = processed - internal,
                      domestic = local + trade) %>%
        addMissing()

      A_ANew <-as.matrix(Z%>%dplyr::select(c(Z$sector))) %*% as.matrix(ioTblNew$processed^-1*diag(nrow(Z)))
      L_ANew <-solve(diag(nrow(A_ANew))-A_ANew);
      D_ANew <-ioTblNew %>% dplyr::select(sector,domestic);
      Z_ANew <-Z %>% dplyr::select(c(sector,Z$sector));
      X_ANew <-ioTblNew %>% dplyr::select(sector,processed);
      sol_ANew <- ioTblNew %>%
        dplyr::select(c(sector,ioTblNew$sector,internal,local, processed, cap, surplus, trade, domestic,
                        names(ioTblNew)[!names(ioTblNew) %in% c("sector",ioTblNew$sector,
                                                                internal,local, processed, cap, surplus, trade, domestic)]));

    }

    #---------------------------------------
    # ZNew
    # Will calculate new A, L, Z, X and ioTblNew based on existing D
    #---------------------------------------

    if(!is.null(ZNew)){

      Z = ZNew

      X <- Z %>%
        dplyr::mutate(internal= Z %>%
                        dplyr::select(c(Z$sector)) %>%
                        rowSums) %>%
        dplyr::left_join(D_Orig %>%
                           dplyr::select(domestic,sector)) %>%
        dplyr::mutate(processed = domestic + internal) %>%
        dplyr::left_join(Cap0%>%dplyr::select(sector,cap), by=c("sector")) %>%
        dplyr::left_join(Trade0%>%dplyr::select(sector,trade), by=c("sector")) %>%
        dplyr::mutate(cap = dplyr::case_when(cap<0~processed,
                                      TRUE~cap),
                      trade = dplyr::case_when(processed>cap~(processed-cap)-trade,
                                        TRUE~trade),
                      processed = dplyr::case_when(processed>cap~cap,
                                            TRUE~processed),
                      surplus = cap-processed)

      # Calculate ioTbl
      ioTblNew <- Z %>%
        dplyr::mutate(internal= Z %>%
                        dplyr::select(c(Z$sector)) %>%
                        rowSums) %>%
        dplyr::left_join(X %>%
                           dplyr::select(processed, cap, trade, surplus,sector)) %>%
        dplyr::mutate(local = processed - internal,
                      domestic = local + trade) %>%
        addMissing()


        A_ZNew<-as.matrix(Z%>%dplyr::select(c(Z$sector))) %*% as.matrix(ioTblNew$processed^-1*diag(nrow(Z)))
        L_ZNew<-solve(diag(nrow(A_ZNew))-A_ZNew);
        D_ZNew<-ioTblNew %>% dplyr::select(sector,domestic);
        Z_ZNew<- Z %>% dplyr::select(c(sector,Z$sector));
        X_ZNew<-ioTblNew %>% dplyr::select(sector,processed);
        sol_ZNew <- ioTblNew %>%
          dplyr::select(c(sector,ioTblNew$sector,internal,local, processed, cap, surplus, trade, domestic,
                          names(ioTblNew)[!names(ioTblNew) %in% c("sector",ioTblNew$sector,
                                                                  internal,local, processed, cap, surplus, trade, domestic)]));

    }

      #---------------------------------------
      # XNew
      # Will calculate new Z, A, L and ioTblNew based on existing D, A and L
      #---------------------------------------

      if(!is.null(XNew)){

        D = tibble::as_tibble(D_Orig);
        Z <- (A_Orig %*% diag(as.vector(t(as.matrix(XNew$processed))))) %>% tibble::as_tibble()
        names(Z)<-sol_Orig$sector
        Z <- dplyr::bind_cols(sol_Orig%>%dplyr::select(sector),Z)

        X <- Z %>%
          dplyr::mutate(internal= Z %>%
                          dplyr::select(c(Z$sector)) %>%
                          rowSums) %>%
          dplyr::left_join(D_Orig %>%
                             dplyr::select(domestic,sector)) %>%
          dplyr::mutate(processed = domestic + internal) %>%
          dplyr::left_join(Cap0%>%dplyr::select(sector,cap), by=c("sector")) %>%
          dplyr::left_join(Trade0%>%dplyr::select(sector,trade), by=c("sector")) %>%
          dplyr::mutate(cap = dplyr::case_when(cap<0~processed,
                                        TRUE~cap),
                        trade = dplyr::case_when(processed>cap~(processed-cap)-trade,
                                          TRUE~trade),
                        processed = dplyr::case_when(processed>cap~cap,
                                              TRUE~processed),
                        surplus = cap-processed)

        # Calculate ioTbl
        ioTblNew <- Z %>%
          dplyr::mutate(internal= Z %>%
                          dplyr::select(c(Z$sector)) %>%
                          rowSums) %>%
          dplyr::left_join(X %>%
                             dplyr::select(processed, cap, trade, surplus,sector)) %>%
          dplyr::mutate(local = processed - internal,
                        domestic = local + trade) %>%
          addMissing()

        A_XNew <-as.matrix(Z%>%dplyr::select(c(Z$sector))) %*% as.matrix(ioTblNew$processed^-1*diag(nrow(Z)))
        L_XNew <-solve(diag(nrow(A_XNew))-A_XNew);
        D_XNew <-XNew %>% dplyr::select(sector,domestic);
        Z_XNew <-Z %>% dplyr::select(c(sector,Z$sector));
        X_XNew <-ioTblNew %>% dplyr::select(sector,processed);
        sol_XNew <- ioTblNew %>%
          dplyr::select(c(sector,ioTblNew$sector,internal,local, processed, cap, surplus, trade, domestic,
                          names(ioTblNew)[!names(ioTblNew) %in% c("sector",ioTblNew$sector,
                                                                  internal,local, processed, cap, surplus, trade, domestic)]));

      }


      A_ANew1= A_ANew1 %>% dplyr::bind_rows(A_ANew1,A_ANew %>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      L_ANew1 = L_ANew1 %>% dplyr::bind_rows(L_ANew1,L_ANew%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      sol_ANew1 = sol_ANew1 %>% dplyr::bind_rows(sol_ANew1,sol_ANew%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      A_DNew1 = A_DNew1 %>% dplyr::bind_rows(A_DNew1,A_DNew%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      L_DNew1 = L_DNew1 %>% dplyr::bind_rows(L_DNew1,A_DNew%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      sol_DNew1 = sol_DNew1 %>% dplyr::bind_rows(sol_DNew1,sol_DNew%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      A_ZNew1 = A_ZNew1 %>% dplyr::bind_rows(A_ZNew1,A_ZNew%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      L_ZNew1 = L_ZNew1 %>% dplyr::bind_rows(L_ZNew1,L_ZNew%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      sol_ZNew1 = sol_ZNew1 %>% dplyr::bind_rows(sol_ZNew1,sol_ZNew%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      A_XNew1 = A_XNew1 %>% dplyr::bind_rows(A_XNew1,A_XNew%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      L_XNew1 = L_XNew1 %>% dplyr::bind_rows(L_XNew1,L_XNew%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      sol_XNew1 = sol_XNew1 %>% dplyr::bind_rows(sol_XNew1,sol_XNew%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      A_ZPartial1 = A_ZPartial1 %>% dplyr::bind_rows(A_ZPartial1,A_ZPartial%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      L_ZPartial1 = L_ZPartial1 %>% dplyr::bind_rows(L_ZPartial1,L_ZPartial%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      sol_ZPartial1 = sol_ZPartial1 %>% dplyr::bind_rows(sol_ZPartial1,sol_ZPartial%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      A_Orig1 = A_Orig1 %>% dplyr::bind_rows(A_Orig1,A_Orig%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      L_Orig1 = L_Orig1 %>% dplyr::bind_rows(L_Orig1,L_Orig%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))
      sol_Orig1 = sol_Orig1 %>% dplyr::bind_rows(sol_Orig1,sol_Orig%>% tibble::as_tibble() %>% dplyr::mutate(scenario=scenario_i,x=year_i,subRegion=subRegion_i))


    } # Close loop scenario
  } # close loop subRegion
} # close loop year


  print(list(A_ANew1=A_ANew1, L_ANew1=L_ANew1, sol_ANew1=sol_ANew1,
             A_DNew1=A_DNew1, L_DNew1=L_DNew1, sol_DNew1=sol_DNew1,
             A_ZNew1=A_ZNew1, L_ZNew1=L_ZNew1, sol_ZNew1=sol_ZNew1,
             A_XNew1=A_XNew1, L_XNew1=L_XNew1, sol_XNew1=sol_XNew1,
             A_ZPartial1=A_ZPartial1, L_ZPartial1=L_ZPartial1, sol_ZPartial1=sol_ZPartial1,
             A_Orig1=A_Orig1, L_Orig1=L_Orig1, sol_Orig1=sol_Orig1
             ))



  # Print Figure Function
  printf <- function(printFig=T,
                     fileName="file",
                     dir,
                     figure,
                     figWidth=13,
                     figHeight=9,
                     pdfpng="pdf"){
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

  sol<-sol_Orig1 %>%
    dplyr::filter(scenario==scenario_i) %>%
    dplyr::select(unique(sol_Orig1$sector), internal, local, trade,  processed, sector,
                  subRegion,x)

  df_Mn<-sol %>%
    #dplyr::select (-processed,processed) %>% # to place processed last for following function
    dplyr::mutate_at(vars(-sector,-subRegion,-x),dplyr::funs(./processed)); df_Mn

  solx <- sol %>%
    tidyr::gather(-sector,-subRegion,-x,key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=sector) %>%
    dplyr::arrange(sectorFrom);

  df_Mnx <- df_Mn %>%
    tidyr::gather(-sector,-subRegion,-x,key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=sector) %>%
    dplyr::arrange(sectorFrom);

  # ioTable normalized bubbles

  ga <- ggplot(df_Mnx, aes(y = sectorFrom, x = sectorTo)) +
    labs(subtitle="test",
         title=paste(scenario_i,"_solOrig1_norm_bubble",sep="")) +
    scale_x_discrete(expand = c(0.1,0.1)) +
    geom_point(aes(col=value, size=value)) +
    geom_text(aes(label=round(value,2)),col="red") +
    coord_fixed(ratio = 1) +
    scale_x_discrete(limits = c(unique(df_Mnx$sectorFrom),"internal", "local","processed",
                                "trade"), expand = c(0.1,0.1)) +
    scale_size_continuous(range = c(1,20)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    facet_grid(x~subRegion); ga

  printf(figure=ga,fileName = paste(scenario_i,"_solOrig1_norm_bubble",sep=""), dir=dir)

  # ioTable normalized values

  gb <- ggplot(df_Mnx, aes(y = sectorFrom, x = sectorTo)) +
    labs(subtitle="test",
         title=paste(scenario_i,"_solOrig1_norm_values",sep="")) +
    scale_x_discrete(limits = c(unique(df_Mnx$sectorFrom),"internal", "local","processed",
                                "trade"), expand = c(0.1,0.1)) +
    scale_size_continuous(range = c(1,20)) +
    geom_text(aes(label=round(value,1)),col="black") +
    coord_fixed(ratio = 1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_grid(x~subRegion); gb

  printf(figure=gb,fileName = paste(scenario_i,"_solOrig1_norm_values",sep=""), dir=dir)


  # ioTable absolute values

  gc <- ggplot(solx, aes(y = sectorFrom, x = sectorTo)) +
    labs(subtitle="test",
         title=paste(scenario_i,"_solOrig1_absolute_values",sep="")) +
    scale_x_discrete(limits = c(unique(df_Mnx$sectorFrom),"internal", "local","processed",
                                "trade"), expand = c(0.1,0.1)) +
    scale_size_continuous(range = c(1,20)) +
    geom_text(aes(label=round(value,1)),col="black") +
    coord_fixed(ratio = 1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_grid(x~subRegion); gc

  printf(figure=gc,fileName = paste(scenario_i,"_solOrig1_absolute_values",sep=""), dir=dir)


  #-----------------
  # Sankey
  #--------------

  solFlows <- sol_Orig1 %>%
    dplyr::filter(scenario==scenario_i) %>%
    dplyr::select(sector, unique(sol_Orig1$sector),local,subRegion,x)
  df <- solFlows;df

  dfx <- df %>%
    tidyr::gather(-sector,-x,-subRegion,key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=sector) %>%
    dplyr::filter(value>0) %>%
    unique() %>%
    dplyr::arrange(sectorFrom); dfx

  g<-ggplot(as.data.frame(dfx%>%dplyr::filter(value!=0)),
            aes(y = value, axis1 = sectorFrom, axis2 = sectorTo, group=subRegion)) +
    ggalluvial::geom_alluvium(aes(fill = sectorFrom), width = 1/12, color="black") +
    ggalluvial::geom_stratum(width = 1/12, fill = "grey30", color = "grey", alpha=1) +
    geom_label(stat = "stratum", label.strata = TRUE) +
    scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
    scale_fill_brewer(type = "qual", palette = "Set1") +
    facet_grid(x~subRegion) +
    ggtitle(paste(scenario_i,"_solOrig1_absolute_sankey",sep=""))+theme_bw();g


  printf(figure=g,fileName = paste(scenario_i,"_solOrig1_absolute_sankey",sep=""), dir=dir)


  #---------------------
  # sol_ZPartial1
  #---------------------


  if(nrow(sol_ZPartial1)>0){

  # ioTable normalized

  sol<-sol_ZPartial1 %>%
    dplyr::filter(scenario==scenario_i) %>%
    dplyr::select(unique(sol_ZPartial1$sector), internal, local, trade,  processed, sector,
                  subRegion,x)

  df_Mn<-sol %>%
    #dplyr::select (-processed,processed) %>% # to place processed last for following function
    dplyr::mutate_at(vars(-sector,-subRegion,-x),dplyr::funs(./processed)); df_Mn

  solx <- sol %>%
    tidyr::gather(-sector,-subRegion,-x,key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=sector) %>%
    dplyr::arrange(sectorFrom);

  df_Mnx <- df_Mn %>%
    tidyr::gather(-sector,-subRegion,-x,key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=sector) %>%
    dplyr::arrange(sectorFrom);

  # ioTable normalized bubbles

  ga <- ggplot(df_Mnx, aes(y = sectorFrom, x = sectorTo)) +
    labs(subtitle="test",
         title=paste(scenario_i,"_solZPartial1_norm_bubble",sep="")) +
    scale_x_discrete(expand = c(0.1,0.1)) +
    geom_point(aes(col=value, size=value)) +
    geom_text(aes(label=round(value,2)),col="red") +
    coord_fixed(ratio = 1) +
    scale_x_discrete(limits = c(unique(df_Mnx$sectorFrom),"internal", "local","processed",
                                "trade"), expand = c(0.1,0.1)) +
    scale_size_continuous(range = c(1,20)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    facet_grid(x~subRegion); ga

  printf(figure=ga,fileName = paste(scenario_i,"_solZPartial1_norm_bubble",sep=""), dir=dir)

  # ioTable normalized values

  gb <- ggplot(df_Mnx, aes(y = sectorFrom, x = sectorTo)) +
    labs(subtitle="test",
         title=paste(scenario_i,"_solZPartial1_norm_values",sep="")) +
    scale_x_discrete(limits = c(unique(df_Mnx$sectorFrom),"internal", "local","processed",
                                "trade"), expand = c(0.1,0.1)) +
    scale_size_continuous(range = c(1,20)) +
    geom_text(aes(label=round(value,1)),col="black") +
    coord_fixed(ratio = 1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_grid(x~subRegion); gb

  printf(figure=gb,fileName = paste(scenario_i,"_solZPartial1_norm_values",sep=""), dir=dir)


  # ioTable absolute values

  gc <- ggplot(solx, aes(y = sectorFrom, x = sectorTo)) +
    labs(subtitle="test",
         title=paste(scenario_i,"_solZPartial1_absolute_values",sep="")) +
    scale_x_discrete(limits = c(unique(df_Mnx$sectorFrom),"internal", "local","processed",
                                "trade"), expand = c(0.1,0.1)) +
    scale_size_continuous(range = c(1,20)) +
    geom_text(aes(label=round(value,1)),col="black") +
    coord_fixed(ratio = 1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_grid(x~subRegion); gc

  printf(figure=gc,fileName = paste(scenario_i,"_solZPartial1_absolute_values",sep=""), dir=dir)


  #-----------------
  # Sankey
  #--------------

  solFlows <- sol_ZPartial1 %>%
    dplyr::filter(scenario==scenario_i) %>%
    dplyr::select(sector, unique(sol_ZPartial1$sector),local,subRegion,x)
  df <- solFlows;df

  dfx <- df %>%
    tidyr::gather(-sector,-x,-subRegion,key="sectorTo",value="value") %>%
    dplyr::rename (sectorFrom=sector) %>%
    dplyr::filter(value>0) %>%
    unique() %>%
    dplyr::arrange(sectorFrom); dfx

  g<-ggplot(as.data.frame(dfx%>%dplyr::filter(value!=0)),
            aes(y = value, axis1 = sectorFrom, axis2 = sectorTo, group=subRegion)) +
    ggalluvial::geom_alluvium(aes(fill = sectorFrom), width = 1/12, color="black") +
    ggalluvial::geom_stratum(width = 1/12, fill = "grey30", color = "grey", alpha=1) +
    geom_label(stat = "stratum", label.strata = TRUE) +
    scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
    scale_fill_brewer(type = "qual", palette = "Set1") +
    facet_grid(x~subRegion) +
    ggtitle(paste(scenario_i,"_solZPartial1_absolute_sankey",sep=""))+theme_bw();g


  printf(figure=g,fileName = paste(scenario_i,"_solZPartial1_absolute_sankey",sep=""), dir=dir)

  } # If sol_ZPartial1 is null

  }

  return(list(A_ANew1=A_ANew1, L_ANew1=L_ANew1, sol_ANew1=sol_ANew1,
             A_DNew1=A_DNew1, L_DNew1=L_DNew1, sol_DNew1=sol_DNew1,
             A_ZNew1=A_ZNew1, L_ZNew1=L_ZNew1, sol_ZNew1=sol_ZNew1,
             A_XNew1=A_XNew1, L_XNew1=L_XNew1, sol_XNew1=sol_XNew1,
             A_ZPartial1=A_ZPartial1, L_ZPartial1=L_ZPartial1, sol_ZPartial1=sol_ZPartial1,
             A_Orig1=A_Orig1, L_Orig1=L_Orig1, sol_Orig1=sol_Orig1
  ))



} # Close Function
