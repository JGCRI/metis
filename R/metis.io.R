#' metis.io
#'
#' This function prepares gridded data for use with domestic metis modules.
#' @param dirOutputs Default =paste(getwd(),"/outputs",sep=""),
#' @param Z0 Default = NULL,
#' @param D0 Default = NULL,
#' @param X0 Default = NULL,
#' @param A0 Default = NULL,
#' @param DNew Default =NULL,
#' @param XNew Default =NULL,
#' @param ZNew Default =NULL,
#' @param ANew Default =NULL,
#' @param ZPartial Default =NULL,
#' @return A table with data by polygon ID for each shapefile provided
#' @keywords gcam, gcam database, query
#' @export


metis.io<-function(Z0=NULL,
                 D0=NULL,
                 X0=NULL,
                 A0=NULL,
                 Trade0=NULL,
                 Cap0=NULL,
                 DNew=NULL,
                 XNew=NULL,
                 ZNew=NULL,
                 ANew=NULL,
                 ZPartial=NULL,
                 dirOutputs=paste(getwd(),"/outputs",sep="")
                        ){
#
#   Z0=NULL
#   D0=NULL
#   X0=NULL
#   A0=NULL
#   Trade0=NULL
#   Cap0=NULL
#   DNew=NULL
#   XNew=NULL
#   ZNew=NULL
#   ANew=NULL
#   ZPartial=NULL
#   dirOutputs=paste(getwd(),"/outputs",sep="")

#----------------
# Initialize variables by setting to NULL
#----------------

NULL->year->sector->domestic->export->processed->V1->value->.->domestic->production->domestic->
    sol_Orig->Z->A->X->D-> classPalette -> internal -> param -> region -> scenario -> x ->
    A_Orig -> L_Orig -> sol_Orig ->
    A_ANew -> L_ANew -> sol_ANew ->
    A_DNew -> L_DNew -> sol_DNew ->
    A_ZNew -> L_ZNew -> sol_ZNew ->
    A_XNew -> L_XNew -> sol_XNew ->
    A_ZPartial -> L_ZPartial -> sol_ZPartial

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
    data<-data%>%dplyr::mutate(x=year)}else{data<-data%>%dplyr::mutate(x=0)}}
  if(!"region"%in%names(data)){data<-data%>%dplyr::mutate(region="region")}
  if(!"subRegion"%in%names(data)){data<-data%>%dplyr::mutate(region="subRegion")}
  if(!"unit"%in%names(data)){data<-data%>%dplyr::mutate(unit="unit")}
  if(!"classPalette"%in%names(data)){data<-data%>%dplyr::mutate(classPalette="pal_hot")}
  if(!"param"%in%names(data)){data<-data%>%dplyr::mutate(param="param")}
  if(!"class"%in%names(data)){data<-data%>%dplyr::mutate(class="class")}
  return(data)
}


  # ------------------
  # Check Columns
  # ------------------

  if(is.null(Z0) & is.null(D0) & is.null(X0) & is.null(A0)){stop("Need to provide atleast two of Z0, X0, D0, A0")}


  if(!is.null(Z0)){
    Z0 <- addMissing(Z0)
    if(!any(grepl("sector",colnames(Z0)))){
      stop(print(paste("Column names in Z0: ",paste(colnames(Z0),collapse=", ")," must include 'sector'.",sep="")))}
    Z0 <- Z0 %>% arrange (sector)
    sectors <- Z0 %>% dplyr::select(sector)
  }

  if(!is.null(ZNew)){
    ZNew <- addMissing(ZNew)
    if(!any(grepl("sector",colnames(ZNew)))){
      stop(print(paste("Column names in ZNew: ",paste(colnames(ZNew),collapse=", ")," must include 'sector'.",sep="")))}
    ZNew <- ZNew %>% arrange (sector)
    }

  if(!is.null(ZPartial)){
    ZPartial <- addMissing(ZPartial)
      if(!any(grepl("sector",colnames(ZPartial)))){
    stop(print(paste("Column names in ZPartial: ",paste(colnames(ZPartial),collapse=", ")," must include 'sector'.",sep="")))}
    ZPartial <- ZPartial %>% arrange (sector)
    }

  if(!is.null(D0)){
   D0 <- addMissing(D0)
   if(!any(grepl(c("sector"),colnames(D0)))){
      stop(print(paste("Column names in D0: ",paste(colnames(D0),collapse=", ")," must include 'sector'.",sep="")))}
    if(!any(grepl(c("domestic"),colnames(D0)))){
      stop(print(paste("Column names in D0: ",paste(colnames(D0),collapse=", "),"must include 'domestic' where domestic represents internal demands for each sector from domestic non-nexus sectors.",sep="")))}
   D0 <- D0 %>% arrange (sector)
   sectors <- D0 %>% dplyr::select(sector)
   }

  if(!is.null(DNew)){
    DNew <- addMissing(DNew)
    if(!any(grepl(c("sector"),colnames(DNew)))){
      stop(print(paste("Column names in DNew: ",paste(colnames(DNew),collapse=", ")," must include 'sector'.",sep="")))}
    if(!any(grepl(c("domestic"),colnames(DNew)))){
      stop(print(paste("Column names in DNew: ",paste(colnames(DNew),collapse=", "),"must include 'domestic' where domestic represents internal demands for each sector from domestic non-nexus sectors.",sep="")))}
    DNew <- DNew %>% arrange (sector)
    }


  if(!is.null(X0)){
    X0 <- addMissing(X0)
    if(!any(grepl("sector",colnames(X0)))){
      stop(print(paste("Column names in X0: ",paste(colnames(X0),collapse=", ")," must include 'sector'.",sep="")))}
    X0 <- X0 %>% arrange (sector)
    sectors <- X0 %>% dplyr::select(sector)
  }

  if(!is.null(XNew)){
    XNew <- addMissing(XNew)
    if(!any(grepl("sector",colnames(XNew)))){
      stop(print(paste("Column names in XNew: ",paste(colnames(XNew),collapse=", ")," must include 'sector'.",sep="")))}
    XNew <- XNew %>% arrange (sector)
  }


  if(!is.null(A0)){
  if(!any(grepl("sector",colnames(A0)))){
    stop(print(paste("Column names in ANew: ",paste(colnames(ANew),collapse=", ")," must include 'sector'.",sep="")))}
      A0 <- A0 %>% arrange (sector)
      sectors <- A0 %>% dplyr::select(sector)
    }


  if(!is.null(ANew)){
    ANew <- addMissing(ANew)
    if(!any(grepl("sector",colnames(ANew)))){
      stop(print(paste("Column names in ANew: ",paste(colnames(ANew),collapse=", ")," must include 'sector'.",sep="")))}
    ANew <- ANew %>% arrange (sector)
  }

  if(!is.null(Z0) & !is.null(D0) ){
    if(!identical(unique(Z0 %>%
                         dplyr::select(sector,unit)),
                  unique(D0 %>%
                         dplyr::select(sector,unit)))){
      print(Z0);print(D0)
      stop(print(paste("Z0 and D0 do not have identical sectors and units.",sep="")))
    }}

  if(!is.null(Trade0)){
    Trade0 <- addMissing(Trade0)
    if(!any(grepl("sector",colnames(Trade0)))){
      stop(print(paste("Column names in Trade0: ",paste(colnames(Trade0),collapse=", ")," must include 'sector'.",sep="")))}
    if(!any(grepl(c("trade"),colnames(Trade0)))){
      stop(print(paste("Column names in DNew: ",paste(colnames(DNew),collapse=", "),"must include 'trade' where domestic represents internal demands for each sector from domestic non-nexus sectors.",sep="")))}
    Trade0 <- Trade0 %>% arrange (sector)
  } else {
      Trade0 <- sectors %>% dplyr::mutate(trade=0)
  }

    if(!is.null(Cap0)){
      Cap0 <- addMissing(Cap0)
      if(!any(grepl("sector",colnames(Cap0)))){
        stop(print(paste("Column names in Cap0: ",paste(colnames(Cap0),collapse=", ")," must include 'sector'.",sep="")))}
      if(!any(grepl("cap",colnames(Cap0)))){
        stop(print(paste("Column names in Cap0: ",paste(colnames(Cap0),collapse=", ")," must include 'cap'.",sep="")))}
      Cap0 <- Cap0 %>% arrange (sector)
      sectors <- Cap0 %>% dplyr::select(sector)
    } else {
      Cap0 <- sectors %>% dplyr::mutate(cap=-1)
    }


 if(!is.null(D0) & !is.null(Trade0)){
   D0 <- D0 %>%
     dplyr::left_join(Trade0,by=c("sector")) %>%
     dplyr::mutate(domestic = domestic - trade) %>%
     dplyr::select(-trade)
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

      A<-as.matrix(A0%>%dplyr::select(-sector))
      L<-solve(diag(nrow(A))-A);

      if(!is.null(D0)){
         # Domestic and trade
         Dt <- D0 %>% left_join(Trade0,by=c("sector")) %>% mutate(domTrade=domestic+trade)
         print("Calculating X based on D0 provided and Trade")
          Xi <-(L%*%as.matrix(Dt$domTrade))%>% tibble::as_tibble()
          names(Xi)<-"processed"
          Xi <- Xi %>% dplyr::bind_cols(Dt%>%dplyr::select(sector))

          # X = If Xi is greater than Cap then adjust Xi to Cap and allocate rest to imports
          # Surpluss = Cap - Xi

          X <- Xi %>%
            dplyr::left_join(Cap0%>%dplyr::select(sector,cap), by=c("sector")) %>%
            dplyr::left_join(Trade0%>%dplyr::select(sector,trade), by=c("sector")) %>%
            dplyr::mutate(cap = case_when(cap<0~processed,
                                          TRUE~cap),
                          trade = case_when(processed>cap~(processed-cap)-trade,
                                                TRUE~trade),
                          processed = case_when(processed>cap~cap,
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
            dplyr::mutate(cap = case_when(cap<0~processed,
                                          TRUE~cap),
                          trade = case_when(processed>cap~(processed-cap)-trade,
                                            TRUE~trade),
                          processed = case_when(processed>cap~cap,
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
      print(Z0 %>% select(-scenario,-x,-region,-unit,-classPalette,-param,-class))}

    Z <- A %*% diag(as.vector(t(as.matrix(X$processed))))
    if(!"sector" %in% names(A0)){stop(
      paste("Need to format A0 to have a 'sector' column corresponding to each sector. eg. ",
            tibble::tibble(sector=c("ag","wat"),ag=c(5,0),wat=c(10,2)),sep=""))}
    Z <- dplyr::bind_cols(A0%>%dplyr::select(sector),tibble::as_tibble(Z))
    names(Z)<-names(A0)
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
    sol_Orig <- ioTbl %>% dplyr::select(c(sector,ioTblNew$sector,internal,local, processed, cap, surplus, trade, domestic,
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
        dplyr::mutate(cap = case_when(cap<0~processed,
                                      TRUE~cap),
                      trade = case_when(processed>cap~(processed-cap)-trade,
                                        TRUE~trade),
                      processed = case_when(processed>cap~cap,
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
        dplyr::mutate(cap = case_when(cap<0~processed,
                                      TRUE~cap),
                      trade = case_when(processed>cap~(processed-cap)-trade,
                                        TRUE~trade),
                      processed = case_when(processed>cap~cap,
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
    sol_Orig <- ioTbl %>% dplyr::select(c(sector,ioTblNew$sector,internal,local, processed, cap, surplus, trade, domestic,
                                          names(ioTblNew)[!names(ioTblNew) %in% c("sector",ioTblNew$sector,
                                                                                  internal,local, processed, cap, surplus, trade, domestic)]));


    } # if A0 not provided loop.


    #---------------------------------------
    # Calibrated Solution Based on ZPartial
    #---------------------------------------

    if(!is.null(ZPartial)){

      Z_Origl<-gather(Z_Orig,-sector,key="To",value="value");Z_Origl
      ZPartiall<-gather(ZPartial,-sector,key="To",value="valueP");ZPartiall
      Z_Calibl<-dplyr::left_join(Z_Origl,ZPartiall,by=c("sector","To")) %>%
        dplyr::mutate(value=case_when(!is.na(valueP)~as.numeric(valueP),
                                      TRUE~as.numeric(value))) %>%
        dplyr::select(-valueP);

      Z_Calib <- spread(Z_Calibl, key="To",value="value");Z_Calib

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
        dplyr::mutate(cap = case_when(cap<0~processed,
                                      TRUE~cap),
                      trade = case_when(processed>cap~(processed-cap)-trade,
                                        TRUE~trade),
                      processed = case_when(processed>cap~cap,
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
        dplyr::mutate(cap = case_when(cap<0~processed,
                                      TRUE~cap),
                      trade = case_when(processed>cap~(processed-cap)-trade,
                                        TRUE~trade),
                      processed = case_when(processed>cap~cap,
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
        dplyr::mutate(cap = case_when(cap<0~processed,
                                      TRUE~cap),
                      trade = case_when(processed>cap~(processed-cap)-trade,
                                        TRUE~trade),
                      processed = case_when(processed>cap~cap,
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
          dplyr::mutate(cap = case_when(cap<0~processed,
                                        TRUE~cap),
                        trade = case_when(processed>cap~(processed-cap)-trade,
                                          TRUE~trade),
                        processed = case_when(processed>cap~cap,
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

  print(list(A_ANew=A_ANew, L_ANew=L_ANew, sol_ANew=sol_ANew,
             A_DNew=A_DNew, L_DNew=L_DNew, sol_DNew=sol_DNew,
             A_ZNew=A_ZNew, L_ZNew=L_ZNew, sol_ZNew=sol_ZNew,
             A_XNew=A_XNew, L_XNew=L_XNew, sol_XNew=sol_XNew,
             A_ZPartial=A_ZPartial, L_ZPartial=L_ZPartial, sol_ZPartial=sol_ZPartial,
             A_Orig=A_Orig, L_Orig=L_Orig, sol_Orig=sol_Orig
             ))

  return(list(A_ANew=A_ANew, L_ANew=L_ANew, sol_ANew=sol_ANew,
              A_DNew=A_DNew, L_DNew=L_DNew, sol_DNew=sol_DNew,
              A_ZNew=A_ZNew, L_ZNew=L_ZNew, sol_ZNew=sol_ZNew,
              A_XNew=A_XNew, L_XNew=L_XNew, sol_XNew=sol_XNew,
              A_ZPartial=A_ZPartial, L_ZPartial=L_ZPartial, sol_ZPartial=sol_ZPartial,
              A_Orig=A_Orig, L_Orig=L_Orig, sol_Orig=sol_Orig
          ))


  # sol_Orig
  # Folder: Combined SubRegions + Years
  # ioTable normalized bubble plot
  # ioTable normalized values
  # ioTable absolute values
  # Sankey

  #------------------
  # Create Folders if needed
  #------------------

   if (!dir.exists(paste(dirOutputs, "/IO/CombSubReg", sep = ""))){
    dir.create(paste(dirOutputs, "/IO/ComSubReg", sep = ""))}

  dir<-paste(dirOutputs, "/IO/ComSubReg", sep = "")

  # ioTable normalized

  sol<-io$sol_Orig %>%
    dplyr::select(sector, io$sol_Orig$sector, internal, local, processed, cap, surplus, trade, domestic)

  df_Mn<-sol %>%
    select (-processed,processed) %>% # to place processed last for following function
    mutate_at(vars(-sector),funs(./processed)); df_Mn

  # ioTable normalized bubbles

  ga <- ggplot(df_Mnx, aes(y = sectorFrom, x = sectorTo)) +
    labs(subtitle="test",
         title="Bubble chart") +
    scale_x_discrete(expand = c(0.1,0.1)) +
    geom_point(aes(col=value, size=value)) +
    geom_text(aes(label=round(value,1)),col="red") +
    coord_fixed(ratio = 1) +
    scale_size_continuous(range = c(1,30)) +
    facet_grid(year~subRegion); ga

  # ioTable normalized values

  gb <- ggplot(df_Mnx, aes(y = sectorFrom, x = sectorTo)) +
    labs(subtitle="test",
         title="Bubble chart") +
    scale_x_discrete(limits = c(unique(df_Mn1$sectorFrom),"intermediateOutput", "processed"), expand = c(0.1,0.1)) +
    geom_text(aes(label=round(value,1)),col="black") +
    coord_fixed(ratio = 1) +
    scale_size_continuous(range = c(1,30)) +
    facet_grid(year~subRegion)+theme_bw(); gb

  # ioTable absolute values

  gb <- ggplot(sol, aes(y = sectorFrom, x = sectorTo)) +
    labs(subtitle="test",
         title="Bubble chart") +
    scale_x_discrete(limits = c(unique(df_Mn1$sectorFrom),"intermediateOutput", "processed"), expand = c(0.1,0.1)) +
    geom_text(aes(label=round(value,1)),col="black") +
    coord_fixed(ratio = 1) +
    scale_size_continuous(range = c(1,30)) +
    facet_grid(year~subRegion)+theme_bw(); gb

  #-----------------
  # Sankey
  #--------------

  solFlows <- io$sol_Orig %>%
    dplyr::select(sector, io$sol_Orig$sector,local,trade)
  df <- solFlows;df


  g<-ggplot(as.data.frame(dfx%>%filter(value!=0)),
            aes(y = value, axis1 = sectorFrom, axis2 = sectorTo, group=subRegion)) +
    geom_alluvium(aes(fill = sectorFrom), width = 1/12, color="black") +
    geom_stratum(width = 1/12, fill = "black", color = "grey", alpha=0.7) +
    geom_label(stat = "stratum", label.strata = TRUE) +
    scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05)) +
    scale_fill_brewer(type = "qual", palette = "Set1") +
    facet_grid(year~subRegion) +
    ggtitle("Title")+theme_bw();g

  if(printFig!=F){
    fname<-paste(fileName,sep="")
    if(!dir.exists(dirOutputs)){
      print(paste("dirOutputs provided: ",dirOutputs," does not exist. Saving to: ", getwd(),sep=""))
      diroutputs=getwd()}else{
        metis.printPdfPng(figure=g,
                          dir=dir,
                          filename=fname,
                          figWidth=figWidth,
                          figHeight=figHeight,
                          pdfpng=pdfpng)

        print(paste("Figure saved as: ",fileName,".",pdfpng," in folder: ", paste(dirOutputs,sep=""),sep=""))
      }}else{print("printFig set to F so no figure will be saved.")}




} # Close Function
