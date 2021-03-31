#' metis.ic
#'
#' This function calculates an interconnectivity index (ic).
#'
#' @param data Default = NULL. Matrix with rows showing from and cols to. Or dataframe with from, to and value.
#' @param icSectors Default = NULL,
#' @param icColors Default = NULL,
#' @param nameAppend Default = ""
#' @param dirOutputs Default = paste(getwd(),"/outputs",sep=""),
#' @param fname Default = "ic"
#' @param folderName Default = NULL,
#' @param saveFile Default = F. If want csv output then change to T
#' @param printFig Default = F.
#' @keywords grid, shape, polygon
#' @return dataframe with index
#' @export

metis.ic <- function(data = NULL,
                     icSectors = NULL,
                     icColors = NULL,
                     dirOutputs = paste(getwd(),"/outputs",sep=""),
                     fname = "ic",
                     nameAppend = "",
                     folderName=NULL,
                     saveFile = F,
                     printFig = F){

  # data = NULL
  # icSectors = NULL
  # icColors = NULL
  # dirOutputs = paste(getwd(),"/outputs",sep="")
  # fname = "ic"
  # nameAppend = ""
  # folderName=NULL
  # saveFile = F
  # printFig = F

print(paste("Starting metis.ic.R...",sep=""))


#................
# Initialize
#...............

  NULL -> returnData -> scenario -> year -> subRegType -> subRegion -> region -> from -> . ->
    sumTotal -> ic_sec -> x -> rankic -> rankic_sec -> rankic_spread -> value -> to -> ic_spread ->
    ic


#................
# Missing Columns
#...............

  if(T){
    addMissing<-function(data){
      if(!any(grepl("\\<scenario\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(scenario="scenario")}else{
        data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenario\\>",names(data),ignore.case = T)])[1])
        data<-data%>%dplyr::mutate(scenario=as.character(scenario),scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~scenario))}
      if(!any(grepl("\\<scenarios\\>",names(data),ignore.case = T))){}else{
        data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenarios\\>",names(data),ignore.case = T)])[1])
        data<-data%>%dplyr::mutate(scenario=as.character(scenario),scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~scenario))}
      if(!"x"%in%names(data)){if("year"%in%names(data)){
        data<-data%>%dplyr::mutate(x=year)}else{data<-data%>%dplyr::mutate(x="x")}}
      if(!any(grepl("\\<subregtype\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(subRegType="subRegType")}else{
        data <- data %>% dplyr::rename(!!"subRegType" := (names(data)[grepl("\\<subregtype\\>",names(data),ignore.case = T)])[1])
        data<-data%>%dplyr::mutate(subRegType=as.character(subRegType),subRegType=dplyr::case_when(is.na(subRegType)~"subRegType",TRUE~subRegType))}
      if(!any(grepl("\\<subregion\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(subRegion="subRegion")}else{
        data <- data %>% dplyr::rename(!!"subRegion" := (names(data)[grepl("\\<subregion\\>",names(data),ignore.case = T)])[1])
        data<-data%>%dplyr::mutate(subRegion=as.character(subRegion),subRegion=dplyr::case_when(is.na(subRegion)~"subRegion",TRUE~subRegion))}
      if(!any(grepl("\\<region\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(region="region")}else{
        data <- data %>% dplyr::rename(!!"region" := (names(data)[grepl("\\<region\\>",names(data),ignore.case = T)])[1])
        data<-data%>%dplyr::mutate(region=as.character(region),region=dplyr::case_when(is.na(region)~"region",TRUE~region))}
      if(any(unique(data$region)!="region") & all(unique(data$subRegion)=="subRegion")){data <- data %>% dplyr::mutate(subRegion=region)}
      return(data)
    }
  }

#................
# Check input data
#...............

  if(!is.null(data)){

    if(all(!class(data) %in% c("matrix","array","tbl_df","tbl","data.frame"))){

      datax <- tibble::tibble()

      for(data_i in data){
        if(file.exists(data_i)){
          dataxNew<-data.table::fread(paste(data_i),encoding="Latin-1")%>%tibble::as_tibble()
          datax<-dplyr::bind_rows(datax,dataxNew)
          rm(dataxNew)
        } else {stop(paste(data_i," does not exist"))}
      }
    }else{datax<-data}

  }else{stop("Data is NULL.")}

  # Check Colors
  if(is.null(icColors)){
   icColors <- c(metis.colors()$pal_sankey_alpha,metis.colors()$pal_Basic)
  }
  colorsSectordf <- data.frame(from=names(icColors),color=icColors)%>%dplyr::distinct();colorsSectordf

  #.........................................................
  # If matrix convert to dataframe
  #..........................................................
  if(any(class(datax) %in% c("matrix","array"))){
    if(is.null(row.names(datax))){
      stop(paste("Please name the rows of your matrix to be the 'from' sectors.",sep=""))
    }else{
    datax <- data.frame(from = row.names(datax),datax) %>%
      tidyr::gather(key="to",value="value",-from)
    }
  }

  #.........................................................
  # Check that data frame has data
  #..........................................................
  if(nrow(datax)>0){

  #................
  # Add Missing Data
  #...............

  datax <- addMissing(datax)

  for (sec_i in unique(c(unique(datax$to),unique(datax$from)))){
    datax <- datax %>%
      dplyr::filter(from!=to)%>%
      dplyr::bind_rows(datax[1,] %>%
                         dplyr::mutate(from=sec_i,to=sec_i,value=NA))%>%
      dplyr::distinct()
  }; datax

  #................
  # Create Folders
  #...............

  if(saveFile|printFig){

    if (!dir.exists(dirOutputs)){dir.create(dirOutputs)}
    if (!dir.exists(paste(dirOutputs,"/",folderName, sep = ""))){dir.create(paste(dirOutputs, "/",folderName,sep = ""))}
    dir = paste(dirOutputs,"/",folderName, "/",sep = "")

    if(!dir.exists(paste(dir,"/chordMatrix",nameAppend,sep=''))){
      dir.create(paste(dir,"/chordMatrix",nameAppend,sep=''))}
    dir = paste(dir,"/chordMatrix",nameAppend,sep = "")
  }

  #.........................................................
  # Check data frame columns
  #..........................................................

    if(!all(c("from","to","value") %in% names(datax))){
      print(paste("Columns for data provided = ", paste(colnames(datax),collapse=", "),sep=""))
      stop(paste("Data provided must contain from, to and value columns.",sep=""))
    }

  #.........................................................
  # Identify Interconnected sectors
  #..........................................................

  if(!any(icSectors %in% unique(c(datax$from,datax$to)))){
    icSectors <- unique(datax$from)[!grepl("other",unique(datax$from),ignore.case = T)]
    print(paste("icSectors set to all sectors except other = ", paste(icSectors,collapse=", "),sep=""))
    icSectors <- icSectors[icSectors %in% unique(c(datax$from,datax$to))]; icSectors
    non_icSectors <- unique(c(datax$from,datax$to))[!unique(c(datax$from,datax$to)) %in% icSectors]; non_icSectors
  }else{
      icSectors <- icSectors[icSectors %in% unique(c(datax$from,datax$to))]; icSectors
      non_icSectors <- unique(c(datax$from,datax$to))[!unique(c(datax$from,datax$to)) %in% icSectors]; non_icSectors
    }

  #.........................................................
  # Spread to data and combine non_icSectors to other
  #..........................................................
   dataxSpread <- datax %>%
     tidyr::spread(key="to",value="value") %>%
     dplyr::mutate(other=raster::rowSums(.[non_icSectors],na.rm=T));  dataxSpread

   if(length(non_icSectors[!non_icSectors %in% "other"])>0){
   dataxSpread <-  dataxSpread %>%
     dplyr::select(-c(non_icSectors[!non_icSectors %in% "other"])); dataxSpread
   }

   dataxSpread <- dataxSpread %>%
     dplyr::filter(from %in% c(icSectors)); dataxSpread

   colsTemp <- names(dataxSpread)[!names(dataxSpread) %in% c(icSectors,non_icSectors)]; colsTemp
   colsSum <- names(dataxSpread)[names(dataxSpread) %in% c(icSectors,non_icSectors)];colsSum
   datax_comb <- dataxSpread %>%
     dplyr::mutate(sumTotal = raster::rowSums(.[colsSum],na.rm=T)) %>%
     dplyr::mutate_at(colsSum,list(~(./sumTotal)))%>%
     dplyr::select(-sumTotal) %>%
     tidyr::gather(key="to",value="value",-colsTemp); datax_comb

  #.........................................................
  # Calculate Interconnectivity
  #..........................................................

   # ic is a weighted combination of sectoral interconnectivity (ic_sec) & the spread(ic_spread).
   # ic_sec is the proportion of a flows going to chosen interconnected sectors.
   # ic_spread is the value of the variation from equal distribution amongst sectors.
   # Ideal spread for 3 sectors would be 1/3 each so the value is the inverse of root mean square distance from 1/3
   # This is then normalzied to give a value from 0 to 1 where 0 is all values given to one sector and 1 is when each get a third
   # dfrm <- data.frame(x=c(1,0.9,0.8,0.8,1/3),
   #                  y=c(0.0,0.1,0.2,0.1,1/3),
   #                  z=c(0.0,0.0,0.0,0.1,1/3))
   # dfrm1<- dfrm %>%
   #   dplyr::mutate( ic_normInvRMSD=round(((2/3)-((x-1/3)^2+(y-1/3)^2+(z-1/3)^2))*3/2,4)); dfrm1

   n_sec <- icSectors %>% unique() %>% length(); n_sec # Unique sectors

   if(n_sec<3){stop("Too few integrated sectors selected")}

   # ic_sec
   datax_ic_sec <- dataxSpread %>%
     dplyr::mutate(ic_sec = raster::rowSums(.[icSectors],na.rm=T)/
                     raster::rowSums(.[c(icSectors,"other")],na.rm=T))%>%
     dplyr::select(-c(icSectors,"other")); datax_ic_sec

   # ic_spread
   datax_icSum <- dataxSpread
   for(col_i in icSectors){
      datax_icSum <- datax_icSum %>%
        dplyr::mutate(!!col_i := dplyr::case_when(from==col_i~0,
                                           TRUE~!!as.name(col_i)))}; datax_icSum
   datax_ic_spread <-datax_icSum %>%
     dplyr::mutate(sumIC = raster::rowSums(.[icSectors],na.rm=T)) %>%
     dplyr::mutate_at(icSectors,list(~((./sumIC)-1/(n_sec-1))^2)); datax_ic_spread
   for(col_i in icSectors){
     datax_ic_spread <- datax_ic_spread %>%
       dplyr::mutate(!!col_i := dplyr::case_when(from==col_i~0,
                                          TRUE~!!as.name(col_i)))}; datax_ic_spread
   datax_ic_spread <- datax_ic_spread %>%
     dplyr::mutate(ic_spread=round((((n_sec-2)/(n_sec-1))-(raster::rowSums(.[icSectors],na.rm=T)))*(n_sec-1)/(n_sec-2),4))%>%
     dplyr::mutate(ic_spread=dplyr::case_when(sumIC==0~0,TRUE~ic_spread))%>%
     dplyr::select(-c(icSectors,"other","sumIC"));datax_ic_spread

   # Combine and calculate ic
   datax_ic_all <- dataxSpread %>%
     dplyr::left_join(datax_ic_sec) %>%
     dplyr::left_join(datax_ic_spread) %>%
     dplyr::mutate(ic = (ic_sec*2 + ic_spread)/3,
                   ic = ifelse(is.nan(ic),0,ic),
                   ic_sec = ifelse(is.nan(ic_sec),0,ic_sec),
                   ic_spread = ifelse(is.nan(ic_spread),0,ic_spread),); datax_ic_all

   groupCols <- (names(datax_ic_all)[!names(datax_ic_all) %in% c(icSectors,"ic","ic_sec","ic_spread","other","from","units")]); groupCols
   datax_ic <- datax_ic_all %>%
     dplyr::group_by_at(groupCols) %>%
     dplyr::summarize_at(c("ic","ic_sec","ic_spread"),~mean(.,na.rm=T)); datax_ic

   #.........................................................
   # Rank Interconnectivity and join with ic data
   #..........................................................

   datax_ic_rank <- datax_ic %>%
     dplyr::group_by(subRegType,scenario,x)%>%
     dplyr::mutate(rankic=rank(-ic, ties.method="min"),
                   rankic_sec=rank(-ic_sec, ties.method="min"),
                   rankic_spread=rank(-ic_spread, ties.method="min")); datax_ic_rank%>%as.data.frame()

   datax_ic_all_rank <- datax_ic_all %>%
     dplyr::group_by(subRegType,scenario,x)%>%
     dplyr::mutate(rankic=rank(-ic, ties.method="min"),
                   rankic_sec=rank(-ic_sec, ties.method="min"),
                   rankic_spread=rank(-ic_spread, ties.method="min")); datax_ic_all_rank%>%as.data.frame()

   # Join back with ic data
   datax_ic <-  datax_ic %>% dplyr::left_join(datax_ic_rank)
   datax_ic_all <-  datax_ic_all %>% dplyr::left_join(datax_ic_all_rank)

  #...................
  # Save Data
  #...................

  if(saveFile==T){
  if(nrow(datax_ic)>0){
      data.table::fwrite(datax_ic,
                         paste(dir,"/",fname,nameAppend,"_ic.csv",sep = ""))
      print(gsub("//","/",paste("File saved to ",dir,"/",fname,nameAppend,"_ic.csv",sep = "")))
    }
  if(nrow(datax_ic_all)>0){
    data.table::fwrite(datax_ic_all,
                       paste(dir,"/",fname,nameAppend,"_ic_all.csv",sep = ""))
    print(gsub("//","/",paste("File saved to ",dir,"/",fname,nameAppend,"_ic_all.csv",sep = "")))
  }
  }

  #...................
  # Print Figures
  #...................


   if(printFig==T){

    df <- datax_ic

   for(subRegion_i in unique(df$subRegion)){
     for(x_i in unique(df$x)){
       for(scenario_i in unique(df$scenario)){

         rank_i <- (df %>%
                      dplyr::filter(subRegion==subRegion_i,x==x_i,scenario==scenario_i) %>%
                      dplyr::ungroup()%>%
                      dplyr::select(rankic,rankic_sec,rankic_spread))[1,]; rank_i

         values_ic <- (df %>%
                         dplyr::filter(subRegion==subRegion_i,x==x_i,scenario==scenario_i) %>%
                         dplyr::ungroup()%>%
                         dplyr::select(ic,ic_sec,ic_spread))[1,]; values_ic

         rank_print <- paste(
           "ranks: ic = ",rank_i$rankic,
           ", sec = ",rank_i$rankic_sec,
           ", spread= ",rank_i$rankic_spread,sep="");rank_print

         rank_print_file <- paste(
           "ic",rank_i$rankic,
           "sec",rank_i$rankic_sec,
           "spread",rank_i$rankic_spread,sep="");rank_print_file

         df_i <- datax_comb %>%
           dplyr::filter(subRegion==subRegion_i,
                         x==x_i,
                         scenario==scenario_i);df_i

         subRegType_i= unique(df_i$subRegType)

         for (sec_i in unique(c(unique(df$to),unique(df$from)))){
           df_i <- df_i %>%
             dplyr::bind_rows(df_i[1,] %>%
                                dplyr::mutate(from=sec_i,to=sec_i,value=NA))%>%
             dplyr::distinct()
         }; df_i

         mat1 <- df_i %>%
           dplyr::bind_rows(df_i[1,] %>%
                              dplyr::mutate(from="other",to="other",value=1))%>%
           tidyr::spread(key="to",value="value"); mat1
         mat <- mat1 %>% dplyr::select(c(icSectors,"other"))%>%as.matrix(); mat
         row.names(mat) <- mat1$from; mat
         for(row_i in 1:nrow(mat)){
           for(col_i in 1:ncol(mat)){
            if(row.names(mat)[row_i] == colnames(mat)[col_i]){mat[row_i,col_i]=1}
           }
         }; mat

         # Check if sectors with color names exist
         if(!all(unique(df_i$from) %in% unique(colorsSectordf$from))){
            colorsSectordf <- data.frame(from=unique(c(df_i$from,df_i$to)),colors=unique(colorsSectordf$color)[1:length(unique(c(df_i$from,df_i$to)))])
         }

         colorsSectordf <- colorsSectordf %>% dplyr::filter(from %in% unique(c(df_i$from,df_i$to)));colorsSectordf
         colorsSector <- colorsSectordf$color
         names(colorsSector) <- colorsSectordf$from; colorsSector

         mat1col <- df_i %>%
           dplyr::bind_rows(df_i[1,] %>%
                              dplyr::mutate(from="other",to="other",value=1))%>%
           dplyr::select(-value)%>%
           dplyr::left_join(colorsSectordf,by="from")%>%
           tidyr::spread(key="to",value="color"); mat1col
         matcol <- mat1col  %>% dplyr::select(c(icSectors,"other"))%>%as.matrix(); matcol
         row.names(matcol) <- mat1col$from; matcol
         for(row_i in 1:nrow(matcol)){
           for(col_i in 1:ncol(matcol)){
             if(row.names(matcol)[row_i] == colnames(matcol)[col_i]){matcol[row_i,col_i]="#FFFFFF00"}
           }
         }; matcol

         icOrder <- c(icSectors,"other")
         icOrder <- icOrder[icOrder %in% unique(c(datax_comb$from,datax_comb$to))]

         grDevices::png(paste(dir,"/",subRegType_i,subRegion_i,scenario_i,x_i,"_",rank_print_file,"_chord.png",sep=""),
             res=300,width=4,height=4,units="in")
         graphics::par(mar=c(0,0,3,0))
         circlize::chordDiagram(mat, grid.col=colorsSector,col=matcol,
                      scale=T,
                      reduce=-0.01,
                      directional=1,
                      order=icOrder,
                      transparency = 0.25,
                      direction.type = c("arrows","diffHeight"), diffHeight  = -0.04,
                      link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE,
                      annotationTrack = c("grid","name"))
         graphics::title(main=paste(subRegType_i," ",subRegion_i," ",scenario_i," ",x_i,"\n",
                          rank_print,"\n",
                          "values: ic = ",round(values_ic$ic,2),
                          ", ic_sec = ",round(values_ic$ic_sec,2),
                          ", ic_spread = ",round(values_ic$ic_spread,2),sep=""),
               cex.main=0.6, outer=F)
         grDevices::dev.off()

         print(paste("File saved as:",sep=""))
         print(gsub("//","/",paste(dir,"/",subRegType_i,subRegion_i,scenario_i,x_i,"_",rank_print_file,"_chord.png",sep="")))

         sectorToOrder = c(icSectors,unique(df_i$to)[!unique(df_i$to) %in% icSectors])
         sectorFromOrder = c(icSectors,unique(df_i$from)[!unique(df_i$from) %in% icSectors])
         sectorToOrder
         sectorFromOrder

         df_i[df_i==0]<-NA; df_i

         p <- ggplot(df_i,aes(x=to,y=from)) + #theme_bw() +
           #labs(title="title") +
           geom_point(data=df_i%>%dplyr::filter(from!="other"),aes(col=from, size=value, alpha=value)) +
           scale_color_manual(values=colorsSector, guide="none") +
           scale_alpha(guide="none") +
           geom_text(aes(label=round(value,2)),col="black", size = 4) +
           coord_fixed(ratio = 1) + xlab(NULL) + ylab(NULL) +
           scale_x_discrete(limits = sectorToOrder,position="top") +
           scale_y_discrete(limits = rev(sectorFromOrder[sectorFromOrder != "other"]), expand = c(0.1,0.1)) +
           scale_size_continuous(range = c(1,15), guide="none")+
           ggtitle(paste(subRegType_i,"_",subRegion_i,"_",scenario_i,"_",x_i,"\n",
                         rank_print,"\n",
                         "values: ic = ",round(values_ic$ic,2),
                         ", ic_sec = ",round(values_ic$ic_sec,2),
                         ", ic_spread = ",round(values_ic$ic_spread,2),sep=""))+
           theme(plot.title = element_text(size = 8, face="bold"),
                 axis.text.x= element_text(size=12),
                 axis.text.y= element_text(size=12));p

         metis.printPdfPng(figure=p,
                           dir=dir,
                           filename=paste("/",subRegType_i,subRegion_i,scenario_i,x_i,"_",rank_print_file,"_matrix",sep=""),
                           figWidth=5,
                           figHeight=5,
                           pdfpng="png",
                           transparent=T)

         print(paste("File saved as:",sep=""))
         print(gsub("//","/",paste(dir,"/",subRegType_i,subRegion_i,scenario_i,x_i,"_",rank_print_file,"_matrix.png",sep="")))


       }
     }
   }
   }

 #...................
 # Conclude function
 #...................

  print(paste("metis.ic.R finished running succesfully.",sep=""))

returnData <- list(ic=datax_ic, ic_mean=datax_ic_all)

  }

invisible(returnData)

}
