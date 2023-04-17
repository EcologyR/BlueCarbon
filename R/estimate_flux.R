#' Estimate the average organic carbon flux
#'
#' @description estimate the average organic carbon flux to the soil in a indicated time frame (by default last 100 years) from the organic carbon concentration and ages obtained from a age-depth or age-accumulated mass model
#'
#' @param df A [data.frame] with, at least, columns: Core.ID, Min.D (minimum depth of the sample), Max.D (maximum depth of the sample), DBD (dry bulk density), POC (organic carbon %), Age (age of the sample obtained from a age-depth or age-accumulated mass model)
#' @param TimeFrame standardization time frame, by default 100 years
#' @param Core.ID the name of the column (between "") from the df with the Core identification for each sample
#' @param DMin the name of the column (between "") from the df with the minimum depth of that sample (already corrected if needed)
#' @param DMax the name of the column (between "") from the df with the maximum depth of that sample (already corrected if needed)
#' @param DBD the name of the column (between "") from the df with the dry bulk density of that sample (already corrected if needed)
#' @param POC the name of the column (between "") from the df with the percentage of organic carbon in the sample
#' @param Age the name of the column (between "") from the df with the percentage of organic carbon in the sample
#'
#' @return [data.frame] with columns Core.id, F.WC (organic carbon fluxes at the whole core), A.Max (maximum age of the core), and Flux (average organic carbon flux at the indicated time frame)
#' @export
#'
#' @examples


estimate_flux<- function(df=NULL,TimeFrame=100, CoreID="CoreID", DMin= "DMin", DMax="DMax", DBD= "DBD", POC="POC", Age="Age") {

  # class of the dataframe
  if (is.data.frame(df)==FALSE) {stop("The data provided is not class data.frame, please chaeck data and transforme")}
  if (is.numeric(TimeFrame)==FALSE) {stop("The TimeFrame provided is not class numeric, please chaeck data and transforme")}


  df2<-as.data.frame(cbind(df[[CoreID]], df[[DMin]],df[[DMax]],df[[DBD]], df[[POC]], df[[Age]]))
  colnames(df2)<-c("CoreID","DMin","DMax","DBD","POC", "Age")
  df2[, 2:6] <- sapply(df2[, 2:6], as.numeric)

  #select those cores with chronological models
  df3 = filter(df2, !is.na(Age))
  df4 = filter(df3, !is.na(POC))

  df4<-estimate_h (df4, CoreID=CoreID, DMin= "DMin", DMax="DMax")

  X<-split(df4, df4$CoreID)

  BCF <- data.frame(CoreID=character(),
                    F.WC=numeric(),
                    A.Max=numeric(),
                    Flux=numeric())


  for(i in 1:length(X)) {
    BCF[i,1]<-names(X[i])
    Data<-as.data.frame(X[i])
    colnames(Data)<-colnames(df4)

    if(nrow(Data)<3) next

    else{

      Data <-Data |> dplyr::mutate (OCgcm2 = DBD*(POC/100)*h)

      #estimation of the average carbon flux for the whole core (OC stock/Max Age)
      BCF[i,2]<-(sum(Data[,which(colnames(Data)=="OCgcm2")]))/max(Data$Age)
      BCF[i,3]<-max(Data$Age)

      #estimation of the average carbon flux for the selected TimeFrame (OC stock last 100 yrs/TimeFrame)

      if (max(Data$Age)==TimeFrame) {

        BCF[i,4]<-((sum(Data[c(1:(nrow(Data))),which(colnames(Data)=="OCgcm2")]))/TimeFrame)

      } else {
        Data<-Data[c(1:(length(which(Data$Age <=TimeFrame))+1)),]

        BCF[i,4]<-(
          (sum(Data[c(1:(nrow(Data)-1)),which(colnames(Data)=="OCgcm2")])+
                       (Data[nrow(Data),which(colnames(Data)=="OCgcm2")]/((max(Data$Age)-Data[(nrow(Data)-1),which(colnames(Data)=="Age")])))
                        *(TimeFrame-Data[(nrow(Data)-1),which(colnames(Data)=="Age")]))/TimeFrame)
      }

    }}
  return(BCF)
}
