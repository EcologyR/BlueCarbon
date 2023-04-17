#' Organic Carbon Stock estimation
#'
#' @description Estimates the carbon stock from soil core data until a specific depth, 100 cm by default. If the core do not reach the
#' standardization depth it extrapolate the stock from a linear model between the organic carbon accumulated mass and depth.
#'
#' @param df A [data.frame] with, at least, columns: Core.ID, Min.D (minimum depth of the sample), Max.D (maximum depth of the sample), DBD (dry bulk density), POC (organic carbon %)
#' @param Depth standardization soil depth, by default 100 cm.
#' @param Core.ID the name of the column (between "") from the df with the Core identification for each sample
#' @param DMin the name of the column (between "") from the df with the minimum depth of that sample (already corrected if needed)
#' @param DMax the name of the column (between "") from the df with the maximum depth of that sample (already corrected if needed)
#' @param DBD the name of the column (between "") from the df with the dry bulk density of that sample (already corrected if needed)
#' @param POC the name of the column (between "") from the df with the percentage of organic carbon in the sample
#'
#' @return [data.frame] with columns Core.id, S.WC (organic carbon stock at the whole core), D.Max (maximum depth of the core), and Stock (organic carbon stock at the standardized depth)
#' @export
#'
#' @examples

estimate_stock <- function(df = NULL, Depth = 100,  Core.ID="Core.ID", DMin= "DMin", DMax="DMax", DBD= "DBD", POC="POC") {

  # class of the dataframe
  if (is.data.frame(df)==FALSE) {stop("The data provided is not class data.frame, please chaeck data and transforme")}
  if (is.numeric(Depth)==FALSE) {stop("The Depth provided is not class numeric, please chaeck data and transforme")}

  df2<-as.data.frame(cbind(df[[Core.ID]], df[[DMin]],df[[DMax]],df[[DBD]], df[[POC]]))
  colnames(df2)<-c("Core.ID","DMin","DMax","DBD","POC")
  df2[, 2:5] <- sapply(df2[, 2:5], as.numeric)


  df3<-estimate_h (df2, Core.ID=Core.ID, DMin= "DMin", DMax="DMax")

  df3 = filter(df3, !is.na(POC))

  X<-split(df3, df3$Core.ID)

  BCS <- data.frame(Core.ID=character(),
                    S.WC=numeric(),
                    D.Max=numeric(),
                    Stock=numeric())

  for(i in 1:length(X)) {
    BCS[i,1]<-names(X[i])
    Data<-as.data.frame(X[i])
    colnames(Data)<-colnames(df3)

    if(nrow(Data)<3) next

    else{

      #estimation of carbon g cm2 per sample, OCgcm2= carbon density (g cm3) by thickness (h)
      Data <-Data |> dplyr::mutate (OCgcm2 = DBD*(POC/100)*h)

      #estimation of the OC stoack in the whole core
      BCS[i,2]<-sum(Data[,which(colnames(Data)=="OCgcm2")])
      BCS[i,3]<-max(Data[,which(colnames(Data)=="EMax")])

      #if core exactly the standarization depth, we keep the stock of the whole core
      if(max(Data$EMax)==Depth) {BCS[i,4]<-sum(Data[,which(colnames(Data)=="OCgcm2")])}


      else{

        # if the core longer than the standarization depth we estimate the stock until 1m depth
        if (max(Data$EMax)>=Depth)

        {
          Data<-Data[c(1:(length(which(Data$EMax <=Depth))+1)),]

          BCS[i,4]<-(((sum(Data[c(1:(nrow(Data)-1)),which(colnames(Data)=="OCgcm2")]))+
                        (Data[nrow(Data),which(colnames(Data)=="OCgcm2")]/((max(Data$EMax)-Data[(nrow(Data)-1),which(colnames(Data)=="EMax")]))
                         *(Depth-Data[(nrow(Data)-1),which(colnames(Data)=="EMax")]))))}

        #if core shorter than than the standarization depth we model the OC acumulated mass with depth and predict the stock at 1m
        else {

          Data <-Data |> dplyr::mutate (OCM = cumsum(OCgcm2))
          model<-lm(OCM ~ EMax, data=Data)
          BCS[i,4]<-coef(model)[1] + Depth*coef(model)[2]}}
    }}
  return(BCS)
}
