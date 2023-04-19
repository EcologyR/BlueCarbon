#' Organic Carbon Stock estimation
#'
#' @description Estimates the carbon stock from soil core data until a specific depth, 100 cm by default. If the core do not reach the
#' standardization depth it extrapolate the stock from a linear model between the organic carbon accumulated mass and depth.
#'
#' @param df A [data.frame] with, at least, columns CoreID, DMin (minimum depth of the sample), DMax (maximum depth of the sample), DBD (dry bulk density), fOC (organic carbon %)
#' @param Depth standardization soil depth, by default 100 cm.
#'
#' @return [data.frame] with columns CoreID, S.WC (organic carbon stock at the whole core), D.Max (maximum depth of the core), and Stock (organic carbon stock at the standardized depth)
#' @export
#'
#' @examples

estimate_stock <- function(df = NULL, Depth = 100) {

  # class of the dataframe
  if (is.data.frame(df)==FALSE) {stop("The data provided is not class data.frame, please chaeck data and transforme")}
  if (is.numeric(Depth)==FALSE) {stop("The Depth provided is not class numeric, please chaeck data and transforme")}

  # name of the columns
  if ("CoreID" %in% colnames(df)==FALSE) {stop("There is not column named CoreID. Please, check necessary columns in functions documentation")}
  if ("DMin" %in% colnames(df)==FALSE) {stop("There is not column named Min.D. Please, check necessary columns in functions documentation")}
  if ("DMax" %in% colnames(df)==FALSE) {stop("There is not column named Max.D. Please, check necessary columns in functions documentation")}
  if ("DBD" %in% colnames(df)==FALSE) {stop("There is not column named DBD. Please, check necessary columns in functions documentation")}
  if ("fOC" %in% colnames(df)==FALSE) {stop("There is not column named fOC. Please, check necessary columns in functions documentation")}

  # class of the columns
  if (is.numeric(df$DMin)==FALSE) {stop("Minimum depth data is not class numeric, please check")}
  if (is.numeric(df$DMax)==FALSE) {stop("Maximum depth data is not class numeric, please check")}
  if (is.numeric(df$DBD)==FALSE) {stop("Dry Bulk Density data is not class numeric, please check")}
  if (is.numeric(df$fOC)==FALSE) {stop("Organic carbon data is not class numeric, please check")}



  df<-df[!is.na(df$fOC),]

  # estimate thickness of the sample

  dfh<-estimate_h(df)

  # estimate stocks

  X<-split(dfh, dfh$CoreID)

  BCS <- data.frame(CoreID=character(),
                    S.WC=numeric(),
                    D.Max=numeric(),
                    Stock=numeric())

  for(i in 1:length(X)) {
    BCS[i,1]<-names(X[i])
    Data<-as.data.frame(X[i])
    colnames(Data)<-colnames(dfh)

    if(nrow(Data)<3) next

    else{

      #estimation of carbon g cm2 per sample, OCgcm2= carbon density (g cm3) by thickness (h)
      Data <-Data |> dplyr::mutate (OCgcm2 = DBD*(fOC/100)*h)

      #estimation of the OC stock in the whole core
      BCS[i,2]<-sum(Data[,which(colnames(Data)=="OCgcm2")])
      BCS[i,3]<-max(Data[,which(colnames(Data)=="EMax")])

      #if core exactly the standarization depth, we keep the stock of the whole core
      if(max(Data$EMax)==Depth) {BCS[i,4]<-sum(Data[,which(colnames(Data)=="OCgcm2")])}


      else{

        # if the core longer than the standardization depth we estimate the stock until that depth
        if (max(Data$EMax)>=Depth)

        {
          Data<-Data[c(1:(length(which(Data$EMax <=Depth))+1)),]

          if(nrow(Data)<3) next

          else{

          BCS[i,4]<-(((sum(Data[c(1:(nrow(Data)-1)),which(colnames(Data)=="OCgcm2")]))+
                        (Data[nrow(Data),which(colnames(Data)=="OCgcm2")]/((max(Data$EMax)-Data[(nrow(Data)-1),which(colnames(Data)=="EMax")]))
                         *(Depth-Data[(nrow(Data)-1),which(colnames(Data)=="EMax")]))))}}

        #if core shorter than than the standardization depth we model the OC acumulated mass with depth and predict the stock at that depth
        else {

          Data <-Data |> dplyr::mutate (OCM = cumsum(OCgcm2))
          model<-lm(OCM ~ EMax, data=Data)
          BCS[i,4]<-coef(model)[1] + Depth*coef(model)[2]}}
    }}
  return(BCS)
}
