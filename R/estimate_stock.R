#' Organic Carbon Stock estimation
#'
#' @description Estimates the carbon stock from soil core data until a specific depth, 100 cm by default. If the core do not reach the
#' standardization depth it extrapolate the stock from a linear model between the organic carbon accumulated mass and depth.
#'
#' @param df A data frame with columns: Core.ID, Min.D (minimum depth of the sample), Max.D (maximum depth of the sample), DBD (dry bulk density), POC (organic carbon %)
#' @param Depth standardization soil depth, by default 100 cm.
#'
#' @return data frame with columns Core.id, S.WC (organic carbon stock at the whole core), D.Max (maximum depth of the core), and Stock (organic carbon stock at the standardized depth)
#' @export
#'
#' @examples

estimate_stock <- function(df = NULL, Depth = 100) {

  df <-df %>% mutate (Center = Min.D+((Max.D-Min.D)/2))

  df = filter(df, !is.na(POC))

  X<-split(df, df$Core.ID)

  BCS <- data.frame(Core.ID=character(),
                    S.WC=numeric(),
                    D.Max=numeric(),
                    Stock=numeric())

  for(i in 1:length(X)) {
    BCS[i,1]<-names(X[i])
    Data<-as.data.frame(X[i])
    colnames(Data)<-colnames(df)



    if(nrow(Data)<3) next

    else{

      Data$h<-NA

      #estimation of the thickness of the sample (h)
      for (j in 2:(nrow(Data)-1)) {

        Data[j,which(colnames(Data)=="h")]<-
          ((Data[j,which(colnames(Data)=="Center")]-Data[(j-1),which(colnames(Data)=="Center")])/2)+
          ((Data[(j+1),which(colnames(Data)=="Center")]-Data[j,which(colnames(Data)=="Center")])/2)

      }

      Data[1,which(colnames(Data)=="h")]<-Data[1,which(colnames(Data)=="Center")]
      Data[nrow(Data),which(colnames(Data)=="h")]<-
        Data[nrow(Data),which(colnames(Data)=="Max.D")]-Data[nrow(Data),which(colnames(Data)=="Center")]

      #estimation of carbon g cm2 per sample, OCgcm2= carbon density (g cm3) by thickness (h)
      Data <-Data %>% mutate (OCgcm2 = DBD*(POC/100)*h)

      #estimation of the OC stoack in the whole core
      BCS[i,2]<-sum(Data[,which(colnames(Data)=="OCgcm2")])
      BCS[i,3]<-max(Data[,which(colnames(Data)=="Max.D")])

      #if core exactly the standarization depth, we keep the stock of the whole core
      if(max(Data$Max.D)==Depth) {BCS[i,4]<-sum(Data[,which(colnames(Data)=="OCgcm2")])}


      else{

        # if the core longer than the standarization depth we estimate the stock until 1m depth
        if (max(Data$Max.D)>=Depth)

        {
          Data<-Data[c(1:(length(which(Data$Max.D <=Depth))+1)),]

          BCS[i,4]<-(((sum(Data[c(1:(nrow(Data)-1)),which(colnames(Data)=="OCgcm2")]))+
                        (Data[nrow(Data),which(colnames(Data)=="OCgcm2")]/((max(Data$Max.D)-Data[(nrow(Data)-1),which(colnames(Data)=="Max.D")]))
                         *(Depth-Data[(nrow(Data)-1),which(colnames(Data)=="Max.D")]))))}

        #if core shorter than than the standarization depth we model the OC acumulated mass with depth and predict the stock at 1m
        else {

          Data <-Data %>% mutate (OCM = cumsum(OCgcm2))
          model<-lm(OCM ~ Max.D, data=Data)
          BCS[i,4]<-coef(model)[1] + Depth*coef(model)[2]}}
    }}
  return(BCS)
}
