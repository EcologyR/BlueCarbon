#' Estimate the average organic carbon flux
#'
#' @description estimate the average organic carbon flux to the soil in a indicated time frame (by default last 100 years) from the organic carbon concentration and ages obtained from a age-depth or age-accumulated mass model
#'
#' @param df A [data.frame] with, at least, columns: Core.ID, DMin (minimum depth of the sample), DMax (maximum depth of the sample), DBD (dry bulk density), POC (organic carbon %), Age (age of the sample obtained from a age-depth or age-accumulated mass model)
#' @param TimeFrame standardization time frame, by default 100 years
#'
#' @return [data.frame] with columns Core.id, F.WC (organic carbon fluxes at the whole core), A.Max (maximum age of the core), and Flux (average organic carbon flux at the indicated time frame)
#' @export
#'
#' @examples


estimate_flux<- function(df=NULL,TimeFrame=100) {

  # class of the dataframe
  if (is.data.frame(df)==FALSE) {stop("The data provided is not class data.frame, please check data and transforme")}
  if (is.numeric(TimeFrame)==FALSE) {stop("The TimeFrame provided is not class numeric, please check data and transforme")}

  # name of the columns
  if ("CoreID" %in% colnames(df)==FALSE) {stop("There is not column named CoreID. Please, check necessary columns in functions documentation")}
  if ("DMin" %in% colnames(df)==FALSE) {stop("There is not column named Min.D. Please, check necessary columns in functions documentation")}
  if ("DMax" %in% colnames(df)==FALSE) {stop("There is not column named Max.D. Please, check necessary columns in functions documentation")}
  if ("DBD" %in% colnames(df)==FALSE) {stop("There is not column named DBD. Please, check necessary columns in functions documentation")}
  if ("fOC" %in% colnames(df)==FALSE) {stop("There is not column named fOC. Please, check necessary columns in functions documentation")}
  if ("Age" %in% colnames(df)==FALSE) {stop("There is not column named Age. Please, check necessary columns in functions documentation")}

  # class of the columns
  if (is.numeric(df$DMin)==FALSE) {stop("Minimum depth data is not class numeric, please check")}
  if (is.numeric(df$DMax)==FALSE) {stop("Maximum depth data is not class numeric, please check")}
  if (is.numeric(df$DBD)==FALSE) {stop("Dry Bulk Density data is not class numeric, please check")}
  if (is.numeric(df$fOC)==FALSE) {stop("Organic carbon data is not class numeric, please check")}
  if (is.numeric(df$Age)==FALSE) {stop("Age data is not class numeric, please check")}



  #select those cores with chronological models
  df<-df[!is.na(df$Age),]
  df<-df[!is.na(df$fOC),]

  df<-estimate_h (df)

  X<-split(df, df$CoreID)

  BCF <- data.frame(CoreID=character(),
                    F.WC=numeric(),
                    A.Max=numeric(),
                    Flux=numeric())


  for(i in 1:length(X)) {
    BCF[i,1]<-names(X[i])
    Data<-as.data.frame(X[i])
    colnames(Data)<-colnames(df)

    if(nrow(Data)<3) next

    else{

      Data <-Data |> dplyr::mutate (OCgcm2 = DBD*(fOC/100)*h)

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
