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
#' @examples estimate_flux(A)
#' @examples estimate_flux(A, ,TimeFrame = 500)


estimate_flux<- function(df=NULL,
                         TimeFrame=100,
                         core = "core",
                         dmin = "dmin",
                         dmax = "dmax",
                         dbd = "dbd",
                         oc = "eoc",
                         age = "age") {

  # class of the dataframe or tibble
  if (!inherits(df, "data.frame")) {
    stop("The data provided must be a tibble or data.frame")
  }
  if (!is.numeric(Depth)) {stop("The Depth provided is not class numeric, please chaeck data and transforme")}

  # name of the columns
  if (!core %in% colnames(df)) {stop("There must be a variable with 'core'")}
  if (!dmin %in% colnames(df)) {stop("There must be a variable with 'dmin'")}
  if (!dmax %in% colnames(df)) {stop("There must be a variable with 'dmax'")}
  if (!dbd %in% colnames(df)) {stop("There must be a variable with 'dbd'")}
  if (!oc %in% colnames(df)) {stop("There must be a variable with 'oc'")}
  if (!age %in% colnames(df)) {stop("There must be a variable with 'age'")}

  # class of the columns
  if (!is.numeric(df[[dmin]])) {stop("Minimum depth data is not class numeric, please check")}
  if (!is.numeric(df[[dmax]])) {stop("Maximum depth data is not class numeric, please check")}
  if (!is.numeric(df[[dbd]])) {stop("Dry Bulk Density data is not class numeric, please check")}
  if (!is.numeric(df[[oc]])) {stop("Organic carbon data is not class numeric, please check")}
  if (!is.numeric(df[[age]])) {stop("Age data is not class numeric, please check")}

  # create variables with working names with the data in the columns specified by the user
  df_r <- df
  df_r$core_r <- df_r[[core]]
  df_r$dmin_r <- df_r[[dmin]]
  df_r$dmax_r <- df_r[[dmax]]
  df_r$dbd_r <- df_r[[dbd]]
  df_r$oc_r <- df_r[[oc]]
  df_r$age_r <- df_r[[age]]


  #select those cores with chronological models
  df_r<-df_r[!is.na(df_r$Age_r),]
  df_r<-df_r[!is.na(df_r$oc_r),]

  df_h<-estimate_h (df_r)

  x<-split(df_h, df_h$core_r)

  #BCF <- data.frame(CoreID=character(),
   #                 F.WC=numeric(),
    #                A.Max=numeric(),
     #               Flux=numeric())


  for(i in 1:length(X)) {
    BCF[i,1]<-names(X[i])
    Data<-as.data.frame(X[i])
    colnames(Data)<-colnames(df)

    estimate_core_f<- function (df, timeframe) {

      CoreID <- as.character(unique(df[1,"core_r"]))
      df<-as.data.frame(df)
      colnames(df)<-colnames(df_h)







      }





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
