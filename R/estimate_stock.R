#' Organic Carbon Stock estimation
#'
#' @description Estimates the carbon stock from soil core data until a specific depth, 100 cm by default. If the core do not reach the
#' standardization depth it extrapolate the stock from a linear model between the organic carbon accumulated mass and depth.
#'
#' @param df A [data.frame] with, at least, columns CoreID, DMin (minimum depth of the sample), DMax (maximum depth of the sample), DBD (dry bulk density), fOC (organic carbon %)
#' @param Depth standardization soil depth, by default 100 cm.
#' @param min.sample
#' @param core
#' @param dmin
#' @param dmax
#' @param dbd
#' @param oc
#'
#'
#' @return [data.frame] with columns CoreID, S.WC (organic carbon stock at the whole core), D.Max (maximum depth of the core), and Stock (organic carbon stock at the standardized depth)
#' @export
#'
#' @examples estimate_stock(A)
#' @examples estimate_stock(A, Depth = 50)

estimate_stock <- function(df = NULL,
                           Depth = 100,
                           min_samples = 3,
                           core = "core",
                           dmin = "dmin",
                           dmax = "dmax",
                           dbd = "dbd",
                           oc = "eoc") {

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

  # class of the columns
  if (!is.numeric(df[[dmin]])) {stop("Minimum depth data is not class numeric, please check")}
  if (!is.numeric(df[[dmax]])) {stop("Maximum depth data is not class numeric, please check")}
  if (!is.numeric(df[[dbd]])) {stop("Dry Bulk Density data is not class numeric, please check")}
  if (!is.numeric(df[[oc]])) {stop("Organic carbon data is not class numeric, please check")}

  # create variables with working names with the data in the columns specified by the user
  df_r <- df
  df_r$core_r <- df_r[[core]]
  df_r$dmin_r <- df_r[[dmin]]
  df_r$dmax_r <- df_r[[dmax]]
  df_r$dbd_r <- df_r[[dbd]]
  df_r$oc_r <- df_r[[oc]]


  df_r<-df_r[!is.na(df_r$oc_r),]

  # estimate thickness of the sample

  df_h<-estimate_h(df_r)

  #for(i in 1:length(X)) {
    #BCS[i,1]<-names(X[i])
    #Data<-as.data.frame(X[i])
    #colnames(Data)<-colnames(dfh)


    estimate_core <- function (df, Depth, min_samples = 3) {

      #BCS <- data.frame(CoreID=character(),
      #                  S.WC=numeric(),
      #                  D.Max=numeric(),
      #                  Stock=numeric(),
      #                  mStock_se=numeric())


      CoreID <- as.character(unique(df[1,"core_r"]))
      df<-as.data.frame(df)
      colnames(df)<-colnames(df_h)


    if(!nrow(df)<min_samples) {

      #estimation of carbon g cm2 per sample, OCgcm2= carbon density (g cm3) by thickness (h)
      df <-df |> dplyr::mutate (ocgcm2 = dbd_r*(oc_r/100)*h)

      #estimation of the OC stock in the whole core
      S.WC<-sum(df$ocgcm2)
      D.Max<-max(df$emax)

      #if core exactly the standarization depth, we keep the stock of the whole core
      if(max(df$emax)==Depth) {Stock<-sum(df$ocgcm2)
        mStock_se<-NA }

      else{

        # if the core longer than the standardization depth we estimate the stock until that depth
        if (max(df$emax)>=Depth)

        {
          df<-df[c(1:(length(which(df$emax <=Depth))+1)),]

          if(!nrow(df)<min_samples) {

            Stock<-(sum(df[c(1:(nrow(df)-1)),"ocgcm2"]))+
                        ((df[nrow(df),"ocgcm2"] / (max(df$emax)-df[(nrow(df)-1),"emax"]))
                         *(Depth-df[(nrow(df)-1),"emax"]))
            mStock_se<-NA
            }}

        #if core shorter than than the standardization depth we model the OC acumulated mass with depth and predict the stock at that depth
        else {

          df <-df |> dplyr::mutate (ocM = cumsum(ocgcm2))
          model<-lm(ocM ~ emax, df)
          Stock<-predict(model, newdata = data.frame(emax=Depth))
          mStock_se <- predict(model, newdata = data.frame(emax=Depth), se.fit = TRUE)$se.fit

    }}}

      BCS<-data.frame(CoreID = CoreID, S.WC = S.WC, D.Max = D.Max, Stock = Stock, mStock_se = mStock_se)

      return(BCS)
}

    # estimate stocks

    x<-split(df_h, df_h$core_r)

    BCS_l<- lapply( X = x,  estimate_core, Depth = Depth) # return a list
    BCS <- as.data.frame(do.call(rbind, BCS_l)) # from list to dataframe

    return(BCS)
}




estimate_stock (df = df_f,
                Depth = 100,
                min_samples = 3,
                core = "core",
                dmin = "mind",
                dmax = "maxd",
                dbd = "dbd",
                oc = "eoc")



df = df_f
Depth = 100
min_samples = 3
core = "core"
dmin = "mind"
dmax = "maxd"
dbd = "dbd"
oc = "eoc"

estimate_stock(df)

