#' Estimation of the thickness of the sample
#'
#' @description checks for space between samples and, if any, divide this space between the previous and next sample to return sample thickness withouth gaps in the core
#'
#' @param df A [data.frame] with with, at least, columns CoreID, DMin (minimum depth of the sample)and DMax (maximum depth of the sample)
#' @param core
#' @param dmin
#' @param dmax
#'
#' @return the initial [data.frame] with three additional columns: EMin (estimated minimum depth of the sample), EMax (estimated maximum depth of the sample) and h (estimated thikness of the sample)
#' @export
#'
#' @examples estimate_h(A)

estimate_h <- function(df = NULL,
                       core = "core",
                       dmin= "dmin",
                       dmax= "dmax") {

  # class of the dataframe or tibble
  if (!inherits(df, "data.frame")) {
    stop("The data provided must be a tibble or data.frame")
  }

  # name of the columns
  if (!core %in% colnames(df)) {stop("There must be a variable with 'core'")}
  if (!dmin %in% colnames(df)) {stop("There must be a variable with 'dmin'")}
  if (!dmax %in% colnames(df)) {stop("There must be a variable with 'dmax'")}

  # class of the columns
  if (!is.numeric(df[[dmin]])) {stop("dmin data must be class numeric")}
  if (!is.numeric(df[[dmax]])) {stop("dmax data must be class numeric")}

  #check for NAs in depth columns
  if (sum(is.na(df[[dmin]]))>0){stop("Samples minimun depth column has NAs, please check")}
  if (sum(is.na(df[[dmin]]))>0){stop("Samples maximun depth column has NAs, please check")}


  # create variables with working names with the data in the columns specified by the user
  df_r <- df
  df_r$core_r <- df_r[[core]]
  df_r$dmin_r <- df_r[[dmin]]
  df_r$dmax_r <- df_r[[dmax]]


  # create individual data frames per each core

  df_r$core_r <- factor(df_r$core_r, levels=unique(df_r$core_r))
  x<-split(df_r, df_r$core_r)


  columns<-c("EMin","EMax","h")
  Fdf2 = data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(Fdf2) = columns


  for(i in 1:length(x)) {

    Data<-as.data.frame(x[i])
    colnames(Data)<-colnames(df_r)

    #check if there is spaces between samples (e.g, first sample ends at 5 cm and next starts at 7)
    space<- c()

    for (j in 1:(nrow(Data)-1)) {

      # if there are no spaces between samples min and maximun depth of samples remain the same
      if (Data[j,which(colnames(Data)=="DMax")] == Data[j+1,which(colnames(Data)=="DMin")]) {
        space[j]<-FALSE} else {space[j]<-TRUE}}

    if (any(space==TRUE)) {
      # if there are spaces between samples it estimate the medium point between the maximum depth of the sample and the minimun depth of the next sample
      # and divide that distance between both samples
      Data <- cbind(Data, EMin=NA, EMax=NA)
      Data[1,"EMin"]<-0
      Data[nrow(Data),"EMax"]<-Data[nrow(Data),"DMax"]

      for (j in 1:(nrow(Data)-1)) {
        if(space[j]==TRUE) {
          Data[j,"EMax"]<-Data[j,"DMax"]+((Data[j+1,"DMin"]-Data[j,"DMax"])/2)
          Data[j+1,"EMin"]<-Data[j,"DMax"]+((Data[j+1,"DMin"]-Data[j,"DMax"])/2)} else {
            Data[j,"EMax"]<-Data[j,"DMax"]
            Data[j+1,"EMin"]<-Data[j+1,"DMin"]}}

    }  else{
      Data <- cbind(Data, EMin=NA, EMax=NA)
      Data$EMin<-Data$DMin
      Data$EMax<-Data$DMax

    }

    Data <- cbind(Data, h=NA)

    #estimation of the thickness of the sample (h) from the new minimun and max depth of the sample

    Data<- Data |> dplyr::mutate (h = EMax-EMin)

    temp<-cbind(Data$EMin, Data$EMax, Data$h)
    colnames(temp)<-colnames(Fdf2)
    Fdf2<-rbind(Fdf2, temp)

  }
  Fdf<-cbind(df, Fdf2)

  return(Fdf)
}

