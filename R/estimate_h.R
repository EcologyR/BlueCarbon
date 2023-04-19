#' Estimation of the thickness of the sample
#'
#' @description checks for space between samples and, if any, divide this space between the previous and next sample to return sample thickness withouth gaps in the core
#'
#' @param df A [data.frame] with with, at least, columns CoreID, DMin (minimum depth of the sample)and DMax (maximum depth of the sample)
#'
#' @return the initial [data.frame] with three additional columns: EMin (estimated minimum depth of the sample), EMax (estimated maximum depth of the sample) and h (estimated thikness of the sample)
#' @export
#'
#' @examples
estimate_h <- function(df = NULL) {

  # class of the dataframe
  if (is.data.frame(df)==FALSE) {stop("The data provided is not class data.frame, please chaeck data and transforme")}

  # name of the columns
  if ("CoreID" %in% colnames(df)==FALSE) {stop("There is not column named CoreID. Please, check necessary columns in functions documentation")}
  if ("DMin" %in% colnames(df)==FALSE) {stop("There is not column named DMin. Please, check necessary columns in functions documentation")}
  if ("DMax" %in% colnames(df)==FALSE) {stop("There is not column named DMax. Please, check necessary columns in functions documentation")}

  # class of the columns
  if (is.numeric(df$DMin)==FALSE) {stop("Minimum depth data is not class numeric, please check")}
  if (is.numeric(df$DMax)==FALSE) {stop("Maximum depth data is not class numeric, please check")}

  #check for NAs in depth columns
  if (sum(is.na(df$DMin))>0){stop("Samples minimun depth column has NAs, please check")}
  if (sum(is.na(df$DMax))>0){stop("Samples maximun depth column has NAs, please check")}


  # create individual data frames per each core

  df$CoreID <- factor(df$CoreID, levels=unique(df$CoreID))
  X<-split(df, df$CoreID)


  columns<-c("EMin","EMax","h")
  Fdf2 = data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(Fdf2) = columns


  for(i in 1:length(X)) {

    Data<-as.data.frame(X[i])
    colnames(Data)<-colnames(df)

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

