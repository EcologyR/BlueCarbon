#' Estimation of the thickness of the sample
#'
#' @description checks for space between samples and, if any, divide this space between the previous and next sample to return sample thickness withouth gaps in the core
#'
#' @param df A [data.frame] with with the required columns
#' @param CoreID the name of the column (between "") from the df with the Core identification for each sample
#' @param DMin the name of the column (between "") from the df with the minimum depth of that sample (already corrected if needed)
#' @param DMax the name of the column (between "") from the df with the maximum depth of that sample (already corrected if needed)
#'
#' @return the initial [data.frame] with three additional columns: EMin (estimated minimum depth of the sample), EMax (estimated maximum depth of the sample) and h (estimated thikness of the sample)
#' @export
#'
#' @examples
estimate_h <- function(df = NULL, CoreID="CoreID", DMin= "DMin", DMax="DMax") {


  df2<-as.data.frame(cbind(df[[CoreID]], df[[DMin]],df[[DMax]]))
  colnames(df2)<-c("CoreID","DMin","DMax")
  df2[, 2:3] <- sapply(df2[, 2:3], as.numeric)

  #check for NAs in depth columns

  if (sum(is.na(df2$DMin))>0){stop("Samples minimun depth column has NAs, please check")}
  if (sum(is.na(df2$DMax))>0){stop("Samples maximun depth column has NAs, please check")}


  # create individual data frames per each core

  df2$CoreID <- factor(df2$CoreID, levels=unique(df2$CoreID))
  X<-split(df2, df2$CoreID)


  columns<-c("EMin","EMax","h")
  Fdf2 = data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(Fdf2) = columns

  for(i in 1:length(X)) {

    Data<-as.data.frame(X[i])
    colnames(Data)<-colnames(df2)

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

    Fdf2<-rbind(Fdf2, Data[,c(4:6)])
  }
  Fdf<-cbind(df, Fdf2)

  return(Fdf)
}

