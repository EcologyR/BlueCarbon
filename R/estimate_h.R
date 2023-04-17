#' Estimation of the thickness of the sample
#'
#' @param df A [data.frame] with, at least, columns: Core.ID, DMin.D (corrected minimum depth of the sample) and DMax.D (corrected maximum depth of the sample)
#'
#' @return the initial [data.frame] with three aditional columns: EMin (estimated minimun depth of the sample), EMax (estimated maximun depth of the sample) and h (estimated thikness of the sample)
#' @export
#'
#' @examples
estimate_h <- function(df = NULL) {


  # class of the dataframe
  if (is.data.frame(df)==FALSE) {stop("The data provided is not class data.frame, please chaeck data and transforme")}

  # name of the columns
  if ("CoreID" %in% colnames(df)==FALSE) {stop("There is not column named CoreID. Please, check necessary columns in functions documentation")}
  if ("DMin.D" %in% colnames(df)==FALSE) {stop("There is not column named Min.D. Please, check necessary columns in functions documentation")}
  if ("DMax.D" %in% colnames(df)==FALSE) {stop("There is not column named Max.D. Please, check necessary columns in functions documentation")}

  # class of the columns
  if (is.numeric(df$Min.D)==FALSE) {stop("Minimum depth data is not class numeric, please chaeck")}
  if (is.numeric(df$Max.D)==FALSE) {stop("Maximum depth data is not class numeric, please chaeck")}

  #check for NAs in depth columns

  if (sum(is.na(df$DMin.D))>0){stop("Samples minimun depth column has NAs, please check")}
  if (sum(is.na(df$DMax.D))>0){stop("Samples maximun depth column has NAs, please check")}


  # create individual data frames per eaxh core
  X<-split(df, df$Core.ID)


  columns<-c(colnames(df), "EMin","EMax","h")
  Fdf = data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(Fdf) = columns

  for(i in 1:length(X)) {

    Data<-as.data.frame(X[i])
    colnames(Data)<-colnames(df)

    #check if there is spaces between samples (e.g, first sample ends at 5 cm and next starts at 7)
    space<- c()

    for (j in 1:(nrow(Data)-1)) {

      # if there are no spaces between samples min and maximun depth of samples remain the same
      if (Data[j,which(colnames(Data)=="DMax.D")] == Data[j+1,which(colnames(Data)=="DMin.D")]) {
        space[j]<-FALSE} else {space[j]<-TRUE}}

    if (any(space==TRUE)) {
      # if there are spaces between samples it estimate the medium point between the maximun depth of the sample and the minimun depth of the next sample
      # and divide that distance between both samples
      Data <- cbind(Data, EMin=NA, EMax=NA)
      Data[1,"EMin"]<-0
      Data[nrow(Data),"EMax"]<-Data[nrow(Data),"DMax.D"]
      for (j in 1:(nrow(Data)-1)) {
        if(space[j]==TRUE) {
          Data[j,"EMax"]<-Data[j,"DMax.D"]+((Data[j+1,"DMin.D"]-Data[j,"DMax.D"])/2)
          Data[j+1,"EMin"]<-Data[j,"DMax.D"]+((Data[j+1,"DMin.D"]-Data[j,"DMax.D"])/2)} else {
            Data[j,"EMax"]<-Data[j,"DMax.D"]
            Data[j+1,"EMin"]<-Data[j+1,"DMin.D"]}}

    }  else{
      Data <- cbind(Data, EMin=Data$DMin.D, EMax=Data$DMax.D)
    }

    Data <- cbind(Data, h=NA)

    #estimation of the thickness of the sample (h) from the new minimun and max depth of the sample

    Data<- Data |> dplyr::mutate (h = EMax-EMin)

    Fdf<-rbind(Fdf, Data)
  }

  return(Fdf)
}

