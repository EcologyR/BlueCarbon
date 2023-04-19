#' Test differences between observed and extrapolated stocks
#'
#' @description subset those cores that reach the standardization depth and estimates the stock (observed stock), estimate the stock from the linear relation of organic carbon accumulated mass and depth using the 90, 75, 50 and 25% top length of the indicated standardization depth. Compares the observed stock with the estimated stocks by extrapolation.
#'
#' @param df A [data.frame] with, at least, columns: CoreID, DMin (minimum depth of the sample), DMax (maximum depth of the sample), DBD (dry bulk density), POC (organic carbon %)
#' @param Depth standardization soil depth, by default 100 cm.
#'
#' @return A [data.frame] with the observed and extrapolated stocks. A plot with comparisons.
#' @export
#'
#' @examples

test_extrapolation <- function(df = NULL, Depth = 100) {

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


  # we select those cores larger than the standard depth

  columns<-colnames(df)
  DataE = data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(DataE) = columns



   X <- split(df, df$CoreID)

  for (i in 1:length(X)) {
    Data <- as.data.frame(X[i])
    colnames(Data) <- colnames(df)

    if (max(Data$DMax) < Depth) next

    DataE<-rbind(DataE,Data)}


   # estimate the stock at the standar depth

  ExtS <- data.frame(
    CoreID = character(),
    EXT.100 = numeric(),
    EXT.90 = numeric(),
    EXT.75 = numeric(),
    EXT.50 = numeric(),
    EXT.25 = numeric()
  )


    hundreth<- Depth #100% of the extrapolation length
    ninety <- Depth * 0.9 #90% of the extrapolation length
    seventy <- Depth * 0.75 #90% of the extrapolation length
    fifhty <- Depth * 0.50 #90% of the extrapolation length
    twentififty <- Depth * 0.25 #90% of the extrapolation length


    #estimate observed stock
    temp100<-estimate_stock (DataE, Depth=hundreth)
    ExtS[c(1:nrow(temp100)),"CoreID"]<-temp100$CoreID
    ExtS$EXT.100<-temp100$Stock

   # estimate stock with a 90, 75, 50 and 25 percentage of the standard depth

    X <- split(DataE, DataE$CoreID)

    for (i in 1:length(X)) {
      Data <- as.data.frame(X[i])
      colnames(Data) <- colnames(DataE)

    Data<-Data[!is.na(Data$fOC),]
    Data<-estimate_h(Data)

    #estimation of carbon g cm2 per sample, OCgcm2= carbon density (g cm3) by thickness (h)
    Data <-Data |> dplyr::mutate (OCgcm2 = DBD*(fOC/100)*h)

    #estimate organic carbon accumulated mass
    Data <-Data |> dplyr::mutate (OCM = cumsum(OCgcm2))

    Data<- subset(Data,Data$DMax<=ninety)

    if (nrow(Data)>3){
      model90<-lm(OCM ~ DMax, data=Data)
      ExtS[i,3]<-coef(model90)[1] + 100*coef(model90)[2]}

    Data<- subset(Data,Data$DMax<=seventy)
    if (nrow(Data)>3){
      model75<-lm(OCM ~ DMax, data=Data)
      ExtS[i,4]<-coef(model75)[1] + 100*coef(model75)[2]}

    Data<- subset(Data,Data$DMax<=fifhty)
    if (nrow(Data)>3){
      model50<-lm(OCM ~ DMax, data=Data)
      ExtS[i,5]<-coef(model50)[1] + 100*coef(model50)[2]}

    Data<- subset(Data,Data$DMax<=twentififty)
    if (nrow(Data)>3){
      model25<-lm(OCM ~ DMax, data=Data)
      ExtS[i,6]<-coef(model25)[1] + 100*coef(model25)[2]}}


  #############
  # we test the correlation between the stock at 1m estimated from real data and the models
  # and the error of the models
  ##############


  CorMat <- cor(na.omit(ExtS[, c(2:6)]), method = "pearson")


  corrplot::corrplot(
    CorMat,
    type = "upper",
    order = "hclust",
    tl.col = "black",
    tl.srt = 45
  )

  #write.csv(CorMat,file.path(Folder,"Corr.Extrapolation.csv"),sep=";", dec=",")


  ExtS <- ExtS |> dplyr::mutate (Error.90 = (abs(EXT.100 - EXT.90) * 100) / EXT.100)
  ExtS <- ExtS |> dplyr::mutate (Error.75 = (abs(EXT.100 - EXT.75) * 100) / EXT.100)
  ExtS <- ExtS |> dplyr::mutate (Error.50 = (abs(EXT.100 - EXT.50) * 100) / EXT.100)
  ExtS <- ExtS |> dplyr::mutate (Error.25 = (abs(EXT.100 - EXT.25) * 100) / EXT.100)

  summary(ExtS)


  #Global Error
  m.ExtS <- ExtS[, c(1, 7:10)]
  m.ExtS <- reshape::melt(m.ExtS, id = c("CoreID"))

  library("ggplot2")

  P1<-ggplot2::ggplot(m.ExtS, aes(variable, value)) + ylab("% of deviation from observed value") + xlab("% of standar depth") +
    geom_boxplot() +
    geom_jitter() +
    scale_x_discrete(labels=c("Error.90" = "90%", "Error.75" = "75%", "Error.50" = "50%", "Error.25" = "25%"))+
    theme(
      #axis.title.x = element_blank(),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )

  P2<-ggplot2::ggplot(ExtS, aes(ExtS[, 2], ExtS[, 3])) + xlab("Observed Stock") + ylab("Extrapolated stock") +
    geom_point(aes(ExtS[, 2], ExtS[, 3], color = "90%"), size = 2) +
    geom_point(aes(ExtS[, 2], ExtS[, 4], color = "75%"), size = 2) +
    geom_point(aes(ExtS[, 2], ExtS[, 5], color = "50%"), size = 2) +
    geom_point(aes(ExtS[, 2], ExtS[, 6], color = "25%"), size = 2) +
    theme(text = element_text(size = 15)) +
    #geom_text_repel(aes(label=A[,1]), size=4)+
    xlim(0, 5) + ylim(0, 5) +
    geom_abline()

Extrapolation_plot<-gridExtra::grid.arrange(P1,P2, ncol=2)

return(ExtS)
return(Extrapolation_plot)

}
