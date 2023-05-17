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
#' @examples test_extrapolation(A, Depth = 50)
#' @examples test_extrapolation(A)

test_extrapolation <- function(df = NULL,
                               depth = 100,
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
  if (!is.numeric(depth)) {stop("The Depth provided is not class numeric, please chaeck data and transforme")}

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

  # we select those cores larger than the standard depth

  columns<-colnames(df_r)
  x <- split(df_r, df_r$core_r)


   select_cores<-function (df, depth) {

     data <- as.data.frame(df)
     colnames(data) <- columns
     if (max(data$dmax_r) > depth) { core<-data
     } else {core<-NULL}

     return (core)
   }

   cores_e<- lapply( X = x,  select_cores, depth = depth) # return a list
   cores_e<-cores_e[!vapply(cores_e, is.null, logical(1))]
   cores_e <- as.data.frame(do.call(rbind, cores_e)) # from list to dataframe


   # estimate obserbed stock

   observed_stock<-estimate_stock(df = cores_e,
                    Depth = depth,
                    core = "core_r",
                    dmin = "dmin_r",
                    dmax = "dmax_r",
                    dbd = "dbd_r",
                    oc = "eoc_r")



   # estimate corrected sample depth, h and organic carbon density and carbon mass per sample

   cores_e<-cores_e[!is.na(cores_e$oc_r),]
   cores_e<-estimate_h(cores_e)

   #estimation of carbon g cm2 per sample, OCgcm2= carbon density (g cm3) by thickness (h)
   cores_e <-cores_e |> dplyr::mutate (ocgcm2 = dbd_r*(oc_r/100)*h)

   # create data frames with same cores different lengths

   x <- split(cores_e, cores_e$core_r)
   columns<-colnames(cores_e)

   cut_cores<- function (df, depth) {

     data <- as.data.frame(df)
     colnames(data) <- columns

     cores90<-subset(df,df$emax<= depth * 0.9)
     cores75<-subset(df,df$emax<= depth * 0.75)
     cores50<-subset(df,df$emax<= depth * 0.5)
     cores25<-subset(df,df$emax<= depth * 0.25)


    dfs<-list(cores90, cores75, cores50, cores25)

   }

   cores_c<- lapply( X = x,  cut_cores, depth = depth)

  extract<- function (lst, position) {
    return(lst[[position]])
  }

  df90<-lapply(X= cores_c, extract, position=1)
  df75<-lapply(X= cores_c, extract, position=2)
  df50<-lapply(X= cores_c, extract, position=3)
  df25<-lapply(X= cores_c, extract, position=4)


 # modeling stock

  model_stock<- function (df, depth = depth) {

    df <-df |> dplyr::mutate (ocM = cumsum(ocgcm2))

    if (nrow(Data)>3){
      model<-lm(ocM ~ emax, data=df)
      mStock<-predict(model, newdata = data.frame(emax=depth))
      mStock_se
      } else {
        mStock<-NA
        mStock_se<-NA
      }

    stocks<-data.frame(mStock = mStock, mStock_se = mStock_se)

    return(stocks)
  }

  stocks90<-lapply(X = df90, model_stock, depth = depth)
  stocks90 <- as.data.frame(do.call(rbind, stocks90))
  stocks75<-lapply(X = df75, model_stock, depth = depth)
  stocks75 <- as.data.frame(do.call(rbind, stocks75))
  stocks50<-lapply(X = df50, model_stock, depth = depth)
  stocks50 <- as.data.frame(do.call(rbind, stocks50))
  stocks25<-lapply(X = df25, model_stock, depth = depth)
  stocks25 <- as.data.frame(do.call(rbind, stocks25))

predictions<-cbind(stocks90, stocks75, stocks50, stocks25)


  stocks_f<-cbind(stocks_r, predictions)




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



df = df_f
depth = 100
core = "core"
dmin = "mind"
dmax = "maxd"
dbd = "dbd"
oc = "eoc"
