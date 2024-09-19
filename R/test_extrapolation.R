#' Test differences between observed and extrapolated stocks
#'
#' @description Subset those cores that reach the standardization depth and estimates the stock (observed stock),
#' estimate the stock from the linear relation of organic carbon accumulated mass and depth using the 90, 75, 50 and 25%
#' top length of the indicated standardization depth. Compares the observed stock with the estimated stocks by extrapolation.
#'
#' @param df A data.frame with, at least, columns: core,
#' mind (minimum depth of the sample), maxd (maximum depth of the sample),
#' dbd (dry bulk density), oc (organic carbon %)
#' @param depth Number Standardization soil depth, by default 100 cm.
#' @param core Character Name of the column reporting core ID.
#' @param mind Character Name of the column reporting the minimum depth of each sample.
#' @param maxd Character Name of the column reporting the maximum depth of each sample.
#' @param dbd Character Name of the column reporting dry bulk density.
#' @param oc Character Name of the column reporting organic carbon concentrations.
#'
#'
#' @return A data.frame with the observed and extrapolated stocks. A plot with comparisons.
#' @export
#' @import ggplot2
#'
#' @examples
#' bluecarbon_decompact <- decompact(bluecarbon_data)
#' oc <- estimate_oc(bluecarbon_decompact)
#' out <- test_extrapolation(oc[[1]])
#'
#'

test_extrapolation <- function(df = NULL,
                               depth = 100,
                               core = "core",
                               mind = "mind_corrected",
                               maxd = "maxd_corrected",
                               dbd = "dbd",
                               oc = "eoc") {

  # class of the dataframe or tibble
  if (!inherits(df, "data.frame")) {
    stop("The data provided must be a tibble or data.frame")
  }

  if (!is.numeric(depth)) {stop("Depth provided must be class numeric")}

  # name of the columns
  check_column_in_df(df, core)
  check_column_in_df(df, mind)
  check_column_in_df(df, maxd)
  check_column_in_df(df, dbd)
  check_column_in_df(df, oc)

  # class of the columns
  if (!is.numeric(df[[mind]])) {stop("Column 'mind' must be class numeric")}
  if (!is.numeric(df[[maxd]])) {stop("Column 'maxd' must be class numeric")}
  if (!is.numeric(df[[dbd]])) {stop("Column 'dbd' must be class numeric")}
  if (!is.numeric(df[[oc]])) {stop("Column 'oc' must be class numeric")}


  # create variables with working names with the data in the columns specified by the user
  df_r <- df
  df_r$core_r <- df_r[[core]]
  df_r$mind_r <- df_r[[mind]]
  df_r$maxd_r <- df_r[[maxd]]
  df_r$dbd_r <- df_r[[dbd]]
  df_r$oc_r <- df_r[[oc]]

  # we select those cores larger than the standard depth

  columns <- colnames(df_r)
  x <- split(df_r, df_r$core_r)

   cores_e <- lapply( X = x,  select_cores, depth = depth, columns) # return a list
   cores_e <- cores_e[!vapply(cores_e, is.null, logical(1))]
   cores_e <- as.data.frame(do.call(rbind, cores_e)) # from list to dataframe

   if (nrow(cores_e) == 0) {stop("None of the cores provided reach the standard depth")}


   # estimate observed stock

   observed_stock <- estimate_oc_stock(df = cores_e,
                    depth = depth,
                    core = "core_r",
                    mind = "mind_r",
                    maxd = "maxd_r",
                    dbd = "dbd_r",
                    oc = "oc_r")



   # estimate corrected sample depth, h and organic carbon density and carbon mass per sample

   cores_e <- cores_e[!is.na(cores_e$oc_r),]
   cores_e <- estimate_h(cores_e,
                       core = "core_r",
                       mind = "mind_r",
                       maxd = "maxd_r")

   #estimation of carbon g cm2 per sample, OCgcm2= carbon density (g cm3) by thickness (h)
   cores_e <- cores_e |> dplyr::mutate (ocgcm2 = dbd_r*(oc_r/100)*h)

   # create data frames with same cores different lengths

   x <- split(cores_e, cores_e$core_r)
   columns <- colnames(cores_e)

   cores_c <- lapply( X = x,  cut_cores, depth = depth)


  df90 <- lapply(X = cores_c, extract_cores, position = 1)

  if (!any(sapply(df90, nrow)>2)) {stop("None of the cores provide had enough samples to model the stock at 90% of Depth provided")}

  df75 <- lapply(X = cores_c, extract_cores, position = 2)
  df50 <- lapply(X = cores_c, extract_cores, position = 3)
  df25 <- lapply(X = cores_c, extract_cores, position = 4)



 # modeling stock
  stocks90 <- lapply(X = df90, model_stock, depth = depth)
  stocks90 <- as.data.frame(do.call(rbind, stocks90))
  stocks75 <- lapply(X = df75, model_stock, depth = depth)
  stocks75 <- as.data.frame(do.call(rbind, stocks75))
  stocks50 <- lapply(X = df50, model_stock, depth = depth)
  stocks50 <- as.data.frame(do.call(rbind, stocks50))
  stocks25 <- lapply(X = df25, model_stock, depth = depth)
  stocks25 <- as.data.frame(do.call(rbind, stocks25))

predictions <- cbind(stocks90, stocks75, stocks50, stocks25)
colnames(predictions) <- c("stock_90", "stock_90_se", "stock_75", "stock_75_se",
                        "stock_50", "stock_50_se", "stock_25", "stock_25_se")


  stocks_f <- cbind(observed_stock[,c( 1, 4)], predictions)


  # estimate the error % of extrapolations and observed stock

  stocks_f <- stocks_f |> dplyr::mutate (error_90 = (abs(stock - stock_90) * 100) / stock)
  stocks_f <- stocks_f |> dplyr::mutate (error_75 = (abs(stock - stock_75) * 100) / stock)
  stocks_f <- stocks_f |> dplyr::mutate (error_50 = (abs(stock - stock_50) * 100) / stock)
  stocks_f <- stocks_f |> dplyr::mutate (error_25 = (abs(stock - stock_25) * 100) / stock)

  #Global Error
  m_stocks_f <- stocks_f[, c(1, 11:14)]
  m_stocks_f <- reshape::melt(m_stocks_f, id = c("core"))

  p1 <- ggplot(m_stocks_f, aes(variable, value)) +
    ylab("% of deviation from observed value") +
    xlab("% of standard depth") +
    geom_boxplot() +
    geom_jitter() +
    theme(
      #axis.title.x = element_blank(),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )

  limits <- max(stocks_f[,c(2,3,5,7,9)], na.rm = TRUE) + (max(stocks_f[,c(2,3,5,7,9)], na.rm = TRUE)*20/100)

  p2 <- ggplot(stocks_f, aes(stock, stock_90)) +
    xlab("Observed Stock") + ylab("Extrapolated stock") +
    geom_point(aes(color = "90%"), size = 2) +
    {if (!all(is.na(stocks75$mStock))) geom_point(aes(stock, stock_75, color = "75%"), size = 2)} +
    {if (!all(is.na(stocks50$mStock))) geom_point(aes(stock, stock_50, color = "50%"), size = 2)} +
    {if (!all(is.na(stocks25$mStock))) geom_point(aes(stock, stock_25, color = "25%"), size = 2)} +
    theme(text = element_text(size = 15)) +
    labs(color = NULL) +
    xlim(0, limits) + ylim(0, limits) +
    geom_abline()

  extrapolation_plot <- gridExtra::grid.arrange(p1,p2, ncol = 2)

  extrapolation_plot

  return(stocks_f)


}


# core processing ------------------------------------------------------

#### SELEC CORES ####
select_cores<-function (df,
                        depth,
                        columns="columns") {

  data <- as.data.frame(df)
  colnames(data) <- columns
  if (max(data$maxd_r) > depth) { core<-data
  } else {core<-NULL}

  return (core)
}

#### CUT CORES ####
cut_cores<- function (df,
                      depth,
                      columns= "columns") {

  data <- as.data.frame(df)
  colnames(data) <- columns

  cores90<-subset(df,df$emax<= depth * 0.9)
  cores75<-subset(df,df$emax<= depth * 0.75)
  cores50<-subset(df,df$emax<= depth * 0.5)
  cores25<-subset(df,df$emax<= depth * 0.25)


  dfs<-list(cores90, cores75, cores50, cores25)

}

#### EXTRACT CORES ####
extract_cores<- function (lst, position) {
  return(lst[[position]])
}


# stock modelisation ------------------------------------------------------


model_stock<- function (df, depth = depth) {

  df <-df |> dplyr::mutate (ocM = cumsum(ocgcm2))

  if (nrow(df)>=3){
    model <- stats::lm(ocM ~ emax, data=df)
    mStock <- stats::predict(model, newdata = data.frame(emax=depth))
    mStock_se <- stats::predict(model, newdata = data.frame(emax = depth), se.fit = TRUE)$se.fit
  } else {
    mStock <- NA
    mStock_se <- NA
  }

  stocks<-data.frame(mStock = mStock, mStock_se = mStock_se)

  return(stocks)
}


