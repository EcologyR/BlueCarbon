#' Estimate the average organic carbon sequestration rate
#'
#' @description estimate the average organic carbon sequestration rate to the soil in a indicated time frame (by default last 100 years)
#' from the organic carbon concentration and ages obtained from a age-depth or age-accumulated mass model
#'
#' @param df A data.frame with, at least, columns: core, mind (minimum depth of the sample),
#' maxd (maximum depth of the sample), dbd (dry bulk density), oc (organic carbon %),
#' age (age of the sample obtained from a age-depth or age-accumulated mass model)
#' @param timeframe standardization time frame, by default 100 years
#' @param core Character Name of the column reporting core ID.
#' @param mind Character Name of the column reporting the minimum depth of each sample.
#' @param maxd Character Name of the column reporting the maximum depth of each sample.
#' @param dbd Character Name of the column reporting dry bulk density.
#' @param oc Character Name of the column reporting organic carbon concentrations.
#' @param age Character Name of the column reporting the age of each sample.
#'
#' @return data.frame with columns Core.id, F.WC (organic carbon sequestration rates at the whole core),
#' A.Max (maximum age of the core), and seq_rate (average organic carbon sequestration rate at the indicated time frame)
#' @export
#'
#' @examples
#' bluecarbon_decompact <- decompact(bluecarbon_data)
#' oc <- estimate_oc(bluecarbon_decompact)
#' out <- estimate_seq_rate(oc[[1]])


estimate_seq_rate <- function(df=NULL,
                         timeframe = 100,
                         core = "core",
                         mind = "mind_corrected",
                         maxd = "maxd_corrected",
                         dbd = "dbd",
                         oc = "eoc",
                         age = "age") {

  # class of the dataframe or tibble
  if (!inherits(df, "data.frame")) {
    stop("The data provided must be a tibble or data.frame")
  }
  if (!is.numeric(timeframe)) {stop("The time frame must be class numeric")}

  # name of the columns
  check_column_in_df(df, core)
  check_column_in_df(df, mind)
  check_column_in_df(df, maxd)
  check_column_in_df(df, dbd)
  check_column_in_df(df, oc)
  check_column_in_df(df, age)

  # class of the columns
  if (!is.numeric(df[[mind]])) {stop("Minimum depth data is not class numeric, please check")}
  if (!is.numeric(df[[maxd]])) {stop("Maximum depth data is not class numeric, please check")}
  if (!is.numeric(df[[dbd]])) {stop("Dry Bulk Density data is not class numeric, please check")}
  if (!is.numeric(df[[oc]])) {stop("Organic carbon data is not class numeric, please check")}
  if (!is.numeric(df[[age]])) {stop("Age data is not class numeric, please check")}

  # create variables with working names with the data in the columns specified by the user
  df_r <- df
  df_r$core_r <- df_r[[core]]
  df_r$mind_r <- df_r[[mind]]
  df_r$maxd_r <- df_r[[maxd]]
  df_r$dbd_r <- df_r[[dbd]]
  df_r$oc_r <- df_r[[oc]]
  df_r$age_r <- df_r[[age]]


  #select those cores with chronological models
  df_r <- df_r[!is.na(df_r$age_r),]
  df_r <- df_r[!is.na(df_r$oc_r),]

  df_h <- estimate_h(df = df_r,
                    core = "core_r",
                    mind = "mind_r",
                    maxd = "maxd_r")


  # estimate sequestration rates
  x <- split(df_h, df_h$core_r)
  BCF_l <- lapply(X = x,  estimate_core_f, timeframe = timeframe) # return a list
  BCF <- as.data.frame(do.call(rbind, BCF_l)) # from list to dataframe

  rownames(BCF) <- NULL

  return(BCF)

}




estimate_core_f <- function (df, timeframe) {

  core <- as.character(unique(df[1,"core_r"]))

  #colnames(df)<-colnames(df_h)

  if (is.unsorted(df$mind_r)) {stop("Samples must be ordered from shallow to deep")}


  #estimation of carbon g cm2 per sample, OCgcm2= carbon density (g cm3) by thickness (h)
  df$ocgcm2 <- df$dbd_r * (df$oc_r / 100) * df$h

  #estimation of the OC stock in the whole core
  seq_rate_wc <- sum(df$ocgcm2)/max(df$age_r)
  maxage <- max(df$age_r)

  # if max age is lower than the time frame by a 25% of the time frame do not estimate
  if ((max(df$age_r) + (max(df$age_r) * 0.25)) < timeframe) {
    message (paste("Core", core, "is younger than the time frame provided"))
    seq_rate <- NA } else {

      # if first sampole is older than timeframe do not stimate
      if ((df[1, "age_r"])>timeframe) {
        message (paste("Core", core, "resolution is to low estimate the sequestration rate in the last", timeframe))
        seq_rate <- NA } else {

      #estimation of the average carbon sequestration rate for the selected TimeFrame (OC stock / TimeFrame)

      if (max(df$age_r)==timeframe) {

        seq_rate<-sum(df$ocgcm2)/max(df$age_r)

      } else {
        df<-df[c(1:(length(which(df$age_r <=timeframe))+1)),]

        seq_rate<-(
          (sum(df[c(1:(nrow(df) - 1)), "ocgcm2"])+
             ((df[nrow(df),"ocgcm2"]/((max(df$age_r)-df[(nrow(df)-1),"age_r"])))
              *(timeframe-df[(nrow(df)-1),"age_r"])))/timeframe)
  }}}

  BCF <- data.frame(core = core, seq_rate_wc = seq_rate_wc, maxage = maxage, seq_rate = seq_rate)

  return(BCF)

}


