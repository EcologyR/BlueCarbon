decompact <- function(df = NULL,
                     core = "core",
                     compression = "compression",
                     mind = "mind",
                     maxd = "maxd",
                     method = "linear") {


  # class of the dataframe or tibble
  if (!inherits(df, "data.frame")) {
    stop("The data provided must be a tibble or data.frame")
  }

  # name of the columns
  check_column_in_df(df, core)
  check_column_in_df(df, compression)
  check_column_in_df(df, mind)
  check_column_in_df(df, maxd)

  # class of the columns
  if (!is.numeric(df[[compression]])) {stop("Compression data is not class numeric, please check")}
  if (!is.numeric(df[[mind]])) {stop("Minimum depth data is not class numeric, please check")}
  if (!is.numeric(df[[maxd]])) {stop("Maximum depth data is not class numeric, please check")}

  #warning in compresion is lower than 1 that it must be in percentage not tanto por 1


  # Stop if method is not linear or exp
  if (!(method %in% c("linear", "exp"))) {
    return("Method must be either 'linear' or 'exp'")
  }

  # create variables with working names with the data in the columns specified by the user
  df_r <- df
  df_r$core_r <- df_r[[core]]
  df_r$compression_r <- df_r[[compression]]
  df_r$mind_r <- df_r[[mind]]
  df_r$maxd_r <- df_r[[maxd]]


  # estimate depth correction with "linear" method
  if (method == "linear") {

    df_r<- df_r %>% mutate (dmin = mind_r/(1-(compression_r/100)))
    df_r<- df_r %>% mutate (dmax = maxd_r/(1-(compression_r/100)))

    return(df_r)}

  # estimate depth correction with "exponential" model
  if(method == "exp") {

    test <- data.frame(x = c(((sampler_lenght - external_distance) - (sampler_lenght - internal_distance)), (sampler_lenght - external_distance)),
                       y = c(((sampler_lenght - external_distance) - (sampler_lenght - internal_distance)), 0.1))

    a <- as.numeric(stats::coef(drc::drm(y~x, data=test, fct=aomisc::DRC.expoDecay()))[1])
    b <- as.numeric(stats::coef(drc::drm(y~x, data=test, fct=aomisc::DRC.expoDecay()))[2])

    decomp$cm_obs <- data$cm + ((sampler_lenght - external_distance) - (sampler_lenght - internal_distance))
    decomp$cm_deco <- decomp$cm_obs -
      (a * exp(-b*decomp$cm_obs))
    decomp$sect_h <- c(dplyr::first(decomp$cm_deco), diff(decomp$cm_deco))
    decomp$volume <- (((pi * (sampler_diameter/2)^2) * decomp$sect_h)/2) #volume is divided by two as half section is used
    decomp$density <- data$weight/decomp$volume

    return(sample_data)
  }}
