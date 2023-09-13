decompact_linear <- function(df = NULL,
                      df_fm = NULL,
                     core = "core",
                     compression = "compression",
                     mind = "mind",
                     maxd = "maxd") {


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


  #warning in compression is lower than 1 that it must be in percentage not tanto por 1


  # create variables with working names with the data in the columns specified by the user
  df_r <- df
  df_r$core_r <- df_r[[core]]
  df_r$compression_r <- df_r[[compression]]
  df_r$mind_r <- df_r[[mind]]
  df_r$maxd_r <- df_r[[maxd]]


    df_r<- df_r %>% mutate (dmin = mind_r/(1-(compression_r/100)))
    df_r<- df_r %>% mutate (dmax = maxd_r/(1-(compression_r/100)))

    return(df_r)}


decompact_exp <- function(df = NULL,
                          df_fd = NULL,
                          xx= 0.1,
                          core = "core",
                          mind = "mind",
                          maxd = "maxd",
                          sampler_length = "sampler_length",
                          internal_distance = "internal_distance",
                          external_distance = "external_distance" ) {

  # class of the dataframe or tibble
  if (!inherits(df, "data.frame")) {
    stop("The data provided must be a tibble or data.frame")
  }
  if (!inherits(df_fd, "data.frame")) {
    stop("The data provided must be a tibble or data.frame")
  }

  # name of the columns
  check_column_in_df(df, core)
  check_column_in_df(df, mind)
  check_column_in_df(df, maxd)

  check_column_in_df(df_fd, sampler_length)
  check_column_in_df(df_fd, internal_distance)
  check_column_in_df(df_fd, external_distance)

  # class of the columns
  if (!is.numeric(df[[compression]])) {stop("Compression data is not class numeric, please check")}
  if (!is.numeric(df[[mind]])) {stop("Minimum depth data is not class numeric, please check")}
  if (!is.numeric(df[[maxd]])) {stop("Maximum depth data is not class numeric, please check")}

  if (!is.numeric(df_fd[[sampler_length]])) {stop("'sampler_length' data must be class numeric")}
  if (!is.numeric(df_fd[[internal_distance]])) {stop("'internal_distance' data must be class numeric")}
  if (!is.numeric(df_fd[[external_distance]])) {stop("'external_distance' data must be class numeric")}

  # create variables with working names with the data in the columns specified by the user
  df_r <- df
  df_r$core_r <- df_r[[core]]
  df_r$mind_r <- df_r[[mind]]
  df_r$maxd_r <- df_r[[maxd]]

  df_fd_r <- df_fd
  df_fd_r$core_r <- df_fd_r[[core]]
  df_fd_r$sampler_length_r <- df_fd_r[[sampler_length]]
  df_fd_r$internal_distance_r <- df_fd_r[[internal_distance]]
  df_fd_r$external_distance_r <- df_fd_r[[external_distance]]



    test <- data.frame(x = c(((df_fd_r$sampler_length_r - df_fd_r$external_distance_r) - (df_fd_r$sampler_lenght_r - df_fd_r$internal_distance_r)), (df_fd_r$sampler_length_r - df_fd_r$external_distance_r)),
                       y = c(((df_fd_r$sampler_length_r - df_fd_r$external_distance_r) - (df_fd_r$sampler_lenght_r - df_fd_r$internal_distance_r)), 0.1))

    a <- as.numeric(stats::coef(drc::drm(y~x, data=test, fct=aomisc::DRC.expoDecay()))[1])
    b <- as.numeric(stats::coef(drc::drm(y~x, data=test, fct=aomisc::DRC.expoDecay()))[2])

    decomp$cm_obs <- data$cm + ((sampler_lenght - external_distance) - (sampler_lenght - internal_distance))
    decomp$cm_deco <- decomp$cm_obs -
      (a * exp(-b*decomp$cm_obs))


    return(sample_data)
  }
