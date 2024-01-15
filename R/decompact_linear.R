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
