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

  # create variables with working names with the data in the columns specified by the user
  df_r <- df
  df_r$core_r <- df_r[[core]]
  df_r$compression_r <- df_r[[compression]]
  df_r$mind_r <- df_r[[mind]]
  df_r$maxd_r <- df_r[[maxd]]

  #warning in compression is lower than 1 that it must be in percentage not tanto por 1

  if (min(df_r$compression_r, na.rm=T)<1) {warning("Compresion values should be provided in %")}


  # check if a compression % is provided. If not estimate from field measurements
  if (any(is.na(df_r$compression_r))) {

    df_r<-fill_compression(df_r, df_fm)}

  #check again if there are NAs in compression and stop if there are cores that con not be decompress

  if (any(is.na(df_r$compression_r))) {

    cores_na_list<-unique(df_r[which(is.na(df_r$compression_r)), "core_r"])

    for (i in 1:length(cores_na_list)) {
    warning("There are cores without estimated compresion data or field mesurements: ", cores_na_list[i])}

    stop("Please, provide compression data of field measurements for all cores")}


  # check there are compression values in all rows. If not give warning (one warning per core, not per row)


    df_r<- df_r %>% mutate (dmin = mind_r/(1-(compression_r/100)))
    df_r<- df_r %>% mutate (dmax = maxd_r/(1-(compression_r/100)))


    # dry bulk density too if there are dry bulk density data


    return(df_r)}




#### compression estimation ####

fill_compression<- function (df_r = df_r, df_fm = df_fm) {

  # check for which core that have no compresion data are in the field mesurements dataframe

  #list of cores without compression data and that are in field measurements data frame
  core_list<-unique(unique(df_r[which(is.na(df_r$compression_r)), "core_r"])
                  [unique(df_r[which(is.na(df_r$compression_r)), "core_r"]) %in% df_fm$core])



  #extraer como datas.frame la fila con los datos del core para el que se debe calcular la compresion

  for (i in 1:length(core_list)) {

  core_id<-core_list[i]

  data<-df_fm[df_fm == core_id,]
  data<-data[1,]

  temp<-estimate_compaction (data)

  # fill compression data

  if ("compression" %in% names(temp)) {
  df_r[which(df_r$core_r == core_id), "compression_r"] <- temp[1,"compression"]}}



  return (df_r)

  }





