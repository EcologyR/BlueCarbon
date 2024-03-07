#' Calculate sediment properties after decompression
#'
#' @description
#' Accepts a data.frame with sample properties and returns a modified version
#' with sample properties after corrected for compression
#'
#'
#' @param df data.frame with core properties
#' @param compression name of the column with core compression IN PERCENTAGE
#' @param mind name of the column with minimum depth
#' @param maxd name of the column with maximum depth
#' @param dbd name of the column with dry bulk density
#'
#' @return None
#'
#' @examples
#' plot_crayons()
#'
#' @export





decompact_linear <- function(df   = NULL,
                     core         = "core",
                     compression  = "compression",
                     mind         = "mind",
                     maxd         = "maxd",
                     dbd          = NULL) {



  # class of the dataframe or tibble
  if (!inherits(df, "data.frame")) {
    stop("The data provided must be a tibble or data.frame")
  }

  # name of the columns
  check_column_in_df(df, core)
  # If compression is not in data.frame, create empty column
  if(!compression %in% names(df)) {
    if(is.null(df_fm)) stop("No compression values or field measurements were provided.")
    df$compression <- as.vector(rep(NA, nrow(df)), mode = "numeric")
  }
  check_column_in_df(df, mind)
  check_column_in_df(df, maxd)


  # class of the columns
  if (!is.numeric(df[[compression]])) {stop("Compression data is not class numeric, please check")}
  if (!is.numeric(df[[mind]])) {stop("Minimum depth data is not class numeric, please check")}
  if (!is.numeric(df[[maxd]])) {stop("Maximum depth data is not class numeric, please check")}
  if (!is.null(dbd)) {stopifnot("Dry bulk density data is not class numeric, please check" = is.numeric(df[[dbd]]))}

  # create variables with working names with the data in the columns specified by the user
  df_r <- df
  df_r$core_r <- df_r[[core]]
  df_r$compression_r <- df_r[[compression]]
  df_r$mind_r <- df_r[[mind]]
  df_r$maxd_r <- df_r[[maxd]]
  if (!is.null(dbd)) df_r$dbd_r <- df_r[[dbd]]

  # check if compression values are in percentage - largest compression value
  # should be bigger than 1, but we should also have non-zero values (all zeros means no compaction)
  if(!all(is.na(df_r$compression_r))){
    if (max(df_r$compression_r, na.rm = T) <= 1 & any(na.omit(df_r$compression_r) != 0)) {
      warning("Compresion values should be provided in %")
    }
  }

  #check again if there are NAs in compression and stop if there are cores that can not be decompress
  if (any(is.na(df_r$compression_r))) {

    cores_na_list<-unique(df_r[which(is.na(df_r$compression_r)), "core_r"])

    stop(
      paste0(
        "There are cores without estimated compresion: ",
        paste(cores_na_list, collapse = ", "),
        "\n",
        "Please, provide compression data of field measurements for all cores. If the core is not compressed compression should be 0 "
      ))}


  # apply decompression
  df_r<- df_r %>% dplyr::mutate (
    mind_corrected = mind_r/(1-(compression_r/100)),
    maxd_corrected = maxd_r/(1-(compression_r/100)))

  if (!is.null(dbd)){
    df_r<- df_r %>% dplyr::mutate (dbd_corrected = dbd_r * (1-(compression_r/100)))
  }

  # add corrected data to original data.frame to return to user
  df$mind_corrected <- df_r$mind_corrected
  df$maxd_corrected <- df_r$maxd_corrected
  df$compression    <- df_r$compression_r
  if (!is.null(dbd)){
    df$dbd_corrected <- df_r$dbd_corrected
  }


  return(df)
  }
