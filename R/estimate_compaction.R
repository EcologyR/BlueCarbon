
#' Estimate core compaction
#'
#' @description Calculates Percentage of core compression for cores
#' Accepts a data.frame with core field measurements and returns a modified version
#' of it, with the addition of the estimated parameters
#'
#' @param df data.frame with core properties
#' @param core_id core identification
#' @param sampler_length name of the column with the total length of the sampler tube
#' @param internal_distance name of the column with distance between sampler top and core surface
#' @param external_distance name of the column with distance between sampler top and sediment surface
#'
#' @return the initial data.frame with the addition of Percentage of core compression
#' @export
#'
#' @examples
#'

estimate_compaction <-  function(df,
           core= "core",
           sampler_length = "sampler_length",
           internal_distance = "internal_distance",
           external_distance = "external_distance") {

    # class of the dataframe or tibble
    if (!inherits(df, "data.frame")) {
      stop("The data provided must be a tibble or data.frame")
    }


    # name of the columns
    check_column_in_df(df, core)
    check_column_in_df(df, sampler_length)
    check_column_in_df(df, internal_distance)
    check_column_in_df(df, external_distance)


    # class of the columns
    if (!is.numeric(df[[sampler_length]])) {stop("'sampler_length' data must be class numeric")}
    if (!is.numeric(df[[internal_distance]])) {stop("'internal_distance' data must be class numeric")}
    if (!is.numeric(df[[external_distance]])) {stop("'external_distance' data must be class numeric")}

    # check that the internal distance is always larger than the external distance

    if (any(df[[internal_distance]] < df[[external_distance]], na.rm = T)) {stop("internal_distance is smaller than the external_distance, this is not posible, please check")}

    # create variables with working names with the data in the columns specified by the user
    df_r <- df
    df_r$core_r <- df_r[[core]]
    df_r$sampler_length_r <- df_r[[sampler_length]]
    df_r$internal_distance_r <- df_r[[internal_distance]]
    df_r$external_distance_r <- df_r[[external_distance]]


    # estimate compaction correction factor

    if (any(!is.na(df_r[, c("sampler_length_r", "internal_distance_r", "external_distance_r")]))) {

    compaction_correction_factor <-
      (df_r[, "sampler_length_r"] - df_r[, "internal_distance_r"]) /
      (df_r[, "sampler_length_r"] - df_r[, "external_distance_r"])

    # compaction rate as percentage
    df$compression <-
      (1 - compaction_correction_factor) * 100 }

    return(df)
  }
