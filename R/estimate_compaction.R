
#' Estimate core compaction
#'
#' @description Calculates Percentage of core compression for cores
#' Accepts a data.frame with core properties and returns a modified version
#' of it, with the addition of the estimated parameters
#'
#' @param df data.frame with core properties
#' @param sampler_length name of the column with the total length of the sampler tube
#' @param internal_distance name of the column with distance between sampler top and core surface
#' @param external_distance name of the column with distance between sampler top and sediment surface
#'
#' @return the initial data.frame with the addition of Percentage of core compression
#' @export
#'
#' @examples
#'

estimate_compaction <-
  function(df,
           sampler_length = "sampler_length",
           internal_distance = "internal_distance",
           external_distance = "external_distance") {

    # class of the dataframe or tibble
    if (!inherits(df, "data.frame")) {
      stop("The data provided must be a tibble or data.frame")
    }
    if (!is.numeric(timeframe)) {stop("The time frame must be class numeric")}


    # name of the columns
    check_column_in_df(df, sampler_length)
    check_column_in_df(df, internal_distance)
    check_column_in_df(df, external_distance)


    # class of the columns
    if (!is.numeric(df[[sampler_length]])) {stop("'sampler_length' data must be class numeric")}
    if (!is.numeric(df[[internal_distance]])) {stop("'internal_distance' data must be class numeric")}
    if (!is.numeric(df[[external_distance]])) {stop("'external_distance' data must be class numeric")}



    # estimate compaction correction factor
    compaction_correction_factor <-
      (df[, sampler_length] - df[, internal_distance]) /
      (df[, sampler_length] - df[, external_distance])

    # compaction rate as percentage
    df$compression_rate <-
      (1 - compaction_correction_factor) * 100

    return(df)
  }
