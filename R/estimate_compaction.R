#' Estimate Core Compaction
#'
#' @description
#' Estimates the percentage of core compaction using measurements
#' from a data.frame containing core properties. It computes a correction factor
#' based on sampler tube length, internal distance, and external distance, and
#' adds a 'compaction' column to the input data.frame with the calculated compaction
#' rate as a percentage.
#'
#' @param df A data.frame containing core properties.
#' @param core_id Character Name of the column identifying each core.
#' @param sampler_length Character Name of the column with the total length of the
#' sampler tube.
#' @param internal_distance Character Name of the column with the distance between
#' sampler top and core surface.
#' @param external_distance Character Name of the column with the distance between
#' sampler top and sediment surface.
#'
#' @return
#' Returns the input data.frame with an additional 'compaction' column indicating
#' the estimated percentage of core compaction.
#'
#' @examples
#' df <- estimate_compaction(core_comp,
#' core_id = "core",
#' sampler_length = "sampler_length",
#' internal_distance = "internal_distance",
#' external_distance = "external_distance")
#'
#' @export
#'
estimate_compaction <- function(df = NULL,
                                core_id = "core",
                                sampler_length = "sampler_length",
                                internal_distance = "internal_distance",
                                external_distance = "external_distance") {

  # Check input data type
  if (!inherits(df, "data.frame")) {
    stop("The data provided must be a tibble or data.frame")
  }

  # Check existence of required columns
  check_column_in_df(df, core_id)
  check_column_in_df(df, sampler_length)
  check_column_in_df(df, internal_distance)
  check_column_in_df(df, external_distance)

  # Check data types of specified columns
  if (!is.numeric(df[[sampler_length]])) {
    stop("Column 'sampler_length' must be numeric.")
  }
  if (!is.numeric(df[[internal_distance]])) {
    stop("Column 'internal_distance' must be numeric.")
  }
  if (!is.numeric(df[[external_distance]])) {
    stop("Column 'external_distance' must be numeric.")
  }


  ## Omit missing data
  complete <- stats::complete.cases(df)
  if (!all(complete)) {
    warning("Removing cores with missing data: ", df$core[!(complete)])
    df <- df[complete, ]
  }

  # Check that internal distance is greater than or equal to external distance
  if (any(df[[internal_distance]] < df[[external_distance]])) {
    stop("Internal distance cannot be smaller than external distance")
  }


  # Create working copies of columns
  df$core_r <- df[[core_id]]
  df$sampler_length_r <- df[[sampler_length]]
  df$internal_distance_r <- df[[internal_distance]]
  df$external_distance_r <- df[[external_distance]]

  # Calculate compaction correction factor
  compaction_correction_factor <-
    (df$sampler_length_r - df$internal_distance_r) /
    (df$sampler_length_r - df$external_distance_r)

  # Calculate compaction rate as percentage
  df$compaction <- (1 - compaction_correction_factor) * 100

  if (any(df$compaction < 0 | df$compaction > 100)) {
    stop("Estimated compaction below 0 or above 100: please check cores ",
         df$core[df$compaction < 0 | df$compaction > 100])
  }

  df <- subset(df, select = -c(core_r, sampler_length_r, internal_distance_r, external_distance_r))

  return(df)
}
