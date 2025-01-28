#' Calculate sediment properties after decompaction
#'
#' @description
#' Accepts a data.frame with sample properties and compaction estimations and
#' returns a modified version with sample properties corrected for
#' compaction
#'
#'
#' @param df Data.frame with core properties
#' @param core Character Name of the column with the id of the core to which
#' the sample belongs
#' @param compaction Character Name of the column with core compaction
#' IN PERCENTAGE, as calculated with [estimate_compaction()].
#' @param mind Character Name of the column with minimum depth of the sample
#' (depth at the top of the sample)
#' @param maxd Character Name of the column with maximum depth of the sample
#' (depth at the bottom of the sample)
#' @param dbd Character Name of the column with dry bulk density
#'
#' @return The initial data.frame with the addition of two columns with the
#' corrected minimum and maximum depth of the samples (additionally, if a
#' dry bulk density column is specified, it will return another column with
#' corrected dry bulk density)
#'
#' @examples
#' decompact(bluecarbon_data)
#'
#' @export

decompact <- function(df   = NULL,
                      core         = "core",
                      compaction  = "compaction",
                      mind         = "mind",
                      maxd         = "maxd",
                      dbd          = NULL) {



  # class of the dataframe or tibble
  if (!inherits(df, "data.frame")) {
    stop("The data provided must be a tibble or data.frame")
  }

  # name of the columns
  check_column_in_df(df, core)
  check_column_in_df(df, compaction)
  check_column_in_df(df, mind)
  check_column_in_df(df, maxd)


  # class of the columns
  if (!is.numeric(df[[compaction]])) {stop("Column Compaction must be class numeric")}
  if (!is.numeric(df[[mind]])) {stop("Column mind must be class numeric")}
  if (!is.numeric(df[[maxd]])) {stop("Column maxd must be class numeric")}
  if (!is.null(dbd)) {
    if (!is.numeric(df[[dbd]])) {
      stop("Column must be class numeric")
    }
  }

  # create variables with working names with the data in the columns specified by the user
  df_r <- df
  df_r$core_r <- df_r[[core]]
  df_r$compaction_r <- df_r[[compaction]]
  df_r$mind_r <- df_r[[mind]]
  df_r$maxd_r <- df_r[[maxd]]
  if (!is.null(dbd)) df_r$dbd_r <- df_r[[dbd]]

  # check if compaction values are in percentage - largest compaction value
  # should be bigger than 1, but we should also have non-zero values (all zeros means no compaction)
  if (all(is.na(df_r$compaction_r))) {
    stop("All compaction values are NA")
  }

  ## Setting NA compaction as 0
  if (any(is.na(df_r$compaction_r))) {
    cores_list <- unique(subset(df_r, is.na(df_r$compaction_r))[,"core_r"])
    warning("Setting compaction = 0 for these cores: ",
            paste(cores_list, collapse = ", "))
    df_r$compaction_r[is.na(df_r$compaction_r)] <- 0
  }


  if (any(df_r$compaction_r < 0 | df_r$compaction_r > 100)) {
    stop("Estimated compaction below 0 or above 100: please, check cores ",
         df_r$core_r[df_r$compaction_r < 0 | df_r$compaction_r > 100])
  }

  if (max(df_r$compaction_r, na.rm = TRUE) <= 1) {
    warning("compaction values should be provided in %")
  }


  # apply decompaction
  df_r <- df_r |>
    dplyr::mutate(
      mind_corrected = mind_r/(1-(compaction_r/100)),
      maxd_corrected = maxd_r/(1-(compaction_r/100))
    )

  if (!is.null(dbd)) {
    df_r <- df_r |> dplyr::mutate (dbd_corrected = dbd_r * (1-(compaction_r/100)))
  }

  # add corrected data to original data.frame to return to user
  df$mind_corrected <- df_r$mind_corrected
  df$maxd_corrected <- df_r$maxd_corrected
  # df$compaction_corrected <- df_r$compaction_r
  if (!is.null(dbd)) {
    df$dbd_corrected <- df_r$dbd_corrected
  }


  return(df)

}

