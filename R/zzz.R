#' Check if a column is in a data frame and stop if not
#' @noRd
check_column_in_df <- function(df = NULL, colname = NULL) {

  if (!is.character(colname)) {
    stop(colname, " is not a character. Please provide the column name as character.")
  }

  if (!colname %in% names(df)) {
    stop("df does not contain a variable called '", colname, "'.")
  }

}


utils::globalVariables(
  c("all_models", "compaction_r", "core_r", "dbd_r", "ecosystem_r", "eoc",
    "external_distance_r", "h", "internal_distance_r", "maxd_r", "mind_r", "n",
    "oc_r", "ocgcm2", "om_r", "origin", "sampler_length_r", "site_r",
    "species_r", "stock", "stock_25", "stock_50", "stock_75", "stock_90",
    "value", "variable"))
