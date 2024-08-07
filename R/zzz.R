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
