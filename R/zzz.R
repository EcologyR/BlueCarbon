check_column_in_df <- function(df = NULL, colname = NULL) {

  if (!colname %in% names(df)) {
    stop("df does not contain a variable called '", colname, "'.")
  }

}
