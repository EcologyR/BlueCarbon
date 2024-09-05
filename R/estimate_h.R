#' Estimation of the thickness of the sample
#'
#' @description checks for space between samples and, if any, divide this space between the previous and
#' next sample to return sample thickness without gaps in the core
#'
#' @param df A data.frame with columns core, mind (minimum depth of the sample) and maxd (maximum depth of the sample)
#' @param core Character Name of the column reporting core ID.
#' @param mind Character Name of the column reporting the minimum depth of each sample.
#' @param maxd Character Name of the column reporting the maximum depth of each sample.
#'
#' @return the initial data.frame with three additional columns:
#' - emin (estimated minimum depth of the sample)
#' - emax (estimated maximum depth of the sample)
#' - h (estimated thickness of the sample)
#'
#' @export
#'
#' @examples
#' bluecarbon_decompact <- decompact(bluecarbon_data)
#' out <- estimate_h(bluecarbon_decompact)

estimate_h <- function(df = NULL,
                       core = "core",
                       mind = "mind_corrected",
                       maxd = "maxd_corrected") {

  # class of the dataframe or tibble
  if (!inherits(df, "data.frame")) {
    stop("The data provided must be a tibble or data.frame")
  }

  # name of the columns
  check_column_in_df(df, core)
  check_column_in_df(df, mind)
  check_column_in_df(df, maxd)


  # class of the columns
  if (!is.numeric(df[[mind]])) {stop("Column 'mind' must be class numeric")}
  if (!is.numeric(df[[maxd]])) {stop("Column 'maxd' must be class numeric")}

  #check for NAs in depth columns
  if (sum(is.na(df[[mind]])) > 0) {stop("Samples minimum depth column has NAs")}
  if (sum(is.na(df[[maxd]])) > 0) {stop("Samples maximum depth column has NAs")}

  # create variables with working names with the data in the columns specified by the user
  df$core_r <- df[[core]]
  df$mind_r <- df[[mind]]
  df$maxd_r <- df[[maxd]]

  # create individual data frames per each core
  df$core_r <- factor(df$core_r, levels = unique(df$core_r))
  x <- split(df, df$core_r)

  list_h <- lapply(X = x, FUN = estimate_height)

  df_h <- do.call(rbind, list_h)

  rownames(df_h) <- NULL

  df_h <- subset(df_h, select = -c(mind_r, maxd_r))

  return(df_h)

}


# estimate depth --------------------------------------------------------------

estimate_depth <- function(df, j) {
  df[j + 1, "emin"] <- df[j, "maxd_r"] + ((df[j + 1, "mind_r"] - df[j, "maxd_r"]) / 2)
  df[j, "emax"] <- df[j, "maxd_r"] + ((df[j + 1, "mind_r"] - df[j, "maxd_r"]) / 2)
  df[1, "emin"] <- 0
  df[nrow(df), "emax"] <- df[nrow(df), "maxd_r"]
  return(df)
}

# estimate height --------------------------------------------------------------

estimate_height <- function(df) {
  data <- as.data.frame(df)
  colnames(data) <- colnames(df)

  if (is.unsorted(df$mind_r)) {stop("Samples must be ordered from shallow to deep")}

  data <- estimate_depth(df = data, j = 1:(nrow(data) - 1))
  data$h <- data$emax - data$emin
  return(data)
}

