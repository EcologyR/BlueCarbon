#' Estimation of the thickness of the sample
#'
#' @description checks for space between samples and, if any, divide this space between the previous and
#' next sample to return sample thickness withouth gaps in the core
#'
#' @param df A [data.frame] with with columns core, mind (minimum depth of the sample)and maxd (maximum depth of the sample)
#' @param core Character Name of the column reporting core ID.
#' @param mind Character Name of the column reporting the minimum depth of each sample.
#' @param maxd Character Name of the column reporting the maximum depth of each sample.
#'
#' @return the initial [data.frame] with three additional columns: emin (estimated minimum depth of the sample), emax (estimated maximum depth of the sample) and h (estimated thikness of the sample)
#' @export
#'
#' @examples estimate_h(A)

estimate_h <- function(df = NULL,
                       core = "core",
                       mind = "mind",
                       maxd = "maxd") {

  # class of the dataframe or tibble
  if (!inherits(df, "data.frame")) {
    stop("The data provided must be a tibble or data.frame")
  }

  # name of the columns
  check_column_in_df(df, core)
  check_column_in_df(df, mind)
  check_column_in_df(df, maxd)


  # class of the columns
  if (!is.numeric(df[[mind]])) {stop("'mind' data must be class numeric")}
  if (!is.numeric(df[[maxd]])) {stop("'maxd' data must be class numeric")}

  #check for NAs in depth columns
  if (sum(is.na(df[[mind]])) > 0) {stop("Samples minimun depth column has NAs, please check")}
  if (sum(is.na(df[[maxd]])) > 0) {stop("Samples maximun depth column has NAs, please check")}

  # create variables with working names with the data in the columns specified by the user
  df_r <- df
  df_r$core_r <- df_r[[core]]
  df_r$mind_r <- df_r[[mind]]
  df_r$maxd_r <- df_r[[maxd]]

  # create individual data frames per each core
  df_r$core_r <- factor(df_r$core_r, levels = unique(df_r$core_r))
  x <- split(df_r, df_r$core_r)

  list_h <- lapply(X = x, FUN = estimate_height)

  df_h <- do.call(rbind, list_h)

  rownames(df_h) <- NULL

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
  colnames(data) <- colnames(df_r)

  if (is.unsorted(df$mind_r)) {stop("Samples must be ordered from shallow to deep")}

  data <- estimate_depth(df = data, j = 1:(nrow(data) - 1))
  data$h <- data$emax - data$emin
  return(data)
}
