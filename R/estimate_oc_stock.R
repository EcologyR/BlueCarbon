#' Organic Carbon Stock estimation
#'
#' @description Estimates carbon stocks from soil core data down to a specified depth, 100 cm by default.
#' If the core does not reach the standardized depth, it extrapolates the stock from a linear model between accumulated mass of organic carbon and depth.
#'
#' @param df A [data.frame] with core (core id), mind (minimum depth of the sample), maxd (maximum depth of the sample),
#' dbd (dry bulk density), oc (organic carbon %)
#' @param depth mas depth to estimate the stock, by default 100.
#' @param core Character Name of the column reporting core ID.
#' @param mind Character Name of the column reporting the minimum depth of each sample.
#' @param maxd Character Name of the column reporting the maximum depth of each sample.
#' @param dbd Character Name of the column reporting dry bulk density.
#' @param oc Character Name of the column reporting organic carbon concentrations.
#'
#' @return [data.frame] with columns core, swc (organic carbon stock at the whole core), maxd (maximum depth of the core), and stock (organic carbon stock at the standardized depth)
#' @export
#'
#' @examples estimate_oc_stock(A)
#' @examples estimate_oc_stock(A, depth = 50)

estimate_oc_stock <- function(df = NULL,
                           depth = 100,
                           core = "core",
                           mind = "mind_corrected",
                           maxd = "maxd_corrected",
                           dbd = "dbd",
                           oc = "eoc") {

  # class of the dataframe or tibble
  if (!inherits(df, "data.frame")) {
    stop("The data provided must be a tibble or data.frame")
  }

  # name of the columns
  check_column_in_df(df, core)
  check_column_in_df(df, mind)
  check_column_in_df(df, maxd)
  check_column_in_df(df, dbd)
  check_column_in_df(df, oc)


  # class of the columns
  if (!is.numeric(depth)) {stop("'depth' must be class numeric")}
  if (!is.numeric(df[[mind]])) {stop("'mind' must be class numeric")}
  if (!is.numeric(df[[maxd]])) {stop("'maxd' must be class numeric")}
  if (!is.numeric(df[[dbd]])) {stop("'dbd' must be class numeric")}
  if (!is.numeric(df[[oc]])) {stop("'oc' must be class numeric")}

  # create variables with working names with the data in the columns specified by the user
  df_r <- df
  df_r$core_r <- df_r[[core]]
  df_r$mind_r <- df_r[[mind]]
  df_r$maxd_r <- df_r[[maxd]]
  df_r$dbd_r <- df_r[[dbd]]
  df_r$oc_r <- df_r[[oc]]

  df_r <- df_r[!is.na(df_r$oc_r), ]

  # estimate the thickness of the sample
  df_h <- estimate_h(df = df_r,
                     core = "core_r",
                     mind = "mind_r",
                     maxd = "maxd_r")

  # estimate stocks
  x <- split(df_h, df_h$core_r)
  BCS_l <- lapply(X = x,  estimate_core, depth = depth) # return a list
  BCS <- as.data.frame(do.call(rbind, BCS_l)) # from list to dataframe

  rownames(BCS) <- NULL

  return(BCS)
}

estimate_core <- function(df, depth) {

  core <- as.character(df$core_r[[1]])

  if (is.unsorted(df$mind_r)) {stop("Samples must be ordered from shallow to deep")}

    #estimation of carbon g cm2 per sample, OCgcm2= carbon density (g cm3) by thickness (h)
    df$ocgcm2 <- df$dbd_r * (df$oc_r / 100) * df$h

    #estimation of the OC stock in the whole core
    stockwc <- sum(df$ocgcm2)
    maxd <- max(df$emax)

    #if core exactly the standarization depth, we keep the stock of the whole core
    if(max(df$emax) == depth) {

      stock <- sum(df$ocgcm2)
      stock_se <- NA

    } else {

      # if the core longer than the standardization depth we estimate the stock until that depth
      if (max(df$emax) >= depth) {

        df <- df[c(1:(length(which(df$emax <= depth)) + 1)), ]

          stock <- (sum(df[c(1:(nrow(df) - 1)), "ocgcm2"])) +
            ((df[nrow(df), "ocgcm2"] / (max(df$emax) - df[(nrow(df) - 1), "emax"]))
             * (depth - df[(nrow(df) - 1), "emax"]))
          stock_se <- NA


      } else { #if core shorter than than the standardization depth we model the OC acumulated mass with depth and predict the stock at that depth

        df$ocm <- cumsum(df$ocgcm2)
        model <- lm(ocm ~ emax, data = df)
        stock <- predict(model, newdata = data.frame(emax = depth))
        stock_se <- predict(model, newdata = data.frame(emax = depth), se.fit = TRUE)$se.fit

      }}

  BCS <- data.frame(core = core, stockwc = stockwc, maxd = maxd, stock = stock, stock_se = stock_se)

  return(BCS)
}

