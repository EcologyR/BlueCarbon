#' Organic Carbon Stock estimation
#'
#' @description Estimates carbon stocks from soil core data down to a specified depth, 100 cm by default.
#' If the core does not reach the standardised depth, it extrapolates the stock from a linear model between accumulated mass of organic carbon and depth.
#'
#' @param df A [data.frame] with core (core id), mind (minimum depth of the sample), maxd (maximum depth of the sample),
#' dbd (dry bulk density), oc (organic carbon %)
#' @param depth standardization soil depth, by default 100 cm.
#' @param min.sample
#' @param core
#' @param mind
#' @param maxd
#' @param dbd
#' @param oc
#'
#' @return [data.frame] with columns core, swc (organic carbon stock at the whole core), maxd (maximum depth of the core), and stock (organic carbon stock at the standardized depth)
#' @export
#'
#' @examples estimate_stock(A)
#' @examples estimate_stock(A, depth = 50)

estimate_stock <- function(df = NULL,
                           depth = 100,
                           min_samples = 3,
                           core = "core",
                           mind = "mind",
                           maxd = "maxd",
                           dbd = "dbd",
                           oc = "eoc") {

  # class of the dataframe or tibble
  if (!inherits(df, "data.frame")) {
    stop("The data provided must be a tibble or data.frame")
  }

  # name of the columns
  if (!core %in% colnames(df)) {stop("There must be a variable with 'core'")}
  if (!mind %in% colnames(df)) {stop("There must be a variable with 'mind'")}
  if (!maxd %in% colnames(df)) {stop("There must be a variable with 'maxd'")}
  if (!dbd %in% colnames(df)) {stop("There must be a variable with 'dbd'")}
  if (!oc %in% colnames(df)) {stop("There must be a variable with 'oc'")}

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
                     mind = "mind_r",
                     maxd = "maxd_r")

  # estimate stocks
  x <- split(df_h, df_h$core_r)
  BCS_l <- lapply(X = x,  estimate_core, depth = depth) # return a list
  BCS <- as.data.frame(do.call(rbind, BCS_l)) # from list to dataframe

  rownames(BCS) <- NULL

  return(BCS)
}

estimate_core <- function(df, depth, min_samples = 3) {

  # esto lo he anadido pero no estoy seguro
  # revisalo porfa
  core <- df$core_r

  if(!nrow(df) < min_samples) {

    #estimation of carbon g cm2 per sample, OCgcm2= carbon density (g cm3) by thickness (h)
    df$ocgcm2 <- df$dbd_r * (df$oc_r / 100) * df$h

    #estimation of the OC stock in the whole core
    swc <- sum(df$ocgcm2)
    maxd <- max(df$emax)

    #if core exactly the standarization depth, we keep the stock of the whole core
    if(max(df$emax) == depth) {

      stock <- sum(df$ocgcm2)
      stock_se <- NA

    } else {

      # if the core longer than the standardization depth we estimate the stock until that depth
      if (max(df$emax) >= depth) {

        df <- df[c(1:(length(which(df$emax <= depth)) + 1)), ]

        if(!nrow(df) < min_samples) {

          stock <- (sum(df[c(1:(nrow(df) - 1)), "ocgcm2"])) +
            ((df[nrow(df), "ocgcm2"] / (max(df$emax) - df[(nrow(df) - 1), "emax"]))
             * (depth - df[(nrow(df) - 1), "emax"]))
          stock_se <- NA

        }

      } else { #if core shorter than than the standardization depth we model the OC acumulated mass with depth and predict the stock at that depth

        df$ocm <- cumsum(df$ocgcm2)
        model <- lm(ocm ~ emax, data = df)
        stock <- predict(model, newdata = data.frame(emax = depth))
        stock_se <- predict(model, newdata = data.frame(emax = depth), se.fit = TRUE)$se.fit

      }}}

  BCS <- data.frame(core = core, swc = swc, maxd = maxd, stock = stock, stock_se = stock_se)

  return(BCS)
}

kk <- estimate_stock (df = df_f,
                      depth = 100,
                      min_samples = 3,
                      core = "core",
                      mind = "mind",
                      maxd = "maxd",
                      dbd = "dbd",
                      oc = "eoc")

# el warning que da de los rownames no es importante creo
# entiendo que no estas interesada en la variable
# que figura en los rownames
# si asi podriamos eliminar el warning o modificar un poco
# el codigo para que no pase eso

df = df_f
depth = 100
min_samples = 3
core = "core"
mind = "mind"
maxd = "maxd"
dbd = "dbd"
oc = "eoc"

estimate_stock(df)

