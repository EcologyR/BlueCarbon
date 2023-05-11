#' Organic carbon estimation
#'
#' @description Model linear relation between organic matter and organic carbon and estimate organic carbon values from organic matter data
#'
#' @param df A tibble or data.frame with, at least, columns CoreID, Ecosystem, Species, SiteID, OM, and OC.
#'
#' @return the initial data.frame + one column with organic carbon values (fOC = final organic carbon)
#' @export
#'
#' @examples transform_om_oc(A, r_squared = 0.8, p_value = 0.05)
#' @examples transform_om_oc(A)

# function 1: split --------------------------------------------------------------
# create list of dataframes with data from each ecosystem, species, and station (site)
split_ecosystem <- function(df = NULL,
                            site = NULL,
                            ecosystem = NULL,
                            species = NULL,
                            om = NULL,
                            oc = NULL) {

  if (!inherits(df, "data.frame")) {
    stop("The data provided must be a tibble or data.frame")
  }

  # names of the columns
  if (!site %in% names(df)) {stop("There must be a variable with 'site'")}
  if (!ecosystem %in% names(df)) {stop("There must be a variable with 'ecosystem'")}
  if (!species %in% names(df)) {stop("There must be a variable with 'species'")}
  if (!om %in% names(df)) {stop("There must be a variable with 'om'")}
  if (!oc %in% names(df)) {stop("There must be a variable with 'oc'")}

  # class of the columns
  if (!is.numeric(df[[om]])) {stop("Organic matter data must be class numeric")}
  if (!is.numeric(df[[oc]])) {stop("Organic carbon data must be class numeric")}

  # check there are no negative values
  # if (min(df[[om]], na.rm = T) < 0) {stop("There are organic matter negative values")}
  if (min(df[[oc]], na.rm = T) < 0) {stop("Organic carbon values must be positive")}

  df_r <- df
  df_r$site_r <- df_r[[site]]
  df_r$species_r <- df_r[[species]]
  df_r$core_r <- df_r[[core]]
  df_r$ecosystem_r <- df_r[[ecosystem]]
  df_r$om_r <- df_r[[om]]
  df_r$oc_r <- df_r[[oc]]

  return(split(df_r, df_r$ecosystem_r))

}

ecosystem_ls <- split_ecosystem(
  df = DataInv,
  site = "SiteID",
  ecosystem = "Ecosystem",
  species = "Specie",
  om = "OM",
  oc = "OC"
)

# function 2: fit models --------------------------------------------------------------
# estimate df linear model to predict OC from OM for each ecosystem, species and station
fit_models <- function(df = NULL) {

  df <- df[!is.na(df$oc_r), ]
  df <- df[!is.na(df$om_r), ]

  # if there are 0 in the data we add 0.00001 or the logarithm will not work
  if (0 %in% df$om_r) {df$om_r <- df$om_r + 0.00001}
  if (0 %in% df$oc_r) {df$oc_r <- df$oc_r + 0.00001}

  if (nrow(df) > 10){

    # ecosystem model --------------------------------------------------------------
    # function to estimate a model with all samples from_r that ecosystem, the full model
    fit_ecosystem_models <- function(x) {
      lm(log(oc_r) ~ log(om_r), data = x)
    }

    ecosystem_model <- fit_ecosystem_models(x = df)

    # multispecies model -----------------------------------------------------------
    # estimates a different model per specie withing the ecosystem
    fit_multispecies_model <- function(x) {
      if (length(unique(df$species_r)) > 1) {
        lm(log(oc_r) ~ log(om_r) *
             species_r, data = x)
      }
    }

    multispecies_model <- fit_multispecies_model(x = df)

    # site model --------------------------------------------------------------

    # divide the data.frame per specie
    split_species <- function(x) {
      species_ls <- split(x, x$species_r)
    }

    species_ls <- split_species(x = df)

    # model with data from_r each specie dependent on sampling site
    fit_site_model <- function(x) {
      if (length(unique(site_r)) > 1) {
        lm(log(oc_r) ~ log(om_r) *
             site_r, data = x)
      } else {
        lm(log(oc_r) ~ log(om_r), data = x)
      }
    }

    site_models <- lapply(X = species_ls, FUN = fit_site_model)

    output <- list(ecosystem_model, multispecies_model, site_models)

    names(output) <- c("ecosystem_model", "multispecies_model", "site_models")

    return(output)

  }

}

all_models <- lapply(
    X = ecosystem_ls,
    FUN = fit_models
  )

View(all_models)

