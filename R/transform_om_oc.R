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

load("data/DataInv.rda")

# part 1: split --------------------------------------------------------------
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
  if (min(df[[oc]], na.rm = T) < 0) {stop("Organic carbon values must be positive")}

  #check if om values are higher than oc values
  if (!is.na(any(df[[om]] < df[[oc]])) == TRUE) {stop("There are organic carbon values higher than organic matter values")}

  df_r <- df
  df_r$site_r <- df_r[[site]]
  df_r$species_r <- df_r[[species]]
  df_r$ecosystem_r <- df_r[[ecosystem]]
  df_r$om_r <- df_r[[om]]
  df_r$oc_r <- df_r[[oc]]

  return(
    list(
      dr_r_split = split(df_r, df_r$ecosystem_r),
      df_r)
  )

}

split_ecosystem_result <- split_ecosystem(
  df = DataInv,
  site = "SiteID",
  ecosystem = "Ecosystem",
  species = "Specie",
  om = "OM",
  oc = "OC"
)


ecosystem_ls <- split_ecosystem_result[[1]]

df_r <- split_ecosystem_result[[2]]


# part 2: fit models --------------------------------------------------------------

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
      if (length(unique(x$site_r)) > 1) {
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

choose_model <- function(df) {

  if (is.null(all_models[[df$ecosystem_r]])) {

    mod <- NULL
    mod_type <- NULL

  } else { #check if there are model for that ecosystem

    mod_site <- all_models[[df$ecosystem_r]][["site_models"]][[df$species_r]]

    if (!is.null(mod_site) && summary(mod_site)$r.squared > 0.5 && df$site_r %in% mod_site$xlevels$`site_r`) { # if there is a model for that specie AND it has that site AND rsqd > 0.5

      mod <- mod_site
      mod_type <- "Model by species and site"

    } else {

      mod_species <- all_models[[df$ecosystem_r]][["multispecies_model"]]

      if (summary(mod_species)$r.squared > 0.5 && df$species_r %in% mod_species$xlevels$`species_r`) {# check if the specie is in the species model and it has a rsqrd >0.5

        mod <- mod_species
        mod_type <- "Model by species"

      } else {

        mod <- all_models[[df$ecosystem_r]][["ecosystem_model"]]
        mod_type <- "Model by ecosystem"

      } # chose specie model, else chose the ecosystem one

    }}

  results_choose_model <- list(mod, mod_type)

  return(results_choose_model)

  }


# part 3: predict oc --------------------------------------------------------------

predict_oc <- function(df) {

  mod_predictions <- lapply(1:nrow(df), function(j) {

    if (!is.na(df[j, "oc_r"])) {
      eoc_val <- df[j, "oc_r"]
      origin <- "Laboratory Data"
      eco_se <- NA

    } else {

      if (is.na(df[j, "om_r"])) {

        eoc_val <- NA
        eoc_se_val <- NA
        origin <- NA

      } else {

        results_choose_models <- choose_model(df_r[j, ])
        mod <- results_choose_models[[1]]
        model_type <- results_choose_models[[2]]

        if (!is.null(mod)) {

          eoc_val <- exp(predict(mod, df[j, ]))
          eoc_se_val <- predict(mod, df[j, ], se.fit = TRUE)$se.fit
          origin <- model_type

        }
        else {

          if (df[j, "ecosystem_r"] == "Salt Marsh") {
            eoc_val <- 0.0025 * (df[j, "om_r"]^2) + 0.4 * df[j, "om_r"]
            origin <- "Craft et al. 1991"
            eco_se <- NA
          } else if (df[j, "ecosystem_r"] == "Seagrass" & df[j, "om_r"] < 20) {
            eoc_val <- -0.21 + 0.4 * df[j, "om_r"]
            origin <- "Fourqurean et al. 2012"
            eco_se <- NA
          } else if (df[j, "ecosystem_r"] == "Seagrass" & df[j, "om_r"] > 20) {
            eoc_val <- -0.33 + 0.43 * df[j, "om_r"]
            origin <- "Fourqurean et al. 2012"
            eco_se <- NA
          } else if (df[j, "ecosystem_r"] == "Mangrove") {
            eoc_val <- 2.89 + 0.415 * df[j, "om_r"]
            origin <- "Kaufmann et al. 2011"
            eco_se <- NA
          }

        }
      }
    }

    data.frame(eoc = eoc_val, eoc_se = eoc_se_val, origin = origin)

  })

  df_f <- cbind(df_r, do.call(rbind, mod_predictions))

  return(df_f)

}

df_f <- predict_oc(df_r)

