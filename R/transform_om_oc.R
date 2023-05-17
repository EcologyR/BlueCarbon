#' Estimate organic carbon
#'
#' Estimate organic carbon from organic matter values
#'
#' @details
#' Estimation of organic Carbon is done by means of linear regressions on
#' log(organic carbon) ~ log(organic matter)
#' # TODO
#'
#'
#' @param df A tibble or data.frame containing all the data
#' @param ecosystem name of the column with ecosystem of the sample
#' @param site name of the column with the information of sample site
#' @param species name of the column with main species of the sample
#' @param om name of the column with organic matter data
#' @param oc name of the column with organic carbon data
#'
#' @return The initial tibble or data.frame with three new columns:
#' - one column with estimated organic carbon values (eOC)
#' - the standard error of the prediction (eOC_se)
#' - the type of model used for estimation (origin)
#' In addition, a plot with the relationship between organic matter and estimated
#' organic carbon
#'
#' @export
#'
#'
#' @examples
#' transform_om_oc(bluecarbon_data)

transform_om_oc <- function(df = NULL,
                           site = "site",
                           ecosystem = "ecosystem",
                           species = "species",
                           om = "om",
                           oc = "oc") {

  if (!inherits(df, "data.frame")) {
    stop("The data provided must be a tibble or data.frame")
  }

  # Check provided column names exist in df
  check_column_in_df(df, site)
  check_column_in_df(df, ecosystem)
  check_column_in_df(df, species)
  check_column_in_df(df, om)
  check_column_in_df(df, oc)


  # class of the columns
  if (!is.numeric(df[[om]])) {stop("Organic matter data must be class numeric")}
  if (!is.numeric(df[[oc]])) {stop("Organic carbon data must be class numeric")}

  # check there are no negative values
  if (min(df[[oc]], na.rm = T) < 0) {stop("Organic carbon values must be positive")}

  #check if om values are higher than oc values (can not estimate logarithms)
  if (any(df[[om]] < df[[oc]])) {
    stop("Some organic carbon values are higher than organic matter values. Please check your data.")
  }


  # create variables to be used internally
  df$ecosystem_r <- df[[ecosystem]]
  df$species_r <- df[[species]]
  df$site_r <- df[[site]]
  df$om_r <- df[[om]]
  df$oc_r <- df[[oc]]


  ecosystem_ls <- split(df, df$ecosystem_r)


  # part 1: fit models --------------------------------------------------------------

  # estimate df linear model to predict OC from OM for each ecosystem, species and station
  fit_models <- function(df = NULL) {

    df <- df[!is.na(df$oc_r), ]
    df <- df[!is.na(df$om_r), ]

    # if there are 0 in the data we add 0.00001 or the logarithm will not work
    if (0 %in% df$om_r) {df$om_r <- df$om_r + 0.00001}
    if (0 %in% df$oc_r) {df$oc_r <- df$oc_r + 0.00001}

    if (nrow(df) > 10){

      # ecosystem model --------------------------------------------------------------
      # function to estimate a model with all samples from that ecosystem, the full model
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

  # choose_model select the best model from the all_models list for that sample
  # preference order: site specific model with r2 higher than 0.5 > specie specific model wth r2 higher than 0.5 >
  # ecosystem model
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


  # part 2: predict oc --------------------------------------------------------------

  # created a new column with organic carbon data from om data per sample by different methods
  # laboratory oc data > model chosen by choose_model function > models from bibliography (Howard et al. 2014)
  # it is possible not to apply any method (return NA)
  # IMPORTANT to apply models from the bibliography ecosystem names should be "Salt Marsh", "Seagrass" or "Mangrove"

  predict_oc <- function(df) {

    mod_predictions <- lapply(1:nrow(df), function(j) {

      if (!is.na(df[j, "oc_r"])) {
        eoc_val <- df[j, "oc_r"]
        origin <- "Laboratory Data"
        eoc_se_val <- NA

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
              eoc_se_val <- NA
            } else if (df[j, "ecosystem_r"] == "Seagrass" & df[j, "om_r"] < 20) {
              eoc_val <- -0.21 + 0.4 * df[j, "om_r"]
              origin <- "Fourqurean et al. 2012"
              eoc_se_val <- NA
            } else if (df[j, "ecosystem_r"] == "Seagrass" & df[j, "om_r"] > 20) {
              eoc_val <- -0.33 + 0.43 * df[j, "om_r"]
              origin <- "Fourqurean et al. 2012"
              eoc_se_val <- NA
            } else if (df[j, "ecosystem_r"] == "Mangrove") {
              eoc_val <- 2.89 + 0.415 * df[j, "om_r"]
              origin <- "Kaufmann et al. 2011"
              eoc_se_val <- NA
            } else {
              eoc_val <- NA
              eoc_se_val <- NA
              origin <- NA
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

  df_p <- df_f[!is.na(df_f$origin), ]

  plot <- ggplot(df_p, aes(om_r, eoc)) +
    geom_point(aes(color = origin)) +
    coord_cartesian(
      xlim = c(0, max(na.omit(df_p$om_r))),
      ylim = c(0, max(na.omit(df_p$om_r))),
      clip = "on") +
    scale_color_discrete(name="") +
    geom_abline() +
    labs(x = "Organic matter (%)", y = "Estimated organic carbon (%)") +
    theme(plot.title = element_text(hjust = 0.5))

  print(plot)

  return(df_f)

}

