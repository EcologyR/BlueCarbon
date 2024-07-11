#' Estimate organic carbon
#'
#' Estimate organic carbon from organic matter values
#'
#' @details
#' Estimation of organic Carbon is done by means of linear regressions on
#' log(organic carbon) ~ log(organic matter). It gives back a organic carbon
#' value for each organic matter value provided. If there is a organic carbon
#' value for that sample it return the same value, else, generates a model for
#' that site, else, model for specie, else,  model for Ecosystem. If a model
#' can not be created due to the low number of samples (<10) it uses the equations
#' in Howard et al. 2014 to estimate the organic carbon.
#'
#' @param df A tibble or data.frame containing all the data. Must have at least
#' five columns (see arguments below).
#' @param ecosystem Character Name of the column reporting ecosystem type.
#' To apply published equations for OC estimation, ecosystem names should be
#' either "Salt Marsh", "Seagrass" or "Mangrove".
#' @param site Character Name of the column reporting sample site.
#' @param species Character Name of the column reporting the main species in the site.
#' @param om Character Name of the column reporting organic matter values.
#' @param oc Character Name of the column reporting organic carbon values.
#'
#' @return The initial tibble or data.frame with three new columns:
#' - one column with estimated organic carbon values (eOC) in %
#' - the standard error of the prediction (eOC_se)
#' - the type of model used for estimation (origin)
#' In addition, a plot with the relationship between organic matter and estimated
#' organic carbon
#'
#' @import ggplot2
#'
#' @export
#'
#'
#' @examples
#' estimate_oc(bluecarbon_data)
#' estimate_oc(df = A,site = "SiteID", ecosystem = "Ecosystem", species = "Specie", om = "OM", oc = "OC")

estimate_oc <- function(df = NULL,
                        core = "core",
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
  check_column_in_df(df, core)
  check_column_in_df(df, ecosystem)
  check_column_in_df(df, species)
  check_column_in_df(df, om)
  check_column_in_df(df, oc)


  # class of the columns
  if (!is.numeric(df[[om]])) {stop("Organic matter data must be class numeric")}
  if (!is.numeric(df[[oc]])) {stop("Organic carbon data must be class numeric")}

  # check there are no negative values, nor values over 100
  df.nona <- df[complete.cases(df[, c(om, oc)]), ]
  if (any(df.nona[[oc]] < 0 | df.nona[[oc]] > 100)) {
    stop("Organic carbon values must be between 0 and 100 (%)")
  }
  if (any(df.nona[[om]] < 0 | df.nona[[om]] > 100)) {
    stop("Organic matter values must be between 0 and 100 (%)")
  }


  #check if om values are higher than oc values (can not estimate logarithms)
  if (any(df.nona[[om]] < df.nona[[oc]])) {
    stop("Some organic carbon values are higher than organic matter values. Please check your data.")
  }


  # create variables to be used internally
  df$core_r <- df[[core]]
  df$ecosystem_r <- df[[ecosystem]]
  df$species_r <- df[[species]]
  df$site_r <- df[[site]]
  df$om_r <- df[[om]]
  df$oc_r <- df[[oc]]

  # generate working df

  df_r<-df[,c("core_r", "ecosystem_r", "species_r", "site_r", "om_r", "oc_r")]


  ## Split df per ecosystem
  ecosystem_ls <- split(df_r, df$ecosystem_r)


  # fit models & predict --------------------------------------------------------
  all_models <- lapply(ecosystem_ls, fit_models)



  #predict

  df_rows <- split(df_r, 1:nrow(df_r))
  df_pred <- lapply(df_rows, predict_oc, model_list = all_models)
  df_pred <- do.call(rbind.data.frame, df_pred)

  plot_eoc_om(df_pred)

  #warnings

  #less than 10 samples

  if (any(!is.na(df_pred$n)==T)) {

  if (any(min(df_pred$n, na.rm=T)<10)) {
    cores_list<-unique(subset(df_pred, n<10)[,"core_r"])
    warning(
      paste0(
        "The following cores were estimated from models with less than 10 initial samples: ",
        paste(cores_list, collapse = ", ")))}}

  #out of range

  if (!is.na(any(df_pred$eoc<df_pred$min_oc))) {
    cores_list<-unique(subset(df_pred, df_pred$eoc<df_pred$min_oc)[,"core_r"])
    warning(
      paste0(
        "The following cores had samples with organic carbon values below the organic carbo range used to built the model: ",
        paste(cores_list, collapse = ", ")))}

  if (!is.na(any(df_pred$eoc>df_pred$max_oc))) {
    cores_list<-unique(subset(df_pred, df_pred$eoc>df_pred$max_oc)[,"core_r"])
    warning(
      paste0(
        "The following cores had samples with organic carbon values above the organic carbo range used to built the model: ",
        paste(cores_list, collapse = ", ")))}


  # Kaufmann et al. function used with less than 5% of organic matter

  if (any(subset(df_pred, origin == "Kaufmann et al. 2011")[,"om_r"]<5)){

    cores_list<-unique(subset(df_pred, origin == "Kaufmann et al. 2011" & om_r<5)[,"core_r"])
    warning(
      paste0(
        "Kaufmann et al. overstimate organic carbon when organic matter concentrations are below 5%, please check your data from cores:",
        paste(cores_list, collapse = ", ")))}


  #outputs

  df_out <- cbind(subset(df,
                    select = c(-core_r, -ecosystem_r, -species_r, -site_r, -om_r, -oc_r)),
                  df_pred[,c("eoc", "eoc_se", "origin")])


  out<-list (df_out, all_models)
  names(out)<-c("data","models")

  invisible(out)

}



##### MODELS RELATING OM TO OC #####

# ecosystem model --------------------------------------------------------------
# estimate a general model of OC ~ OM with all samples from that ecosystem,
# without distinguishing sites or species

fit_ecosystem_model <- function(df = NULL) {
  lm(log(oc_r) ~ log(om_r), data = df)
}


# multispecies model -----------------------------------------------------------
# estimates a different relationship between OM and OC
# for each species within the ecosystem
fit_multispecies_model <- function(df = NULL) {
  if (length(unique(df$species_r)) > 1) {
    lm(log(oc_r) ~ log(om_r) * species_r, data = df)
  }
}


# site model --------------------------------------------------------------
# estimate a model for a single species
# if there is >1 site, site differences will also be taken into account
fit_site_model <- function(df) {
  if (length(unique(df$site_r)) > 1) {
    lm(log(oc_r) ~ log(om_r) * site_r, data = df)
  } else {
    lm(log(oc_r) ~ log(om_r), data = df)
  }
}



#### FIT MODELS ####

# estimate df linear model to predict OC from OM for each ecosystem, species and station
fit_models <- function(df = NULL) {

  # df must contain certain columns created in estimate_oc
  check_column_in_df(df, colname = "om_r")
  check_column_in_df(df, colname = "oc_r")

  df <- df[!is.na(df$oc_r), ]
  df <- df[!is.na(df$om_r), ]

  # if there are 0 in the data we add 0.001 or the logarithm will not work
  if (any(is.infinite(log(df$om_r)), is.infinite(log(df$oc_r)))) {
    df$om_r <- df$om_r + 0.001
    df$oc_r <- df$oc_r + 0.001
  }


  #if any ecosystem has less than 3 samples with both OM and OC we do not adjust a model for it, nor for species or site
  if (nrow(df) < 3) {

    ecosystem_model <- NULL
    multispecies_model <- NULL
    site_models <- NULL

     } else {

      ecosystem_model <- fit_ecosystem_model(df)

      # check if there is 2 or more specie with more than 3 samples (if not we don't adjust model per species)

      if (length(which(table(df$species_r)>3))<2) {

        multispecies_model <- NULL} else {

          # check if there is any specie with less than 3 samples to adjust model, if so, we delete that specie from the model fitting

          if (length(which(table(df$species_r)<3))>=1) {to_keep_sp<-rownames(as.data.frame(which(table(df$species_r)>=3)))

                      df<-subset(df, df$species_r %in% to_keep_sp)}

          multispecies_model <- fit_multispecies_model(df)}

      # check if there is 2 or more sites with more than 3 samples (if not we dont adjust model per sites)

      if (length(which(table(df$site_r)>3))<2) { site_models <- NULL } else {

        if (length(which(table(df$site_r)>3))>=2) {to_keep_st<-rownames(as.data.frame(which(table(df$site_r)>=3)))

        df<-subset(df, df$site_r %in% to_keep_st)}

        # divide the data.frame per species and fit one model per species
        site_ls <- split(df, df$site_r)
        site_models <- lapply(site_ls, fit_site_model)

      }}



    # assemble output list
    output <- list(ecosystem_model, multispecies_model, site_models)
    names(output) <- c("ecosystem_model", "multispecies_model", "site_models")

    output

  }



#### CHOOSE MODEL ####

# preference order:
# 1. site-specific model with r2 higher than 0.5
# 2. species specific model with r2 higher than 0.5
# 3. ecosystem model

choose_model <- function(df_row = NULL, model_list = all_models) {

  # all_models = list containing ecosystem, multispecies, and site models
  # as produced by fit_models

  if (is.null(model_list[[df_row$ecosystem_r]][[1]])) {

    mod <- NULL
    mod_type <- NULL

  }

  if (!is.null(model_list[[df_row$ecosystem_r]][[1]])) { #check if there are models for that ecosystem

    mod_site <- model_list[[df_row$ecosystem_r]][["site_models"]][[df_row$site_r]]

    # if there is a model for that site AND it has that site AND r2 > 0.5
    if (!is.null(mod_site) &&
        summary(mod_site)$r.squared > 0.5 &&
        df_row$site_r %in% mod_site$xlevels$`site_r`) {

      mod <- mod_site
      mod_type <- "Model by site"

    } else {

      if (!is.null(model_list[[df_row$ecosystem_r]][["multispecies_model"]])) {

      mod_species <- model_list[[df_row$ecosystem_r]][["multispecies_model"]]

      # check if the species is in the species model and it has a r2 >0.5
      if (summary(mod_species)$r.squared > 0.5 &&
          df_row$species_r %in% mod_species$xlevels$`species_r`) {

        mod <- mod_species
        mod_type <- "Model by species"}

      } else {

        mod <- model_list[[df_row$ecosystem_r]][["ecosystem_model"]]
        mod_type <- "Model by ecosystem"

      }

    }
    }

  chosen_model <- list(mod, mod_type)

  chosen_model

}




#### PREDICT OC FROM MODELS OR PUBLISHED EQUATIONS ####

predict_oc <- function(df_row = NULL, model_list = all_models) {

  # Set default values (NA)
  # If OM is NA, OC also NA

  eoc <- NA # estimated organic carbon
  eoc_se <- NA # standard error of estimated organic carbon
  origin <- NA # model used for the estimation
  n <- NA # number of samples used to built the model (used in the warnings)
  min_oc <- NA # minimum value of oc used to built the model (used in the warnings)
  max_oc <- NA # maximum value of organic carbon used to built the model (used in the warnings)


  ## If OC has been measured, use that value
  if (!is.na(df_row$oc_r)) {

    eoc <- df_row$oc_r
    eoc_se <- NA
    origin <- "Measured"
    n <- NA
    min_oc <- NA
    max_oc <- NA

  }

  ## If OC is not available, estimate it
  if (is.na(df_row$oc_r)) {

    ## If OM available, use it to estimate OC
    ## Using model if available, published equation otherwise
    if (!is.na(df_row$om_r)) {

      chosen_model <- choose_model(df_row, model_list = model_list)
      mod <- chosen_model[[1]]
      model_type <- chosen_model[[2]]


      ## If there is a model, use it to predict OC from OM
      if (!is.null(mod)) {

        eoc <- exp(predict(mod, df_row))
        eoc_se <- predict(mod, df_row, se.fit = TRUE)$se.fit
        origin <- model_type
        n <- nrow(mod$model)
        min_oc <- exp(min(mod$model[,1]))
        max_oc <- exp(max(mod$model[,1]))

      }

      ## If no model, use published equations

      if (is.null(mod)) {

        if (df_row$ecosystem_r == "Salt Marsh") {
          eoc <- 0.000683 * (df_row$om_r^2) + 0.41 * df_row$om_r
          eoc_se <- NA
          origin <- "Maxwell et al. 2023"
        }

        if (df_row$ecosystem_r == "Seagrass" && df_row$om_r < 20) {
          eoc <- -0.21 + 0.4 * df_row$om_r
          eoc_se <- NA
          origin <- "Fourqurean et al. 2012"
        }

        if (df_row$ecosystem_r == "Seagrass" && df_row$om_r >= 20) {
          eoc <- -0.33 + 0.43 * df_row$om_r
          eoc_se <- NA
          origin <- "Fourqurean et al. 2012"
        }

        if (df_row$ecosystem_r == "Mangrove") {
          eoc <- 2.89 + 0.415 * df_row$om_r
          eoc_se <- NA
          origin <- "Kaufmann et al. 2011"

        }
      }
    }
  }

  df_row_p <- data.frame(df_row, eoc = eoc, eoc_se = eoc_se, n = n, min_oc = min_oc, max_oc = max_oc, origin = origin)

  df_row_p

}


#### PLOT ####

plot_eoc_om <- function(df = NULL) {

  check_column_in_df(df, "eoc")
  check_column_in_df(df, "origin")
  check_column_in_df(df, "om_r")

  df <- df[!is.na(df$origin), ]

  gg <- ggplot2::ggplot(df, ggplot2::aes(om_r, eoc)) +
    ggplot2::geom_point(ggplot2::aes(color = origin)) +
    ggplot2::coord_cartesian(
      xlim = c(0, max(na.omit(df$om_r))),
      ylim = c(0, max(na.omit(df$om_r))),
      clip = "on") +
    ggplot2::scale_color_discrete(name = "") +
    ggplot2::geom_abline() +
    ggplot2::labs(x = "Organic matter (%)", y = "Estimated organic carbon (%)") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  suppressWarnings(print(gg))
}


