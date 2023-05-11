#' Organic carbon estimation
#'
#' @description Model linear relation between organic matter and organic carbon and estimate organic carbon values from organic matter data
#'
#' @param df A tibble or data.frame with, at least, columns CoreID, Ecosystem, Species, SiteID, OM, and OC.
#' #### FRS: An alternative is to let the user specify the column names for each of these things
#'
#' @return the initial data.frame + one column with organic carbon values (fOC = final organic carbon)
#' @export
#'
#' @examples

df = "exampledata"
site = "SiteID"
core = "CoreID"
ecosystem = "Ecosystem"
species = "Specie"
om = "OM"
oc = "OC"

transform_om_oc <- function(df = NULL,
                            site = NULL,
                            core = NULL,
                            ecosystem = NULL,
                            species = NULL,
                            om = NULL,
                            oc = NULL) {

  #### Estimate df linear model to predict OC from OM for each ecosystem, species and station ###

  # check if the class of the parameters objects, the names and class of the columns of df are correct

  if (!inherits(df, "data.frame") || !inherits(df, "tibble")) {
    stop("The data provided must be a tibble or data.frame")
  }

  # name of the columns
  if (!site %in% names(df)) {stop("There must be a column named 'SiteID'")}
  if (!core %in% names(df)) {stop("There must be a column named 'CoreID'")}
  if (!ecosystem %in% names(df)) {stop("There must be a column named 'Ecosystem'")}
  if (!species %in% names(df)) {stop("There must be a column named 'Species'")}
  if (!om %in% names(df)) {stop("There must be a column named 'OM'")}
  if (!oc %in% names(df)) {stop("There must be a column named 'OC'")}

  # class of the columns
  if (!is.numeric(df$om)) {stop("Organic matter data must be class numeric")}
  if (!is.numeric(df$oc)) {stop("Organic carbon data must be class numeric")}

  # check there are no negative values

  if (min(df$om)<0) {stop("There are organic matter negative values")}
  if (min(df$oc)<0) {stop("There are organic carbon negative values")}


  # create list of dataframes with data from each ecosystem, species, and station (site)
  ecosystem_ls <- split(DataInv, DataInv[[ecosystem]])



  #for each ecosystem

  fit_ecosystem_models<- function (df) {

    df<-df[!is.na(df[[oc]]),]
    df<-df[!is.na(df[[om]]),]

    # if there are 0 in the data we add 0.00001 or the logaritm will not work
    if (0 %in% df[[om]]) {df[[om]]<-df[[om]]+0.00001}
    if (0 %in% df[[oc]]) {df[[oc]]<-df[[oc]]+0.00001}

    if (nrow(df)>10){


   # ecosystem model --------------------------------------------------------------
  fit_full_model <- function(x) { # function to estimate a model with all samples from that ecosystem, the full model
    lm(log(x[[oc]]) ~ log(x[[om]]))
  }

  ecosystem_model <- fit_full_model (df)

  # multispecies model -----------------------------------------------------------

  fit_multispecies_model <- function(x) {# estimates a different model per specie withing the ecosystem
    if (length(unique(x[[species]])) > 1) {
      lm(log(x[[oc]]) ~ log(x[[om]]) *
           x[[species]])
    }
  }

  multispecies_model <- fit_multispecies_model(df)

# site model --------------------------------------------------------------

  split_species <- function(df) {# divide the data.frame per specie
    species_ls <- split(df, df[[species]])
  }
  species_ls <- split_species (df)

  fit_site_model <- function(x) {# model with data from each specie dependent on sampling site
    if (length(unique(x[[site]])) > 1) {
      lm(log(x[[oc]]) ~ log(x[[om]]) *
           x[[site]])
    } else {
      lm(log(x[[oc]]) ~ log(x[[om]]))
    }
  }

  site_models <- lapply(X = species_ls, FUN = fit_site_model)

  output<-list(ecosystem_model, multispecies_model, site_models)
  names(output)<-c("ecosystem_model", "multispecies_model", "site_models")

   return(output)}

    }

  all_models<-lapply( X = ecosystem_ls, FUN = fit_ecosystem_models)



}


#quiero que choose_model especifique que modelo uso
choose_model <- function (df, models= all_models) {

  if (is.null(models[[df$Ecosystem]])) {mod<-NULL} else { #check if there are model for that ecosystem

  mod_site<-models[[df$Ecosystem]][["site_models"]][[df$Specie]]

  if (!is.null(mod_site) & summary(mod_site)$r.squared > 0.5 & df$SiteID %in% mod_site$xlevels$`x[[site]]`) { # if there is a model for that specie AND it has that site AND rsqd > 0.5

    mod<-mod_site} else {

      mod_specie<-models[[df$Ecosystem]][["multispecies_model"]]

      if (summary(mod_specie)$r.squared > 0.5 & df$Specie %in% mod_specie$xlevels$`x[[species]]`) # check if the specie is in the species model and it has a rsqrd >0.5

      {mod<-mod_specie} else { mod<-models[[df$Ecosystem]][["multispecies_model"]]} # chose specie model, else chose the ecosystem one

      return(mod)
      }}}

mod<-choose_model(df)

#I want and extra column with the type of model or lab or howard
predict_oc <- function (df, models= all_models) {

  eOC<-data.frame(eOC=numeric(),
                  eOC_SE=numeric(),
                  Origin=character())

  for (i in 1:nrow(df)) {

  if (!is.na(df[j, oc])) {
    eOC[j,"eOC"]<-df[j,oc]
    eOC[j,"Origin"]<-"Laboratory Data"} else {

    if (is.na(df[j, om])) {eOC[j,"eOC"]<-NA} else {

      mod<-choose_model(df[j,], models)
       if(!is.null(mod)) {


      #predict sigue sin funcionar

      eOC[j,"eOC"]<-predict(mod)
      eOC[j,"eOC_SE"]<-predict(mod, se.fit = TRUE)$se.fit


      } else { # if there is no model use howard 2014

        if (df[j,ecosystem]=="Salt Marsh") {
          eOC[j,"eOC"]<- 0.0025 * (df[j, om]^2) + 0.4 * df[j, om]
          eOC[j,"Origin"]<-"Craft et al. 1991"}
        if (df[j,ecosystem]=="Seagrass" & df[j, om]<20 ) {
          eOC[j,"eOC"]<- -0.21 + 0.4 * df[j, om]
          eOC[j,"Origin"]<-"Fourqurean et al. 2012"}
        if (df[j,ecosystem]=="Seagrass" & df[j, om]>20 ) {
          eOC[j,"eOC"]<- -0.33 + 0.43 * df[j, om]
          eOC[j,"Origin"]<-"Fourqurean et al. 2012"}
        if (df[j,ecosystem]=="Mangrove") {
          eOC[j,"eOC"]<- 2.89 + 0.415 * df[j, om]
          eOC[j,"Origin"]<-"Kaufmann et al. 2011"}

      }

   }}}}








}
