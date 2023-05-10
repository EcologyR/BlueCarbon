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
species = "Species"
om = "OM"
oc = "OM"

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

  # ecosystem model --------------------------------------------------------------

  # create list of dataframes with data from each ecosystem, species, and station (site)
  ecosystem_ls <- split(df, df[[ecosystem]])

  fit_ecosystem_model <- function(x) {
    lm(log(x[["OC"]]) ~ log(x[["OM"]]))
  }

  ecosystem_model_lapply <- lapply(X = ecosystem_ls, FUN = fit_ecosystem_model)

  ecosystem_model_purrr <- purrr::map(ecosystem_ls, \(x) fit_ecosystem_model(x))

  plot(ecosystem_model_lapply[[1]])

  # nested
  ecosystem_mod <- df |>
    dplyr::group_by(Ecosystem) |>
    # genera listas columna con dataframes dentro
    # en este caso para cada especie
    tidyr::nest() |>
    dplyr::mutate(
      # \(dat) = function(dat)
      lm = purrr::map(data, \(dat) lm(
        log(OC) ~ log(OM), data = dat))
    )

# multispecies model -----------------------------------------------------------

  fit_multispecies_model <- function(x) {
    if (length(unique(x[["Specie"]])) > 1) {
      lm(log(x[["OC"]]) ~ log(x[["OM"]]) *
           x[["Specie"]])
    }
  }

  View(ecosystem_ls)

  multispecies_model_lapply <- lapply(X = ecosystem_ls, FUN = fit_species_model)

# site model --------------------------------------------------------------

  split_species <- function(df) {
    species_ls <- split(df, df[[species]])
  }

  species_ls <- lapply(X = ecosystem_ls, FUN = split_species)

  fit_site_model <- function(x) {
    if (length(unique(x[["SiteID"]])) > 1) {
      lm(log(x[["OC"]]) ~ log(x[["OM"]]) *
           x[["SiteID"]])
    } else {
      lm(log(x[["OC"]]) ~ log(x[["OM"]]))
    }
  }

  fit_site_model(species_ls[["Seagrass"]][["Posidonia oceanica"]])

  species_ls_rec <- unlist(species_ls, recursive = F)

  site_model_lapply <- lapply(X = species_ls_rec, FUN = fit_site_model)

  output <- list(
    ecosystem = ecosystem_model_lapply,
    multispecies = multispecies_model_lapply,
    site = site_model_lapply
  )

  return(output)

}

