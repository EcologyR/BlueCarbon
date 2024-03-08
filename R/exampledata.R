#' bluecarbon_data
#'
#' Example dataset of blue carbon data. It does not contain real data.
#'
#'
#' @format ## `bluecarbon_data`
#' A data frame with 1719 rows and 11 columns:
#' \describe{
#'   \item{site}{character column with the id of the sampling locatios}
#'   \item{core}{character column with the id of the core}
#'   \item{ecosystem}{character column with the name of the ecosystem,
#'   "seagrass", "salt marsh" or "mangrove"}
#'   \item{species}{character column with the main specie or ecosystem type}
#'   \item{compression}{numbers between 0 and 100}
#'   \item{mind}{number column with upper depth of each sample}
#'   \item{maxd}{number column with lower depth of each sample}
#'   \item{dbd}{number column with dry bulk density values, mass/volumen}
#'   \item{om}{number column with percentage of organic matter, numbers between 0 and 100}
#'   \item{oc}{number column with percentage of organic carbon, numbers between 0 and 100}
#'   \item{age}{number column with age of the sample, years from sampling, positive numbers}
#' }
#' @source data from cores collected by the authors that has been anonymized
#' and modified. It does not longer represent any existing dataset.
"bluecarbon_data"


#' core_comp
#'
#' Example dataset of field measurements of cores collected by manual percussion.
#'
#'
#' @format ## `core_comp`
#' A data frame with 78 rows and 4 columns:
#' \describe{
#'   \item{core}{character column with the id of the core}
#'   \item{sampler_length}{numeric column with the length of the sampler/tube
#'   used to extract the core}
#'   \item{internal_distance}{numeric column with the distance between
#'    the top of the sampler/tube and the surface of the sediment inside
#'    the sampler/tube after finishing inserting it in the soil and before
#'    extracting it}
#'   \item{external_distance}{numeric column with the distance between
#'    the top of the sampler/tube and the surface of the sediment outside
#'    the sampler/tube after finishing inserting it in the soil and before
#'    extracting it}

#' }
#' @source made up data
"exampledata"
