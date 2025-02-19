#' Dataframe containing geochemical data from blue carbon soil cores
#'
#'@description
#' Example dataset of geochemical data from seagrass, salt marsh and mangroves
#' soil cores. It does not contain real data. All variables needed to estimate
#' organic carbon stocks and fluxes are included. Different cores are provided,
#' one below the other, and samples from the same core are grouped together and
#' ordered from the sample closer to the soil surface to the deepest.
#'
#' There are four ID columns: 'site', 'core', 'ecosystem' and 'species'.
#' 'site' refers to the ID given to the sampling location of the core.
#' 'core' is the Id of each individual core (replicate cores would have the same
#' site IDs but different cores IDs).
#' 'ecosystem' refers to the type of ecosystem where the core was collected.
#' 'species' identifies the type of vegetation in the sampling area.
#' Species could either be the name of the species or a code referring to
#' a mixture of different species (e.g. "Low", meaning the dominant species
#' usually found at low marsh areas).
#'
#' 'compaction' indicates the percentage of compression suffered by the core
#' during retrieval. Minimum and maximum depths ('mind' and 'maxd') are the depth of
#' the top and bottom of each sample in the core respectively. Dry bulk density
#' ('dbd') is the dry weight of each sample divided by the volume occupied in the
#' core (before compression correction). 'om' and 'oc' are the percentage of organic
#' matter and organic carbon in the sample. 'age' refers to the age of the sample
#' where the top of the core has age 0. Ages of the samples are estimated from
#' age-depth models.
#'
#'
#' @format ## `bluecarbon_data`
#' A data frame with 1719 rows and 11 columns:
#' \describe{
#'   \item{site}{character column with the id of the sampling locatios}
#'   \item{core}{character column with the id of the core}
#'   \item{ecosystem}{character column with the name of the ecosystem,
#'   "seagrass", "salt marsh" or "mangrove"}
#'   \item{species}{character column with the main species or ecosystem type}
#'   \item{compaction}{numbers between 0 and 100}
#'   \item{mind}{number column with the upper depth of each sample}
#'   \item{maxd}{number column with the lower depth of each sample}
#'   \item{dbd}{number column with dry bulk density values, mass/volumen}
#'   \item{om}{number column with percentage of organic matter, numbers between 0 and 100}
#'   \item{oc}{number column with percentage of organic carbon, numbers between 0 and 100}
#'   \item{age}{number column with age of the sample, years from sampling, positive numbers}
#' }
#' @source data from cores collected by the authors that have been anonymized
#' and modified to cover different cases that may occur when using the package.
#' It no longer represents any existing dataset.
"bluecarbon_data"


#' Dataframe containing field measurements of soil cores to estimate compaction caused
#' by core collection.
#'
#' Example dataset of field measurements of cores collected by percussion.
#' All variables needed to estimate core compaction are included.
#' The 'core' ID is the same as in bluecarbon_data. 'sampler_length' is the length
#' of the tube or sampler introduced in the soil from top to bottom.
#' 'internal_distance' refers to the distance between the top of the sampler and the
#' surface of the soil within the sampler (the same depth or lower than the
#' surface of the soil outside). 'external_distance' refers to the distance from
#' the top of the sampler to the surface of the soil outside the sampler. The
#' reference used as "top of the sampler" is the same in the three measurements.
#' Internal and external distances are measured after sampler insertion in the
#' soil and before its extraction.
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
"core_comp"
