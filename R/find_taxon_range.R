#' Find Taxon Range
#'
#' Calculate the distances between all individuals within a dataset and return the maximum linear distance.
#'
#' @import geosphere
#' @import utils
#'
#' @param taxon_dataframe A dataset containing occurrence data for the taxon of interest.
#'
#' @return Returns a numeric value representing the maximum distance (metres) between all individuals within the given dataset.
#' @export
#'
#' @examples # find maximum taxon range in different years for the Centrolenidae family
#'Centro_2011 <- subset(Andes, family == "Centrolenidae" & year == 2011)
#'Centro_2016 <- subset(Andes, family == "Centrolenidae" & year == 2016)
#'Centro_2021 <- subset(Andes, family == "Centrolenidae" & year == 2021)
#'
#'find_taxon_range(Centro_2011)
#'find_taxon_range(Centro_2016)
#'find_taxon_range(Centro_2021)
find_taxon_range <- function(taxon_dataframe){

  coordinates <- data.frame(longitude = taxon_dataframe$decimalLongitude,
                            latitude = taxon_dataframe$decimalLatitude)

  maximum_range <-max(
    distHaversine(
      coordinates[combn(nrow(coordinates), 2),]))

  return(maximum_range)

}

