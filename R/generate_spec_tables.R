#' Generate Species Tables
#'
#' Generate a list of tables for species counts from the input clusterised object.
#'
#' @param clusterised_object An object created by the clusterise_sites function.
#' @param min_individuals The minimum number of individuals in each table
#' @param min_species The minimum number of species in each table.
#'
#' @return A list of species counts tables.
#' @export
#'
#' @examples # generate species count tables using a clusterised object.
#'\dontrun{SCM_Santander_1 <- generate_speccomm(
#'
#'  clusterised_object = Colombia_Santander_1,
#'
#'  min_individuals = 30,
#'  min_species = 5
#'
#')}
generate_spec_tables <- function(

  clusterised_object,
  min_individuals = 0,
  min_species = 0

){

  recombined_dataframe <- do.call(rbind, clusterised_object[[1]])

  split_dataframe <- split(recombined_dataframe, list(recombined_dataframe$site_group, recombined_dataframe$date))

  species_tables <- lapply(split_dataframe, function(df) table(df$scientificName))

  NIndividuals <- numeric()
  for(i in 1:length(species_tables)){

    NIndividuals[i] <- sum(species_tables[[i]])

  }

  species_tables <- species_tables[NIndividuals > min_individuals]

  NSpecies <- numeric()
  for(i in 1:length(species_tables)){

    NSpecies[i] <- base::nrow(species_tables[[i]])

  }

  species_tables <- species_tables[NSpecies > min_species]

}
