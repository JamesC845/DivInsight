#' Generate Species Tables
#'
#' Generate a list of tables for species counts from the input dataframe.
#'
#' @param dataframe An occurrence dataframe.
#' @param min_individuals The minimum number of individuals in each table
#' @param min_species The minimum number of species in each table.
#'
#' @return A list of species counts tables.
#' @export
#'
#' @examples # generate species count tables for the entire country of Colombia.
#'\dontrun{SCM_Colombia <- generate_speccomm_df(
#'
#'  dataframe = Colombia,
#'
#'  min_individuals = 50,
#'  min_species = 10
#'
#')}
generate_spec_tables_df <- function(

  dataframe,
  min_individuals = 0,
  min_species = 0

){

  dataframe$date <- substr(dataframe$eventDate, 0, 10)
  split_dataframe <- split(dataframe, dataframe$date)
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
