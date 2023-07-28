#' Convert GBIF Object
#'
#' Convert a GBIF object into workable R dataframe.
#'
#' @import dplyr
#'
#' @param GBIF_object An object queried from the GBIF database: a list containing occurrence data for a specified taxon and location.
#'
#' @return An R dataframe containing the occurrence data from the GBIF object.
#' @export
#'
#' @examples # convert the GBIF object containing occurrence data from 2000 to 2004
#' \dontrun{df_data_Ants_Colombia_2000_to_2004 <- gbif_to_dataframe(
#'  GBIF_data_Ants_Colombia_2000_to_2004
#')}
#' @examples # convert the GBIF object containing occurence data from 2005 to 2008
#' \dontrun{df_data_Ants_Colombia_2005_to_2008 <- gbif_to_dataframe(
#'  GBIF_data_Ants_Colombia_2005_to_2008
#')}
gbif_to_dataframe <- function(GBIF_object){

  GBIF_object <- do.call(bind_rows, lapply(GBIF_object, function(x) x$data))

  return(GBIF_object)

}
