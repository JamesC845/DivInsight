#' Create Individual Occurrence Map
#'
#' Create interactive map using individual occurrence coordinates.
#'
#' @import leaflet
#' @import dplyr
#'
#' @param taxon_dataframe A dataframe with occurrence data for the chosen taxon.
#' @param site_name An optional label to add to the coordinate circles.
#' @param colour The colour of the coordinate circles.
#'
#' @return A Viewer object: an interactive map.
#' @export
#'
#' @examples # create an interactive map using coordinates from a taxon dataframe
#' dataframe_Lepto <- subset(Andes, family == "Leptodactylidae")
#'
#' map <- mapStart_indivCirc(dataframe_Lepto, site_name = "Whole Dataset")
#'
#' map
mapStart_indivCirc <- function(taxon_dataframe, site_name = "", colour = "black"){

  taxon_dataframe <- as.data.frame(taxon_dataframe)

  new_map <- leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      lng = taxon_dataframe$decimalLongitude,
      lat = taxon_dataframe$decimalLatitude,
      label = site_name,
      color = colour,
      radius = 4,
      weight = 1,
      opacity = 1,
      fillOpacity = 1
    )

  new_map <- addLegend(
    map = new_map,
    position = "bottomright",
    labels = site_name,
    colors = colour
  )

  return(new_map)

}
