#' Add to Individual Occurrence Map
#'
#' Add coordinate circles to an existing interactive map.
#'
#' @import leaflet
#' @import dplyr
#'
#' @param existing_map A map object that has been created using the map_start function.
#' @param clusterised_object An object created by the clusterise_sites functions.
#' @param site_name An optional label to add the the coordinate pins.
#' @param colour The colour of the coordinate pins.
#'
#' @return Coordinate pins to be added to an existing interactive map object.
#' @export
#'
#' @examples # add site coordinates to an existing map
#' dataframe_Lepto <- subset(Andes, family == "Leptodactylidae")
#'
#' map <- mapStart_indivCirc(dataframe_Lepto, site_name = "Whole Dataset")
#'
#' dataframe_Lepto_split <- split(dataframe_Lepto, dataframe_Lepto$year)
#'
#' map <- mapAdd_indivCirc(map, dataframe_Lepto_split$`2011`, "2011", "red")
#' map <- mapAdd_indivCirc(map, dataframe_Lepto_split$`2016`, "2016", "purple")
#' map <- mapAdd_indivCirc(map, dataframe_Lepto_split$`2021`, "2021", "green")
#'
#' map
mapAdd_indivCirc <- function (existing_map, taxon_dataframe, site_name = "", colour = "#60f"){
  existing_map <- addCircleMarkers(

    map = existing_map,

    lng = taxon_dataframe$decimalLongitude,
    lat = taxon_dataframe$decimalLatitude,
    label = site_name,
    color = colour,
    radius = 4,
    weight = 1,
    opacity = 1,
    fillOpacity = 1
  )

  existing_map <- addLegend(
    map = existing_map,
    position = "bottomright",
    labels = site_name,
    colors = colour
  )

  return(existing_map)

}
