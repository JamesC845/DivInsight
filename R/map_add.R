#' Add to Site Map
#'
#' Add coordinate pins to an existing interactive map.
#'
#' @import leaflet
#'
#' @param existing_map A map object that has been created using the map_start function.
#' @param clusterised_object An object created by the clusterise_sites functions.
#' @param site_name An optional label to add the the coordinate pins.
#' @param colour The colour of the coordinate pins.
#'
#' @return Coordinate pins to be added to an existing interactive map object.
#' @export
#'
#' @examples # add coordinate pins to an existing map object
#'\dontrun{Colombia_Meta_map <-
#'
#'  map_add(
#'    existing_map = Colombia_Meta_map,
#'
#'    clusterised_object = Colombia_Meta_5,
#'    site_name = "Meta#5",
#'    colour = "purple"
#'  )}
map_add <- function(

  existing_map,

  clusterised_object,
  site_name = "",
  colour = "#09f"){

  existing_map <- existing_map %>%
    addCircleMarkers(
      lat  = clusterised_object[[2]]$latitude,
      lng  = clusterised_object[[2]]$longitude,
      popup = site_name,
      color = colour
    )


  return(existing_map)


}
