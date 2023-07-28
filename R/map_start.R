#' Create Site Map
#'
#' Create interactive map using centred site coordinates.
#'
#' @import leaflet
#'
#' @param clusterised_object An object created by the clusterise_sites function.
#' @param site_name An optional label to add to the coordinate pins.
#' @param colour The colour of the coordinate pins.
#'
#' @return A Viewer object: an interactive map.
#' @export
#'
#' @examples # create a new map using coordinates from site 1 in the Meta province of Colombia
#'\dontrun{Colombia_Meta_map <-
#'
#'  map_start(
#'    clusterised_object = Colombia_Meta_1,
#'    site_name = "Meta#1",
#'    colour = "green"
#'  )}
map_start <- function(

  clusterised_object,
  site_name = "",
  colour = "#60f"){

  map <- leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      lat   = clusterised_object[[2]]$latitude,
      lng   = clusterised_object[[2]]$longitude,
      popup = site_name,
      color = colour
    )

  return(map)

}
