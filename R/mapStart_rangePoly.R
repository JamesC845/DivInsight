#' Create Taxon Range Map
#'
#' Create interactive map and plot a taxon range polygon.
#'
#' @import leaflet
#' @import sp
#' @import dplyr
#'
#' @param taxon_dataframe A dataframe with occurrence data for the chosen taxon.
#' @param site_name An optional label to add to the coordinate circles.
#' @param colour The colour of the coordinate circles.
#' @param site_legend A boolean value to choose to add a site name legend to the map.
#' @param poly_opacity An optional value to set the opacity of the polygon.
#'
#' @return A Viewer object: an interactive map.
#' @export
#'
#' @examples # create an interactive map using coordinates from a taxon dataframe
#'species_dataframe <- subset(Andes, species == "Boana crepitans")
#'
#'map <- mapStart_rangePoly(species_dataframe, site_name = "Whole Dataset")
#'
#'map
mapStart_rangePoly <- function (taxon_dataframe, site_name = "", colour = "black",
                                site_legend = T, poly_opacity = 0.2)
{
  coordinates <- data.frame(lon = taxon_dataframe$decimalLongitude,
                            lat = taxon_dataframe$decimalLatitude)
  hull_indices <- chull(coordinates$lat, coordinates$lon)
  hull_coordinates <- coordinates[hull_indices, ]
  polygon <- Polygon(hull_coordinates)
  polygon <- Polygons(list(polygon), ID = "1")
  polygon <- SpatialPolygons(list(polygon))
  new_map <- leaflet() %>% addTiles() %>% addPolygons(data = polygon,
                                                      color = colour, weight = 2, opacity = 1, fillOpacity = poly_opacity,
                                                      label = site_name)

  if(site_legend == T){

    new_map <- addLegend(map = new_map, position = "bottomright",
                         labels = site_name, colors = colour)
  }
  return(new_map)
}

