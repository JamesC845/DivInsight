#' Add to Individual Occurrence Map
#'
#' Add range polygons to an existing interactive map.
#'
#' @import leaflet
#' @import sp
#'
#' @param existing_map A map object that has been created using the map_start function.
#' @param clusterised_object An object created by the clusterise_sites functions.
#' @param site_name An optional label to add the the coordinate pins.
#' @param colour The colour of the coordinate pins.
#' @param site_legend A boolean value to choose to add a site name legend to the map.
#' @param poly_opacity An optional value to set the opacity of the polygon.
#'
#' @return Coordinate pins to be added to an existing interactive map object.
#' @export
#'
#' @examples # add site coordinates to an existing map
#'species_dataframe <- subset(Andes, species == "Boana crepitans")
#'
#'map <- mapStart_rangePoly(species_dataframe, site_name = "Whole Dataset")
#'
#'species_dataframe_split <- split(species_dataframe, species_dataframe$year)
#'
#'map <- mapAdd_rangePoly(map, species_dataframe_split$`2011`, "2016", "purple")
#'map <- mapAdd_rangePoly(map, species_dataframe_split$`2013`, "2013", "green")
#'map <- mapAdd_rangePoly(map, species_dataframe_split$`2015`, "2015", "red")
#'map <- mapAdd_rangePoly(map, species_dataframe_split$`2017`, "2017", "blue")
#'
#'map
mapAdd_rangePoly <- function (existing_map, taxon_dataframe, site_name = "", colour = "#60f",
                              site_legend = T, poly_opacity = 0.4)
{
  coordinates <- data.frame(lon = taxon_dataframe$decimalLongitude,
                            lat = taxon_dataframe$decimalLatitude)
  hull_indices <- chull(coordinates$lat, coordinates$lon)
  hull_coordinates <- coordinates[hull_indices, ]
  polygon <- Polygon(hull_coordinates)
  polygon <- Polygons(list(polygon), ID = "1")
  polygon <- SpatialPolygons(list(polygon))
  existing_map <- addPolygons(map = existing_map, data = polygon,
                              color = colour, weight = 2, opacity = 1, fillOpacity = poly_opacity,
                              label = site_name)

  if(site_legend == T){

    existing_map <- addLegend(map = existing_map, position = "bottomright",
                              labels = site_name, colors = colour)

  }

  return(existing_map)

}
