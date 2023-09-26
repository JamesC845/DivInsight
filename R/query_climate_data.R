#' Query Climate Data
#'
#' Query climate data from the Storm Glass datagrid.
#'
#' @import httr
#' @import jsonlite
#'
#' @param stats_dataframe A dataframe created by using the `generate_stats()` function.
#' @param api_key An API key acquired by signing up to `stormglass.io`.
#'
#' @return A nested list containing elevation data and query metadata for the given stats and coordinates.
#' @export
#'
#' @examples # query elevation data using Singapore data clusters
#' \dontrun{
#'
#' stats_df <- generate_stats(Singpore_clst)
#'
#' API <- "a1b2c3d4e5"
#'
#' data_elev <- query_elevation_data(stats_df, API)
#'
#' }
query_climate_data <- function(stats_dataframe, api_key){

  INNERFUNCTION <- function(stats_dataframe, api_key){

    url <- "https://api.stormglass.io/v2/weather/point"

    params <- list(
      lat = stats_dataframe$latitude,
      lng = stats_dataframe$longitude,
      params = "airTemperature,airTemperature80m,airTemperature100m,pressure,cloudCover,humidity,precipitation,windSpeed,windSpeed20m,windSpeed30m,windSpeed40m,windSpeed50m,windSpeed80m,windSpeed100m",

      start = gsub(pattern = " ", replacement = "", x = paste(stats_dataframe$date, "T00:00:00Z")),
      end =   gsub(pattern = " ", replacement = "", x = paste(stats_dataframe$date, "T23:00:00Z"))

    )

    response <- GET(url, query = params, add_headers("Authorization" = api_key))

    json_data <- content(response, "text",  encoding = "UTF-8")
    climate_data <- fromJSON(json_data, simplifyDataFrame = TRUE)

  }

  climate_data_list <- lapply(1:nrow(stats_dataframe), function(row){
    INNERFUNCTION(stats_dataframe[row, ], api_key)
  })

  return(climate_data_list)

}
