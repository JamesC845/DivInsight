#' Plant Occurrence Clusters in Singapore (2009-2023)
#'
#' This dataset contains clustered occurrence data for various sites within Singapore. Clusters have a minimum size of 30 observations and are based on a radius of 5000 meters.
#'
#' @format
#' A list containing two elements. The first element is a list containing 41 dataframes (site clusters) of varying lengths. The second is a dataframe where each row has metadata pertaining to each of the 41 dataframes.
#' @source Queried from the Global Biodiversity Information Facility database then clusterised using `clusterised_sites()` from `DivInsight`.
#'
#' @examples
#' data("Singapore_clst")
"Singapore_clst"
