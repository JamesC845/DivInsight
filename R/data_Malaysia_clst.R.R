#' Plant Occurrence Clusters in Peninsular Malaysia (2005-2022)
#'
#' This dataset contains clustered occurrence data for various sites within Peninsular Malaysia. Clusters have a minimum size of 30 observations and are based on a radius of 5000 meters.
#'
#' @format
#' A list containing two elements. The first element is a list containing 62 dataframes (site clusters) of varying lengths. The second is a dataframe where each row has metadata pertaining to each of the 62 dataframes
#' @source Queried from the Global Biodiversity Information Facility database then clusterised using `clusterised_sites()` from `DivInsight`.
#'
#' @examples
#' data("Malaysia_clst")
"Malaysia_clst"
