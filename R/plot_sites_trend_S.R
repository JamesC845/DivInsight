#' Plot Species Richness against Date
#'
#' Plot Species Richness on the Y axis and dates on the X axis with uniform spacing for each date and a line of best fit.
#'
#' @import dplyr
#'
#' @param clusterised_object An object created by the clusterise_sites function.
#' @param xlabel The X axis label.
#' @param ylabel The Y axis label with "Species Richness (S)" as default.
#' @param main_title The main title with "Species Richness Over Time" as default.
#'
#' @return Returns a base R plot in the plot panel, depicting the trend of diversity index values over time.
#' @export
#'
#' @examples # create a trend chart showing species richness values over time at the Caquetá province
#'Colombia_Caquetá_dataframe <- subset(Colombia, stateProvince == "Caquetá")
#'
#'clusterised_Caquetá <- clusterise_sites(dataframe = Colombia_Caquetá_dataframe,
#'                                        cluster_min_length = 30,
#'                                        group_radius = 20000
#')
#'
#'plot_sites_trend_S(clusterised_Caquetá)
plot_sites_trend_S <- function(clusterised_object, xlabel = "", ylabel = "Diversity (S)", main_title = "Species Richness over Time"){



  stats_dataframe <- generate_stats(

    clusterised_object

  )


  plot(x = stats_dataframe$date %>% as.ordered,
       y = stats_dataframe$S,

       xlab = xlabel,
       ylab = ylabel,

       main = main_title,

       cex.axis = 0.6,
       las = 2
  )

  abline(lm(stats_dataframe$S ~ stats_dataframe$date %>% as.ordered %>% as.numeric ))

}
