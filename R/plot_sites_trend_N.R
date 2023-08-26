#' Plot Local Abundance against Date
#'
#' Plot local abundance on the Y axis and dates on the X axis with uniform spacing for each date and a line of best fit.
#'
#' @import dplyr
#'
#' @param clusterised_object An object created by the clusterise_sites function.
#' @param xlabel The X axis label.
#' @param ylabel The Y axis label with "Abundance (N)" as default.
#' @param main_title The main title with "Local Abundance Over Time" as default.
#'
#' @return Returns a base R plot in the plot panel, depicting the trend of local abundance over time.
#' @export
#'
#' @examples # create a trend chart showing abundance values over time at the Caquetá province
#'Colombia_Caquetá_dataframe <- subset(Colombia, stateProvince == "Caquetá")
#'
#'clusterised_Caquetá <- clusterise_sites(dataframe = Colombia_Caquetá_dataframe,
#'                                        cluster_min_length = 30,
#'                                        group_radius = 20000
#')
#'
#'plot_sites_trend_N(clusterised_Caquetá)
plot_sites_trend_N <- function(clusterised_object, xlabel = "", ylabel = "Abundance (N)", main_title = "Local Abundance over Time"){

  stats_dataframe <- generate_stats(

    clusterised_object

  )

        N <- numeric()
  for(i in 1:length(clusterised_object[[1]])){

    N[i] <- length(clusterised_object[[1]][[i]][[1]])

  }

  plot(x = stats_dataframe$date %>% as.ordered,
       y = N,

       xlab = xlabel,
       ylab = ylabel,

       main = main_title,

       cex.axis = 0.6,
       las = 2
  )

  abline(lm(N ~ stats_dataframe$date %>% as.ordered %>% as.numeric))

}
