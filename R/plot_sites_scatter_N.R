#' Plot Local Abundance against Date
#'
#' Plot local abundance on the Y axis and dates on the X axis with scaled spacing for each date and scattered points.
#'
#' @import ggplot2
#'
#' @param clusterised_object An object created by the clusterise_sites function.
#' @param xlabel The X axis label with "Date" as default.
#' @param ylabel The Y axis label with "Abundance (N)" as default.
#' @param main_title The main label with "Local Abundance Over Time" as default.
#'
#' @return Returns a ggplot2 scatter plot in the plot panel, showing the relationship between dates and local abundance.
#' @export
#'
#' @examples # create a scatter plot showing abundance values over time at the Caquetá province
#'Colombia_Caquetá_dataframe <- subset(Colombia, stateProvince == "Caquetá")
#'
#'clusterised_Caquetá <- clusterise_sites(dataframe = Colombia_Caquetá_dataframe,
#'                                        cluster_min_length = 30,
#'                                        group_radius = 20000
#')
#'
#'plot_sites_scatter_N(clusterised_Caquetá)
plot_sites_scatter_N <- function(clusterised_object,
                                 xlabel = "Date",
                                 ylabel = "Abundance (N)",
                                 main_title = "Local Abundance over Time"){

        N <- numeric()
  for(i in 1:length(clusterised_object[[1]])){

    N[i] <- length(clusterised_object[[1]][[i]][[1]])

  }

  suppressWarnings({

    stats_dataframe <- generate_stats(clusterised_object)

    ggplot(stats_dataframe, aes(x = as.Date(stats_dataframe$date), y = N)) +
      geom_point() +
      labs(x = xlabel, y = ylabel, title = main_title)

  })

}
