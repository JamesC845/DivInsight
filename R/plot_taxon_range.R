#' Plot Taxon Range against Year
#'
#' Display the the maximum inter-individual distance and abundance of a specified taxon over time.
#'
#' @import ggplot2
#' @import geosphere
#' @import gridExtra
#'
#' @param taxon_dataframe A dataset containing occurrence data for the taxon of interest.
#' @param abundance_col A character value that determines the colour of the abundance annotations.
#' @param title A character value to change the main title of the char
#'
#' @return Returns a ggplot2 bar plot, showing the relationship between years and taxon range.
#' @export
#'
#' @examples # plot abundance and maximum taxon range for the Centrolenidae family
#' Centro <- subset(Andes, family == "Centrolenidae")
#'
#' plot_taxon_range(Centro, "darkred")
plot_taxon_range <- function (taxon_dataframe, main_title = "", abundance_col = "black"){

  suppressWarnings({

    taxon_dataframe_split <- split(taxon_dataframe, taxon_dataframe$year)

    taxon_range_dataframe <- data.frame(year = sort(as.numeric(names(taxon_dataframe_split))),
                                        taxon_range = as.numeric(lapply(taxon_dataframe_split,
                                                                        find_taxon_range))/1000)

    taxon_dataframe_collapsed <- do.call(rbind, taxon_dataframe_split)

    abundance_table <- data.frame(Year = names(table(taxon_dataframe_collapsed$year)),
                                  N = as.numeric(table(taxon_dataframe_collapsed$year)))



    INNERFUNCTION <- function(taxon_dataframe, main_title = "", abundance_col = "black"){

      final_plot <- ggplot(taxon_range_dataframe, aes(x = taxon_range_dataframe$year,
                                                      y = taxon_range_dataframe$taxon_range)) +
        geom_bar(stat = "identity") +
        labs(x = "Year", y = "Maximum Distance (km)", title = main_title) +
        theme(text = element_text(size = 9.5)) + geom_line(aes(y = table(taxon_dataframe_collapsed$year),
                                                               group = 1), color = abundance_col, size = 0.85) +
        scale_x_continuous(breaks = unique(taxon_range_dataframe$year))
    }



    final_plot <- INNERFUNCTION(taxon_dataframe, main_title, abundance_col)

    final_plot <- final_plot + annotation_custom(
      tableGrob(
        d = t(abundance_table),

        theme = ttheme_default(
          base_size = 8,
          base_colour = abundance_col
        )),
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    )

  })

  return(final_plot)

}
