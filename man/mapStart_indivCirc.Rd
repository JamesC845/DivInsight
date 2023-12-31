% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapStart_indivCirc.R
\name{mapStart_indivCirc}
\alias{mapStart_indivCirc}
\title{Create Individual Occurrence Map}
\usage{
mapStart_indivCirc(taxon_dataframe, site_name = "", colour = "black")
}
\arguments{
\item{taxon_dataframe}{A dataframe with occurrence data for the chosen taxon.}

\item{site_name}{An optional label to add to the coordinate circles.}

\item{colour}{The colour of the coordinate circles.}
}
\value{
A Viewer object: an interactive map.
}
\description{
Create interactive map using individual occurrence coordinates.
}
\examples{
# create an interactive map using coordinates from a taxon dataframe
dataframe_Lepto <- subset(Andes, family == "Leptodactylidae")

map <- mapStart_indivCirc(dataframe_Lepto, site_name = "Whole Dataset")

map
}
