% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_sites_trend_H.R
\name{plot_sites_trend_H}
\alias{plot_sites_trend_H}
\title{Plot H against Date}
\usage{
plot_sites_trend_H(
  clusterised_object,
  xlabel = "",
  ylabel = "Diversity (H)",
  main_title = "Shannon's H over Time"
)
}
\arguments{
\item{clusterised_object}{An object created by the clusterise_sites function.}

\item{xlabel}{The X axis label.}

\item{ylabel}{The Y axis label with "Diversity (H)" as default.}

\item{main_title}{The main title with "Shannon's H over Time" as default.}
}
\description{
Plot Shannon's H on the Y axis and dates on the X axis with uniform spacing for each date and a line of best fit.
}
\examples{
# plot trend charts
\dontrun{plot_sites_trend_H(clusterised_object = clusterised_Meta)}
}