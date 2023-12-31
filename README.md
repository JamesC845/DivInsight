
<!-- README.md is generated from README.Rmd. Please edit that file -->

 

# DivInsight: Providing Ecological Insights from Historical Occurrence Data

Welcome to DivInsight, a tool for unlocking valuable ecological insights
from historical occurrence data. This R package is designed to help
researchers by providing powerful methods for ecological desk studies.

 

## What DivInsight Can Do

DivInsight takes datasets of taxon occurrences and enables you to:

- generate diversity index values for given locations
- understand species abundance and diversity changes over time
- create species composition matrices which can be used in other
  analyses
- query environmental data alongside diversity index values
- calculate and visualise taxon range

Please check the articles tab at the top of this page to see these
analyses in action!

 

## Installation

You can install DivInsight 0.1.0 from CRAN by using the following
command:

``` r
install.packages("DivInsight")
```

Until the newest version has been accepted by CRAN, DivInsight 1.2.0 can
be downloaded from GitHub using the following command:

``` r
remotes::install_github("JamesC845/DivInsight")
```

 

## Upcoming Updates

- DivInsight currently uses functions compatible with datasets queried
  from the Global Biodiversity Information Facility (GBIF) regarding
  occurrence data, as well as datasets queried the Storm Glass datagrid
  (www.stormglass.io) regarding environmental data. In the future, other
  data sources will be explored in order to improve the validity of
  results provided by this package.

- Given the laborious process of querying large datasets from GBIF using
  R, a vignette tutorial will be provided to show users the most
  efficient way to do so. This will be especially helpful for users
  unfamiliar with the `rgbif` package.

- Various utility functions will be added to make scripts more
  efficient.

 

## Feedback

Any kind of feedback on this new and upcoming R package is welcome.
Whether you have any suggestions for specific research applications,
ideas for new functions, or constructive criticism on how to improve the
package’s functionality, your thoughts are highly appreciated. Please
don’t hesitate to get in contact and share your feedback via email at
<jameschurchward1@outlook.com>.

 
