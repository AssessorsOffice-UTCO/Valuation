# Valuation

___

### This package is in development.

The goal is to house custom functions from the Data Analysis Team
here for easy access.

___

### Installation

To install the latest development version:

`devtools::install_github("UTCoAssessors/Valuation",dependencies = TRUE)`

___

### Current functions

`ratio_stats()` - Takes two vectors ("assessed values" and "actual sale prices") to calculate Ratio Statistics. The two vectors must be the same length and each element must correspond to a unique parcel. NA values are removed, so only parcels with actual sales info are included.

`find_cap_rate()` - Takes two vectors ("net_income" and "sale_price") and estimates cap rate. Function can handle missing data and sales == 0. Returns a named list of 6 elements with mean, median, and upper- and lower-bounds. Also plots the values.

`value_from_cap_rate()` - Takes a vector ("net_income") and a numeric value ("cap_rate"). Function can handle missing and invalid income values. Returns a vector of estimated valuations based on the formula Value = Income / Cap_Rate. Returns "NA" for any invalid net_income values.

`StripAttr()` - Taken from [*DescTools* package](https://cran.r-project.org/web/packages/DescTools/DescTools.pdf) to reduce dependencies

`MedianCI()` - Taken from [*DescTools*](https://cran.r-project.org/web/packages/DescTools/DescTools.pdf) package to reduce dependencies
