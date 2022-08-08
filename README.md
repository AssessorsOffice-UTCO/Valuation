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

`StripAttr()` - Taken from *DescTools* package to reduce dependencies

`MedianCI()` - Taken from *DescTools* package to reduce dependencies
