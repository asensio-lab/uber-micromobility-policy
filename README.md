# uber-micromobility-policy
This repository contains scientific replication materials and code for "Micromobility displaces cars in the urban center: a field experiment with mobile app geofencing"

Contents:

The analysis is conducted in R. The code was tested in R version 4.1.1. The data needed for the replication is available in both human and machine-readable formats, incuding .csv and .kml.

The packages needed for the analysis include:
- `plm` for generating linear models from panel data / fixed-effects estimates.
- `miceadds` for imputation.
- `estimatr` for modelling with options for robust standard errors.
- `acs` for integrating maps and data fron the U.S. Census Bureau. 
- `tigris` for downloading TIGER shapefiles to use for mapping.
- `sf` to encode spatial data for mapping.
- `ggplot2` for generating all production figures.
- `dplyr` for data manipulation and preprocessing. 
- `tidyverse` for additional data manipulation and preprocessing.

The visual test for parallel trends is created in Python. The code was tested in Python version 3.7.4. 

The packages needed for the visualizations include:
- 'pandas' for loading and manipulating data
- 'datetime' for encoding strings as date objects
- 'plotly' for creating the visualizations

The data files needed to replicate the econometric analysis as well as replicate the maps in Figure 1 include:
- atlantatracts.csv: lists the census tracts in the City of Atlanta.
- martatracts.csv: lists the census tracts that contain MARTA subway stations.
- Transit_Routes_2019.csv: describes the transit lines between MARTA subway stations.
- Transit_Routes_2019.kml: includes the location of MARTA subway stations.
- MidtownDD_final.csv: the dataset used in calculating difference-in-differences estimators for Midtown experiment.
- MidtownDDD_final.csv: the dataset used in calculating triple-difference estimators for Midtown experiment.
- MB_final.csv: the dataset used for the Mercedes-Benz experiment. 
- MARTA_final.csv: the dataset used for the MARTA exeriment.
- covsfinal_MidtownDDD: used for generating descriptive statistics from Midtown experiment.

The tracts used as treatment and counterfactual in the study are described in the main R analysis file. 
The raw data on travel times for the City of Atlanta is publicly available on movement.uber.com. 
