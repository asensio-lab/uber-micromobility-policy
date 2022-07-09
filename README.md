# uber-micromobility-policy
This repository contains scientific replication materials and code for "Micromobility displaces cars in the urban center: a field experiment with mobile app geofencing"

## Code Instructions and Information

The analysis is conducted in R, see "**Analysis_policy_experiments_v2.R**". The code was tested in R version 4.1.1. The data needed for the replication is available in both human and machine-readable formats, incuding .csv and .kml.

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

The visual test for parallel trends is created in Python, see "**parallel_trend_figs.py**". The code was tested in Python version 3.7.4. 

The packages needed for the visualizations include:
- `pandas` for loading and manipulating data.
- `datetime` for encoding strings as date objects.
- `plotly` for creating the visualizations.

## Data Description and Dictionary

### Datasets

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

### Data Dictionary
The variables used in the econometric analysis include:
-*origin.tract*: Origin census tract for trip 
-*destination.tract*: Destination census tract for trip 
-*date*: Date in which trip occurred 
-*origin.area*: Origin area for trip (Midtown, Buckhead, Cumberland, Sandy Springs)
-*destination.area*: Destination area for trip (Midtown, Buckhead, Cumberland, Sandy Springs) 
-*distance*: Straight line distance from the centroid between census tracts (miles)
-*traveltime.mile*: Trip duration (min) 
-*post*: Binary variable indicating whether trip occurred after the ban 
-*treated*: Binary variable indicating whether trip began in census tract affected by the ban 
-*scooter*: Binary variable indicating whether trip began in census tract with scooters available
-*month*: Month in which trip occurred 
-*dayofweek*: Day of week in which trip occurred 
-*numvehicles*: Aggregate number of vehicles used in commuting by workers whose means of transportation is car, truck, or van within a census trac
-*transit.routes*: Number of transit routes within half a mile of an average block group 
-*bike.hubs*: Number of bike shares within half a mile of transit
-*walk.score*: Integer between 0-100 measuring the walkability of a census tract 
-*totalschool*: Count of students enrolled in school by Census tract 
-*event*: Binary variable indicating whether trip occurred on the day of a large event at the Mercedes Benz Station or State Farm Arena 
-*precip*: Binary variable indicating whether trip occurred during a time with precipitation 
