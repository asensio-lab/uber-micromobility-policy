# uber-micromobility-policy
This repository contains scientific replication materials and code for "Micromobility displaces cars in the urban center: a natural experiment with mobile app geofencing".

## Repository Structure
The repository contains five directories and our primary analysis code file (**uber_micromobility_analysis.R**, described in detail below). Each of the five directories contains all relevant code, data, and data dictionaries for executing each portion of our analysis. We describe the purpose and contents of each directory below. We also note that we often provide data in excess of the specific variables required to reproduce our findings in order to support further analysis (i.e., not all data included in this repository is necessary to replicate the paper's results). 

For each dataset used in generating our main results, we provide a comprehensive data dictionary in `.txt` format defining each variable in the experiment's corresponding directory. For more information on sources of data and how user-derived variables were created, see Supplementary Information Table S5. 

### 1. Midtown Experiment
This directory contains all data required to replicate our findings from the Midtown Experiments, including both our difference-in-differences and triple-differences estimators. Contents include:
- `MidtownDD.csv`: The dataset used in formulating our difference-in-differences estimator for the Midtown experiment. 
- `MidtownDDD.csv`: The dataset used in formulating our triple-differences estimator for the Midtown experiment.
- `MidtownDD Data Dictionary.txt`: A data dictionary giving the data type and definition of each variable contained in the dataset for the difference-in-differences estimator.
- `MidtownDDD Data Dictionary.txt`: A data dictionary giving the data type and definition of each variable contained in the dataset for the triple-differences estimator (which contains additional counterfactual tracts).

### 2. MARTA Experiment
This directory contains all data required to replicate our findings from the MARTA Experiment. 
- `MARTA.csv`: The dataset used in formulating our difference-in-differences estimator for the MARTA experiment. 
- `MARTA Data Dictionary.txt`: A data dictionary giving the data type and definition of each variable contained in the dataset.

### 3. Mercedes-Benz Experiment
This directory contains all data required to replicate our findings from the Mercedes-Benz Experiment. 
- `Mercedes-Benz.csv`: The dataset used in formulating our fixed effects estimator for the Mercedes-Benz experiment. 
- `Mercedes-Benz Data Dictionary.txt`: A data dictionary giving the data type and definition of each variable contained in the dataset.

### 4. Supplementary Information
This directory contains all data and code required to replicate the tables and figures contained in the paper's Supplementary Information. 
- `Midtown_Descriptives.csv`: The dataset containing replication data for Table S2, descriptive statistics on the Midtown experiment and its corresponding tracts. 
- `MidtownDDD_2018.csv`: The dataset used for replicating Table S8, placebo testing using data from the year prior to the natural experiment (2018). The structure of the data is the same as the corresponding datasets from the main analysis.
- `parallel_trend_figs.py`: 



## Code Instructions and Information

The main analysis is conducted in R (see "**uber_micromobility_analysis.R**"). Running the R file in sequential order will create and save all tables and figures (except for Figure S1, which is generated in Python). The code was tested in R version 4.1.1. The data needed for the replication is available in both human and machine-readable formats, incuding .csv and .kml.

The packages needed for the analysis are listed below. The version of each package used in our analysis is shown in parenthesis after the package name.
- `plm` (2.6.0) for generating linear models from panel data / fixed-effects estimates.
- `miceadds` (3.11.6) for imputation.
- `estimatr` (0.32.3) for modelling with options for robust standard errors.
- `acs` (2.1.4) for integrating maps and data fron the U.S. Census Bureau. 
- `tigris` (1.4.1) for downloading TIGER shapefiles to use for mapping.
- `sf` (1.0.2) to encode spatial data for mapping.
- `ggplot2` (3.3.5) for generating all production figures.
- `dplyr` (1.0.7) for data manipulation and preprocessing. 
- `tidyverse` (1.3.1) for additional data manipulation and preprocessing.
- `lfe` (2.8.7.1) for multi-way clustering and verifying model results using one-way clustering.

The visual test for parallel trends is created in Python (see "**parallel_trend_figs.py**"). Running the Python file in sequential order will create and display trends of daily average travel times for the Midtown and MARTA experiment (see Figure S1). The code was tested in Python version 3.7.4. 

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
- *origin.tract*: Origin census tract for trip 
- *destination.tract*: Destination census tract for trip 
- *date*: Date in which trip occurred 
- *origin.area*: Origin area for trip (Midtown, Buckhead, Cumberland, Sandy Springs)
- *destination.area*: Destination area for trip (Midtown, Buckhead, Cumberland, Sandy Springs) 
- *distance*: Straight line distance from the centroid between census tracts (miles)
- *traveltime.mile*: Trip duration (min) 
- *post*: Binary variable indicating whether trip occurred after the ban 
- *treated*: Binary variable indicating whether trip began in census tract affected by the ban 
- *scooter*: Binary variable indicating whether trip began in census tract with scooters available
- *month*: Month in which trip occurred 
- *dayofweek*: Day of week in which trip occurred 
- *numvehicles*: Aggregate number of vehicles used in commuting by workers whose means of transportation is car, truck, or van within a census trac
- *transit.routes*: Number of transit routes within half a mile of an average block group 
- *bike.hubs*: Number of bike shares within half a mile of transit
- *walk.score*: Integer between 0-100 measuring the walkability of a census tract 
- *totalschool*: Count of students enrolled in school by Census tract 
- *event*: Binary variable indicating whether trip occurred on the day of a large event at the Mercedes Benz Station or State Farm Arena 
- *precip*: Binary variable indicating whether trip occurred during a time with precipitation 
