# Impacts of micromobility on car displacement with evidence from a natural experiment and geofencing policy 
This repository contains scientific replication materials and code for "Impacts of micromobility on car displacement with causal evidence from a natural experiment and geofencing policy".

## Repository Structure
The repository contains five directories and our primary analysis code file (**uber_micromobility_analysis.R**, described in detail below). Each of the five directories contains all relevant code, data, and data dictionaries for executing each portion of our analysis. We describe the purpose and contents of each directory below. We also note that we often provide data in excess of the specific variables required to reproduce our findings in order to support further analysis (i.e., not all data included in this repository is necessary to replicate the paper's results). 

For each dataset used in generating our main results, we provide a comprehensive data dictionary in `.txt` format defining each variable in the experiment's corresponding directory. For more information on sources of data and how user-derived variables were created, see Supplementary Information Table S5. 

### 1. Midtown Experiment
This directory contains all data required to replicate our findings from the Midtown Experiments, including both our difference-in-differences and triple-differences estimators. Contents include:
- `MidtownDDD.csv`: The dataset used in formulating our difference-in-differences and triple-differences estimator for the Midtown experiment.
- `MidtownDDD Data Dictionary.txt`: A data dictionary giving the data type and definition of each variable contained in the dataset for the difference-in-differences and triple-differences estimator (which contains additional counterfactual tracts).

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
- `parallel_trend_figs.py`: The Python script used to generate Figure S1, parallel time trends. We recommend executing this code on a Mac or Linux machine to guarantee replication of the figure as shown in the paper, because we identified a bug in one of the dependencies which causes the trendlines to display incorrectly on some Windows devices. 

### 5. Mapping
This directory contains data required to generate the map from Figure 1. With the release of new U.S. Census data, we note that careful attention should be paid to changing defaults for mapping as packages used in generating this figure are updated from the 2010 to 2020 Census to guarantee replication.
- `atlantatracts.csv`: lists the census tracts in the City of Atlanta.
- `martatracts.csv`: lists the census tracts that contain MARTA subway stations.
- `Transit_Routes_2019.csv`: describes the transit lines between MARTA subway stations.
- `Transit_Routes_2019.kml`: includes the location of MARTA subway stations for mapping purposes.

## Code Instructions and Information

The main analysis is conducted in R (see "**uber_micromobility_analysis.R**"). Running the R file in sequential order will create and save all tables and figures (except for Figure S1, which is generated in Python). For easy replication, ensure that the `.R` file is located one level above the five FILES described above, as it is designed to locate the data files in each of their respective directories. The code was tested in R version 4.1.1.

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

The code contains consistent naming conventions to make replication more straightforward. Tables follow the naming convention **table_XX** where **XX** represents the table's corresponding number in the manuscript. For example, Table S1 appears as a matrix named **table_s1** in the `.R` file.
 
The visual test for parallel trends is created in Python (see "**parallel_trend_figs.py**"). Running the Python file in sequential order will create and display trends of daily average travel times for the Midtown and MARTA experiment (see Figure S1). The code was tested in Python version 3.8.5. 

The packages needed for the visualizations include:
- `pandas` (1.1.3) for loading and manipulating data.
- `plotly` (5.2.1) for creating the visualizations.
- `statsmodels` (0.12.0) for generating trendlines.

The tracts used as treatment and counterfactual in the study are described in the main R analysis file. The raw data on travel times for the City of Atlanta is publicly available on movement.uber.com. 

Data retrieved from Uber Movement, (c) 2022 Uber Technologies, Inc., https://movement.uber.com. 
