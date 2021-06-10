# uber-micromobility-policy
This repository contains scientific replication materials and code for "Shared micromobility reduces urban traffic: evidence from a natural experiment with mobile app geofencing"

Contents:

The analysis is conducted in R. The code was tested in R version 4.0.3. The data needed for the replication is available in both human and machine-readable formats, incuding .csv and .kml.

The packages needed for the analysis include:
- plm
- texreg
- miceadds
- estimatr
- tmap
- acs
- tigris
- sf
- ggplot2
- plotly
- gridExtra
- dplyr
- grDevices
- tidyverse

The data files needed to replicate the econometric analysis as well as replicate the maps in Figure 1 include:
- atlantatracts.csv: lists the census tracts in the City of Atlanta.
- martatracts.csv: lists the census tracts that contain MARTA subway stations.
- Transit_Routes_2019.csv: describes the transit lines between MARTA subway stations.
- Transit_Routes_2019.kml: includes the location of MARTA subway stations. 

The tracts used as treatment and counterfactual in the study are described in the main R analysis file. 
The raw data on travel times for the City of Atlanta is publicly available on movement.uber.com. 
