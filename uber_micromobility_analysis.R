#The libraries needed are:

library(plm)
library(miceadds)
library(estimatr)
library(acs)
library(tigris)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lfe)

# Read in data
MB <- read.csv("Mercedes-Benz Experiment/Mercedes-Benz.csv", na = "empty")
MARTA <- read.csv("MARTA Experiment/MARTA.csv", na = "empty")
MARTA$Date <- format(as.Date(MARTA$Date, format = "%m/%d/%y"), "%Y-%m-%d")

MidtownDD <- read.csv("Midtown Experiment/MidtownDD.csv", na = "empty")
MidtownDD$Date <- format(as.Date(MidtownDD$Date, format = "%m/%d/%y"), "%Y-%m-%d")
MidtownDD$weeknumber = format(as.Date(MidtownDD$Date), "%V")
MidtownDD$post = as.logical(MidtownDD$post)
MidtownDD$treated = as.logical(MidtownDD$treated)

MidtownDDD <- read.csv("Midtown Experiment/MidtownDDD.csv", na = "empty")
MidtownDDD$Date <- format(as.Date(MidtownDDD$Date, format = "%m/%d/%y"), "%Y-%m-%d")
MidtownDDD$weeknumber = format(as.Date(MidtownDDD$Date), "%V")
MidtownDDD$treated = as.logical(MidtownDDD$treated)
MidtownDDD$post = as.logical(MidtownDDD$post)
MidtownDDD$scooter = as.logical(MidtownDDD$scooter)

#-------------------------------------------
#Figure 1
#-------------------------------------------

#-------------------------------------------
#Figure 1A
#-------------------------------------------
#All census tracts in the state of Georgia
shapefile <- tracts(state='13', year = 2019)
shapefile$NAME = as.numeric(shapefile$NAME)
#All school districts in the state of Georgia 
Atlanta <- school_districts(state = "13", year = 2019)
#Select the census tracts in the City of Atlanta
Atlanta <- subset(Atlanta, (NAME == "Atlanta City School District"))
#Finding tracts in the City of Atlanta
Atlanta.sub <- st_intersection(shapefile, Atlanta)
Atlanta.sub$NAME <- as.numeric(Atlanta.sub$NAME)
#Select all tracts in the City of Atlanta that are not in the treatment area
Atlanta.sub <- Atlanta.sub[Atlanta.sub$NAMELSAD != "Census Tract 4" & Atlanta.sub$NAMELSAD != "Census Tract 5" & Atlanta.sub$NAMELSAD != "Census Tract 5" & Atlanta.sub$NAMELSAD != "Census Tract 10.01" & Atlanta.sub$NAMELSAD != "Census Tract 12.01" & Atlanta.sub$NAMELSAD != "Census Tract 12.02" & Atlanta.sub$NAMELSAD != "Census Tract 11" & Atlanta.sub$NAMELSAD != "Census Tract 13",]

#Tracts name and code
coa <- read.csv("Mapping/atlantatracts.csv")

#Selecting tracts in county 67 
shapefile.cum <- tracts(state = "13", "67", year = 2019)
#Geometry on cumberland tracts
shapefile.cum <- merge(coa, shapefile.cum, by.x = "name", by.y = "NAMELSAD")

midtown_tracts = unique(MidtownDDD[MidtownDDD$origin.area == "m",]$origin)
midtown = (shapefile$NAME %in% midtown_tracts)

cumberland_tracts = unique(MidtownDDD[MidtownDDD$origin.area == "c",]$origin)
cumberland = (shapefile$NAME %in% cumberland_tracts)

#Adding variables for the type of zone
shapefile$Counterfactual <- ifelse(midtown,"Treatment - Midtown",ifelse(cumberland,"Counterfactual - Cumberland","None"))
shapefile.cum$Counterfactual <- "Counterfactual - Cumberland"

c <- c("Counterfactual - Cumberland" = "#D55E00", "Treatment - Midtown"="#56B4E9", "Policy Zone" = "grey", "None"="white")

#defining edges of the map
shapefile.cropped <- st_crop(shapefile, xmin = -84.2202, xmax = -84.6279,
                             ymin = 33.600, ymax = 34.020)

#All tracts except treatment and control zones
shapefile.cropped <- subset(shapefile.cropped, shapefile.cropped$NAME != 4.00 & shapefile.cropped$NAME != 5.00 & shapefile.cropped$NAME != 10.01 & shapefile.cropped$NAME != 12.01 & shapefile.cropped$NAME != 12.02 & shapefile.cropped$NAME != 11.00 & shapefile.cropped$NAME != 13.00 &
                              shapefile.cropped$NAMELSAD != "Census Tract 303.20" & shapefile.cropped$NAME != 303.39 & shapefile.cropped$NAME != 303.44 & shapefile.cropped$NAME != 303.45 & shapefile.cropped$NAME != 312.07 & shapefile.cropped$NAME != 312.11 )
#Geometry for midtown
shapefile.mid <- subset(shapefile, midtown|cumberland)
shapefile.mid <- subset(shapefile.mid, shapefile.mid$COUNTYFP == 121 | shapefile.mid$COUNTYFP == "067")

Atlanta.sub$Counterfactual<-"Policy Zone"

#Sandy springs

sandy_tracts = unique(MidtownDDD[MidtownDDD$origin.area == "s",]$origin)
sandy = (shapefile$NAME %in% sandy_tracts)

shapefile.sandy <- subset(shapefile, sandy)
shapefile.sandy <- subset(shapefile.sandy, shapefile.sandy$COUNTYFP == 121 |shapefile.sandy$COUNTYFP == "089")

shapefile.sandy$Counterfactual<-"Counterfactual - Sandy Springs"

#Buckhead
buckhead_tracts = unique(MidtownDDD[MidtownDDD$origin.area == "b",]$origin)
buck = (shapefile$NAME %in% buckhead_tracts)

shapefile.buck <- subset(shapefile, buck)
shapefile.buck <- subset(shapefile.buck, shapefile.buck$COUNTYFP == 121 |shapefile.buck$COUNTYFP == "089")

shapefile.buck$Counterfactual<-"Counterfactual - Buckhead"

c <- c("Counterfactual - Cumberland" = "#D55E00", "Treatment - Midtown"="#56B4E9", "Policy Zone" = "grey", "None"="white", "Counterfactual - Sandy Springs"="seagreen3", "Counterfactual - Buckhead"="darkmagenta")

Atlanta.sub <- Atlanta.sub[Atlanta.sub$NAMELSAD != "Census Tract 100.02" & Atlanta.sub$NAMELSAD != "Census Tract 100.01" & Atlanta.sub$NAMELSAD != "Census Tract 91.01" & Atlanta.sub$NAMELSAD != "Census Tract 95.02" & Atlanta.sub$NAMELSAD != "Census Tract 96.01" & Atlanta.sub$NAMELSAD != "Census Tract 95.01",]

#Figure 1A
figure_1a <- ggplot()+ 
  geom_sf(data = shapefile.cropped, aes(geometry=geometry, fill = Counterfactual), color = "#949494")+           #Outside white zone
  geom_sf(data = shapefile.mid, aes(geometry=geometry, fill = Counterfactual), lwd = 0.2, color = "black")+                 #Treatment and control zones
  geom_sf(data = shapefile.sandy, aes(geometry=geometry, fill = Counterfactual), lwd = 0.2, color = "black")+                 #Treatment and control zones
  geom_sf(data = shapefile.buck, aes(geometry=geometry, fill = Counterfactual),lwd = 0.2, color = "black")+                 #Treatment and control zones
  geom_sf(data = Atlanta.sub, aes(geometry=geometry, fill = Counterfactual),lwd = 0.2, color = "black" , size = 1)+      #Policy zone
  theme( axis.text =  element_blank(),
         axis.ticks =  element_blank(),
         panel.background = element_blank())+
  scale_fill_manual(values = c)+
  theme(text=element_text(family="Times New Roman", face="bold", size=20))+
  theme(legend.title = element_blank())

figure_1a

#-------------------------------------------
#Figure 1C
#-------------------------------------------


shapefile2 <- tracts(state='13', year = 2019)
shapefile2$NAME = as.numeric(shapefile2$NAME)

Atlanta <- school_districts(state = "13", year = 2019)
Atlanta <- subset(Atlanta, (NAME == "Atlanta City School District"))
Atlanta.sub <- st_intersection(shapefile2, Atlanta)
Atlanta.sub <- Atlanta.sub[Atlanta.sub$NAMELSAD != "Census Tract 4" & Atlanta.sub$NAMELSAD != "Census Tract 5"& 
                             Atlanta.sub$NAMELSAD != "Census Tract 10.01" & Atlanta.sub$NAMELSAD != "Census Tract 12.01" & 
                             Atlanta.sub$NAMELSAD != "Census Tract 12.02" & Atlanta.sub$NAMELSAD != "Census Tract 11" &
                             Atlanta.sub$NAMELSAD != "Census Tract 13"& Atlanta.sub$NAMELSAD != "Census Tract 18"& Atlanta.sub$NAMELSAD != "Census Tract 19"& 
                             Atlanta.sub$NAMELSAD != "Census Tract 21"& Atlanta.sub$NAMELSAD != "Census Tract 28"& Atlanta.sub$NAMELSAD != "Census Tract 35"& 
                             Atlanta.sub$NAMELSAD != "Census Tract 118"& Atlanta.sub$NAMELSAD != "Census Tract 119"& Atlanta.sub$NAMELSAD != "Census Tract 120"& 
                             Atlanta.sub$NAMELSAD != "Census Tract 26",]

destination_tracts = unique(MB$destination)
destination = (shapefile2$NAME %in% destination_tracts)

shapefile2$Location <- ifelse(destination,"Destination",ifelse(shapefile2$NAME == 26.00,"Origin","Policy Zone" ))
shapefile2$Location[is.na(shapefile2$Location)] = "None"

shapefile2$Location <- factor(shapefile2$Location, levels = c("Origin", "Destination", "None"))

c <- c("Origin" = "#CC79A7", "Destination"="#F0E442", "None"="white",  "Policy Zone" = "grey")

shapefile2.cropped <- st_crop(shapefile2, xmin = -84.2202, xmax = -84.6279,
                              ymin = 33.600, ymax = 34.020)



shapefile2.cropped <- subset(shapefile2.cropped, shapefile2.cropped$NAME != 4.00 & shapefile2.cropped$NAME != 5.00 & shapefile2.cropped$NAME != 10.01 & shapefile2.cropped$NAME != 12.01 & shapefile2.cropped$NAME != 12.02 & shapefile2.cropped$NAME != 11.00 & shapefile2.cropped$NAME != 13.00 & shapefile2.cropped$NAME != 18.00 & shapefile2.cropped$NAME != 19.00 & shapefile2.cropped$NAME != 21.00 & shapefile2.cropped$NAME != 28.00 & shapefile2.cropped$NAME != 35.00 & shapefile2.cropped$NAME != 119.00 & shapefile2.cropped$NAME != 120.00 & shapefile2.cropped$NAME != 118.00 & shapefile2.cropped$NAME != 26.00 )

shapefile2.MB <- subset(shapefile2, destination | shapefile2$NAME == 26.00)
shapefile2.MB <- subset(shapefile2.MB, shapefile2.MB$COUNTYFP == 121)

Atlanta.sub$Counterfactual<-"Policy Zone"

figure_1c <- ggplot()+ 
  geom_sf(data= shapefile2.cropped, aes( fill=Location, geometry=geometry), color = "#949494")+
  geom_sf(data= shapefile2.MB, aes( fill=Location, geometry=geometry), lwd = 0.2, color = "black")+
  geom_sf(data = Atlanta.sub, aes(geometry=geometry, fill=Counterfactual),lwd = 0.2, color = "black")+
  theme( axis.text =  element_blank(),
         axis.ticks =  element_blank(),
         panel.background = element_blank())+
  scale_fill_manual(values = c)+
  theme(text=element_text(family="Times New Roman", face="bold", size=20))+
  theme(legend.title = element_blank())

figure_1c


#-------------------------------------------
#Figure 1B
#-------------------------------------------

atl.counties3 <- c("121","89","67","63","135","97","151")
shapefile3 <- tracts(state='13',county=atl.counties3, year = 2019)
shapefile3$NAME = as.numeric(shapefile3$NAME)
Atlanta <- school_districts(state = "13", year = 2019)
Atlanta <- subset(Atlanta, (NAME == "Atlanta City School District"))
Atlanta.sub <- st_intersection(shapefile3, Atlanta)
Atlanta.sub <- Atlanta.sub[Atlanta.sub$NAMELSAD != "Census Tract 4" & Atlanta.sub$NAMELSAD != "Census Tract 25" & Atlanta.sub$NAMELSAD != "Census Tract 85" & Atlanta.sub$NAMELSAD != "Census Tract 204" & Atlanta.sub$NAMELSAD != "Census Tract 19" & Atlanta.sub$NAMELSAD != "Census Tract 208.01" & Atlanta.sub$NAMELSAD != "Census Tract 35" & Atlanta.sub$NAMELSAD != "Census Tract 26" & Atlanta.sub$NAMELSAD != "Census Tract 81.02" & Atlanta.sub$NAMELSAD != "Census Tract 30" & Atlanta.sub$NAMELSAD != "Census Tract 119" & Atlanta.sub$NAMELSAD != "Census Tract 76.03" & Atlanta.sub$NAMELSAD != "Census Tract 94.03"& Atlanta.sub$NAMELSAD != "Census Tract 94.02" & Atlanta.sub$NAMELSAD != "Census Tract 12.02" & Atlanta.sub$NAMELSAD != "Census Tract 66.02" & Atlanta.sub$NAMELSAD != "Census Tract 42" & Atlanta.sub$NAMELSAD != "Census Tract 40",]

marta <- read.csv("Mapping/martatracts.csv")

shapefile3.marta <- tracts(state = "13", county = atl.counties3, year = 2019)

MARTA.out.tracts = unique(MARTA[MARTA$treatment == 0,]$origin)
MARTA.out = (shapefile3$NAME %in% MARTA.out.tracts)

shapefile3.marta <- subset(shapefile3.marta, (MARTA.out))
shapefile3.marta$Counterfactual <- "Counterfactual Tract"

lines <- read.csv("Mapping/Transit_Routes_2019.csv")
lines.shape <- st_read("Mapping/Transit_Routes_2019.kml")
lines.together <- bind_cols(lines, lines.shape)
lines.together <- subset(lines.together, lines.together$agency_id == "MARTA")
lines.together <- subset(lines.together, lines.together$rte_type == "subway, metro")

MARTA.in.tracts = unique(MARTA[MARTA$treatment == 1,]$origin)
MARTA.in = (shapefile3$NAME %in% MARTA.in.tracts)
shapefile3$Counterfactual <- ifelse(MARTA.in, "Treatment Tract", "None")
marta$location <- "MARTA Station"

c <- c("Counterfactual Tract" = "#D55E00", "Treatment Tract"="#56B4E9", "None"="white", "Policy Zone"="grey")

shapefile3.cropped <- st_crop(shapefile3, xmin = -84.2202, xmax = -84.6279,
                              ymin = 33.600, ymax = 34.020)


all_marta = unique(MARTA$origin)
shapefile3.cropped = subset(shapefile3.cropped, !(shapefile3.cropped$NAME %in% all_marta))
shapefile3.in = subset(shapefile3, shapefile3$NAME %in% MARTA.out.tracts | shapefile3$NAME %in% MARTA.in.tracts)
shapefile3.in$Counterfactual <- ifelse(shapefile3.in$Counterfactual == "None","Counterfactual Tract","Treatment Tract")

Atlanta.sub$Counterfactual<-"Policy Zone"

figure_1b <- ggplot()+ 
  geom_sf(data = shapefile3.cropped, aes(geometry=geometry, fill = Counterfactual), color = "#949494")+
  geom_sf(data = shapefile3.in, aes(geometry=geometry, fill= Counterfactual),lwd = 0.2, color="black")+
  geom_sf(data = Atlanta.sub, aes( geometry=geometry, fill=Counterfactual),lwd = 0.2, color = "black")+
  geom_point(data = marta, aes(x = long, y = lat),
             shape = 16, fill = "black", size=2)+
  geom_sf(data = lines.together, aes(geometry=geometry), size = .5)+
  theme( axis.text =  element_blank(),
         axis.ticks =  element_blank(),
         panel.background = element_blank())+
  scale_fill_manual(values = c)+
  theme(axis.title = element_blank())+
  theme(text=element_text(family="Times New Roman", face="bold", size=20))+
  theme(legend.title = element_blank())

figure_1b


#-------------------------------------------
#Table 1
#-------------------------------------------

#------------------------------------------
#Mercedes Benz
#-------------------------------------------

#Getting necessary variable for the converted coefficeint 
average.pre.attendance <- (40048 + 42537 + 42703 + 42557)/4
average.distance <- distinct(MB, origin, destination , .keep_all = TRUE)
average.distance <- mean(average.distance$distance)


#Getting the converted Estimates and Standard Errors
MB$converted.independent <- MB$traveltime.person*average.pre.attendance*(1/average.distance)


#Fixed Effects
MB$OD <- paste(MB$origin,MB$destination)
MB <- MB[c(9,4,8,5)]
MB$dependent <- MB$converted.independent
MB_est <- plm(dependent ~ policy.dummy, data = MB)

# Confidence intervals
MB_CI = confint(MB_est, level = 0.95)
#-------------------------------------------
#MARTA
#-------------------------------------------

MARTA <- MARTA %>%
  dplyr::mutate(dayofweek = case_when(Mon == 1 ~ "Monday",
                              Wed == 1 ~ "Wednesday",
                              Thur == 1 ~ "Thursday",
                              Fri == 1 ~ "Friday",
                              Sat == 1 ~ "Saturday",
                              Sun == 1 ~ "Sunday",
                              TRUE ~ "Tuesday"))

# Using lm_robust package
MARTA_est <- lm_robust(Travel.Time.per.Mile..Minutes.Mile. ~ treatment + post + post*treatment + as.factor(month) + Vehicles + TransRoutes + WalkScore + NumBikeshare + school.population  + event + as.factor(dayofweek), data = MARTA, cluster = as.factor(origin.tract), se_type = "stata")
summary(MARTA_est)
MARTA_est

# Confidence intervals
MARTA_CI = confint(MARTA_est)

# Validating with felm package
est_MARTA <- felm(Travel.Time.per.Mile..Minutes.Mile. ~ treatment + post + post*treatment + month + Vehicles + TransRoutes + WalkScore + NumBikeshare + school.population + event + as.factor(dayofweek) | 0 | 0 | origin.tract, data = MARTA, cmethod = "cgm2")
summary(est_MARTA)
tidy(est_MARTA, conf.int = TRUE)

#-------------------------------------------
#MIDTOWN DD
#-------------------------------------------

MidtownDD_est = lm_robust(as.numeric(traveltime.mileEvening) ~ treated+ post + treated*post + as.factor(dayofweek) + as.factor(month)+ numvehicles + transRoutes + WalkScore + NumBikeshare+ totalschool + Precip,
                          data=MidtownDD, se_type = "stata", clusters=MidtownDD$origin.tract)   
summary(MidtownDD_est)
MidtownDD_CI = confint(MidtownDD_est)

# Validate with FELM package
est_MidtownDD <- felm(as.numeric(traveltime.mileEvening) ~ treated+ post + treated*post +
                        numvehicles + transRoutes + WalkScore +
                        NumBikeshare+ totalschool + Precip + as.factor(month) + as.factor(dayofweek) | 0 | 0 |origin.tract,
                      data = MidtownDD, cmethod = "cgm2")
summary(est_MidtownDD)

# Validate CIs
tidy(est_MidtownDD, conf.int = TRUE)
#-------------------------------------------
#MIDTOWN DDD
#-------------------------------------------

MidtownDDD_est<-lm_robust(traveltime.mile ~ treated+ post + scooter + treated*post + treated*scooter+ post*scooter +
                            treated*post*scooter + as.factor(month) + numvehicles +
                            transit.routes + walk.score + bike.hubs + totalschool +
                            Precip + as.factor(dayofweek), data=MidtownDDD, clusters=origin.tract, se_type = "stata")
summary(MidtownDDD_est)
MidtownDDD_CI = confint(MidtownDDD_est)

# Validate with FELM package
est_MidtownDDD <- felm(traveltime.mile ~ treated+ post + scooter + treated*post + treated*scooter+ post*scooter + treated*post*scooter + numvehicles + transit.routes + walk.score + bike.hubs +
                         Precip + totalschool+ as.factor(dayofweek) + as.factor(month) | 0 | 0 | origin.tract,
                       data = MidtownDDD, cmethod = "cgm2")
summary(est_MidtownDDD)

# Validate CIs
t=tidy(est_MidtownDDD, conf.int = TRUE)

#-------------------------------------------
#TABLE 1
#-------------------------------------------

table_1<- matrix(nrow = 6,ncol = 7, dimnames = list(c("Midtown Experiment","SE","MARTA Experiment","SE","Mercedes Benz Experiment","SE"),
                                                    c("Diff-indiff Estimator","DDD Estimator", "CI Low", "CI High", "Percent", "Minutes Low", "Minutes High")))

table_1[1, ] <- c(round(MidtownDD_est$coefficients['treatedTRUE:postTRUE'],3), round(MidtownDDD_est$coefficients['treatedTRUE:postTRUE:scooterTRUE'],3), round(MidtownDDD_CI[nrow(MidtownDDD_CI),][['2.5 %']],3), round(MidtownDDD_CI[nrow(MidtownDDD_CI),][['97.5 %']],3), NA, NA, NA)
table_1[2, ] <- c(round(MidtownDD_est$std.error['treatedTRUE:postTRUE'],3), round(MidtownDDD_est$std.error['treatedTRUE:postTRUE:scooterTRUE'],3), "-", "-", NA, NA, NA)
table_1[3, ] <- c(round(MARTA_est$coefficients['treatment:post'],3), "-", round(MARTA_CI[nrow(MARTA_CI),][['2.5 %']],3), round(MARTA_CI[nrow(MARTA_CI),][['97.5 %']],3), NA, NA, NA)
table_1[4, ] <- c(round(MARTA_est$std.error['treatment:post'],3), "-", "-", "-", NA, NA, NA)
table_1[5, ] <- c(round(MB_est$coefficients[1],3), "-" , round(MB_CI[1,][['2.5 %']],3) , round(MB_CI[1,][['97.5 %']],3), NA, NA, NA)
table_1[6, ] <- c(round(summary(MB_est)$coefficients[2],3), "-", "-", "-", NA, NA, NA)

table_1[,5] <- c(round(13.4 * MidtownDDD_est$coefficients['treatedTRUE:postTRUE:scooterTRUE'] / 32.5,4),
                 "-",
                 round(13.4 * MARTA_est$coefficients['treatment:post'] / 32.5,4),
                 "-",
                 round(13.4 * MB_est$coefficients[1] / 32.5,4),
                 "-")

table_1[,6] <- c(round(13.4 * MidtownDDD_CI[nrow(MidtownDDD_CI),][['2.5 %']],2),
                 "-",
                 round(13.4 * MARTA_CI[nrow(MARTA_CI),][['2.5 %']] ,2),
                 "-",
                 round(13.4 * MB_CI[1,][['2.5 %']],2),
                 "-")

table_1[,7] <- c(round(13.4 * MidtownDDD_CI[nrow(MidtownDDD_CI),][['97.5 %']],2),
                 "-",
                 round(13.4 * MARTA_CI[nrow(MARTA_CI),][['97.5 %']] ,2),
                 "-",
                 round(13.4 * MB_CI[1,][['97.5 %']],2),
                 "-")
table_1

#-------------------------------------------
# Figure 2a
#-------------------------------------------

coeffs <- c()
ci_low <- c()
ci_high <- c()
se <- c()
p <- c()
observations <- c()
f <- c()
start <- as.Date("2019-08-10")
MidtownDDD <- read.csv("Midtown Experiment/MidtownDDD.csv", na = "empty")
MidtownDDD$treated = as.logical(MidtownDDD$treated)
MidtownDDD$post = as.logical(MidtownDDD$post)
MidtownDDD$scooter = as.logical(MidtownDDD$scooter)
MidtownDDD$Date <- format(as.Date(MidtownDDD$Date, format = "%m/%d/%y"), "%Y-%m-%d")
for(i in 0:27) {
  print(start)
  subset <- MidtownDDD[MidtownDDD$Date <= start,]
  reg <- lm_robust(traveltime.mile ~ treated+ post + scooter + treated*post + treated*scooter+ post*scooter + treated*post*scooter + as.factor(dayofweek) + numvehicles + transit.routes + walk.score + bike.hubs + totalschool + Precip, data=subset, se_type = "stata")
  start <- start + 1
  coeffs <- c(coeffs, unname(reg$coefficients[["treatedTRUE:postTRUE:scooterTRUE"]]))
  ci_low <- c(ci_low, unname(reg$conf.low[["treatedTRUE:postTRUE:scooterTRUE"]]))
  ci_high <- c(ci_high, unname(reg$conf.high[["treatedTRUE:postTRUE:scooterTRUE"]]))
  se <- c(se, unname(reg$std.error[["treatedTRUE:postTRUE:scooterTRUE"]]))
  p <- c(p, unname(reg$p.value[["treatedTRUE:postTRUE:scooterTRUE"]]))
  f <- c(f, summary(reg)$fstatistic[["value"]])
  observations <- c(observations, nrow(subset))
}

effects_over_time <- data.frame(ci_low, ci_high, coeffs)

fig2a <- ggplot(effects_over_time, aes(index(effects_over_time))) + 
  geom_ribbon(
    aes(ymin = ci_low, ymax = ci_high), fill = "royalblue2", alpha = 0.2) + 
  geom_line(aes(y = coeffs), color = "black", linetype=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                     panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                     axis.title.x = element_text(size = 20, margin = unit(c(3, 0, 0, 0), "mm")),
                                     axis.title.y = element_text(size = 20, margin = unit(c(0, 3, 0, 0), "mm")),
                                     axis.text = element_text(size = 18)) +
  geom_line(aes(y = ci_low), color = "black", linetype = 3) +
  geom_line(aes(y = ci_high), color = "black", linetype = 3) + 
  xlab("Days Since Policy Implementation") +
  ylab("Effect Size") +
  coord_cartesian(xlim =c(2.2, 27)) +
  scale_x_continuous(breaks=c(1, 5,10, 15, 20, 25,30))+
  scale_y_continuous(limits = c(-0.5,1.2), breaks=c(-0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1))+
  geom_hline(yintercept=0, linetype = 1)
  
fig2a

#-------------------------------------------
#TABLE S1
#-------------------------------------------

table_s1<- matrix(nrow = 7,ncol = 5, dimnames = list(c("Midtown","Buckhead", "Cumberland", "Sandy Springs", "Policy zone subway", "No policy zone subway", "Stadium"),
                                                     c("Mean","SD", "Min", "Max", "N")))

table_s1[1,] <- c(mean(subset(MidtownDDD, origin.area == 'm')$traveltime.mile),
                  sd(subset(MidtownDDD, origin.area == 'm')$traveltime.mile),
                  min(subset(MidtownDDD, origin.area == 'm')$traveltime.mile), 
                  max(subset(MidtownDDD, origin.area == 'm')$traveltime.mile),
                  length(subset(MidtownDDD, origin.area == 'm')$traveltime.mile))
table_s1[2,] <- c(mean(subset(MidtownDDD, origin.area == 'c')$traveltime.mile),
                  sd(subset(MidtownDDD, origin.area == 'c')$traveltime.mile),
                  min(subset(MidtownDDD, origin.area == 'c')$traveltime.mile), 
                  max(subset(MidtownDDD, origin.area == 'c')$traveltime.mile),
                  length(subset(MidtownDDD, origin.area == 'c')$traveltime.mile))
table_s1[3,] <- c(mean(subset(MidtownDDD, origin.area == 'b')$traveltime.mile),
                  sd(subset(MidtownDDD, origin.area == 'b')$traveltime.mile),
                  min(subset(MidtownDDD, origin.area == 'b')$traveltime.mile), 
                  max(subset(MidtownDDD, origin.area == 'b')$traveltime.mile),
                  length(subset(MidtownDDD, origin.area == 'b')$traveltime.mile))
table_s1[4,] <- c(mean(subset(MidtownDDD, origin.area == 's')$traveltime.mile),
                  sd(subset(MidtownDDD, origin.area == 's')$traveltime.mile),
                  min(subset(MidtownDDD, origin.area == 's')$traveltime.mile), 
                  max(subset(MidtownDDD, origin.area == 's')$traveltime.mile),
                  length(subset(MidtownDDD, origin.area == 's')$traveltime.mile))
table_s1[5,] <- c(mean(subset(MARTA, treatment == 1)$Travel.Time.per.Mile..Minutes.Mile.),
                  sd(subset(MARTA, treatment == 1)$Travel.Time.per.Mile..Minutes.Mile.),
                  min(subset(MARTA, treatment == 1)$Travel.Time.per.Mile..Minutes.Mile.), 
                  max(subset(MARTA, treatment == 1)$Travel.Time.per.Mile..Minutes.Mile.),
                  length(subset(MARTA, treatment == 1)$Travel.Time.per.Mile..Minutes.Mile.))
table_s1[6,] <- c(mean(subset(MARTA, treatment == 0)$Travel.Time.per.Mile..Minutes.Mile.),
                  sd(subset(MARTA, treatment == 0)$Travel.Time.per.Mile..Minutes.Mile.),
                  min(subset(MARTA, treatment == 0)$Travel.Time.per.Mile..Minutes.Mile.), 
                  max(subset(MARTA, treatment == 0)$Travel.Time.per.Mile..Minutes.Mile.),
                  length(subset(MARTA, treatment == 0)$Travel.Time.per.Mile..Minutes.Mile.))
table_s1[7,] <- c(mean(MB$dependent),
                  sd(MB$dependent),
                  min(MB$dependent), 
                  max(MB$dependent),
                  length(MB$dependent))


#-------------------------------------------
#TABLE S2
#-------------------------------------------
covs = read_csv("Supplementary Information/Midtown_Descriptives.csv")
covs$minorityShare <- (1 - covs$White/covs$Population)

table_s2<- matrix(nrow = 14,ncol = 8, dimnames = list(c("Pop","Med Age", "Med Inc", "Vehicles","Ed. Attainment", "Pct. Nonwhite", "Transit score", "No. routes", "Walk score", "No. bike hubs","Travel Time", "Tot enrollment", "K-12", "College"),
                                                      c("Mean, Midtown","SD, Midtown", "Min, Midtown", "Max, Midtown","Mean, CF","SD, CF", "Min, CF", "Max, CF")))

table_s2[1,] <- c(mean(subset(covs, origin.area == 'm')$Population),
                  sd(subset(covs, origin.area == 'm')$Population),
                  min(subset(covs, origin.area == 'm')$Population), 
                  max(subset(covs, origin.area == 'm')$Population),
                  mean(subset(covs, origin.area != 'm')$Population),
                  sd(subset(covs, origin.area != 'm')$Population),
                  min(subset(covs, origin.area != 'm')$Population), 
                  max(subset(covs, origin.area != 'm')$Population))

table_s2[2,] <- c(mean(subset(covs, origin.area == 'm')$MedianAge),
                  sd(subset(covs, origin.area == 'm')$MedianAge),
                  min(subset(covs, origin.area == 'm')$MedianAge), 
                  max(subset(covs, origin.area == 'm')$MedianAge),
                  mean(subset(covs, origin.area != 'm')$MedianAge),
                  sd(subset(covs, origin.area != 'm')$MedianAge),
                  min(subset(covs, origin.area != 'm')$MedianAge), 
                  max(subset(covs, origin.area != 'm')$MedianAge))

table_s2[3,] <- c(mean(subset(covs, origin.area == 'm')$MedianIncome),
                  sd(subset(covs, origin.area == 'm')$MedianIncome),
                  min(subset(covs, origin.area == 'm')$MedianIncome), 
                  max(subset(covs, origin.area == 'm')$MedianIncome),
                  mean(subset(covs, origin.area != 'm')$MedianIncome),
                  sd(subset(covs, origin.area != 'm')$MedianIncome),
                  min(subset(covs, origin.area != 'm')$MedianIncome), 
                  max(subset(covs, origin.area != 'm')$MedianIncome))

table_s2[4,] <- c(mean(subset(covs, origin.area == 'm')$veh.pop),
                  sd(subset(covs, origin.area == 'm')$veh.pop),
                  min(subset(covs, origin.area == 'm')$veh.pop), 
                  max(subset(covs, origin.area == 'm')$veh.pop),
                  mean(subset(covs, origin.area != 'm')$veh.pop),
                  sd(subset(covs, origin.area != 'm')$veh.pop),
                  min(subset(covs, origin.area != 'm')$veh.pop), 
                  max(subset(covs, origin.area != 'm')$veh.pop))

table_s2[5,] <- c(mean(subset(covs, origin.area == 'm')$Bachelors),
                  sd(subset(covs, origin.area == 'm')$Bachelors),
                  min(subset(covs, origin.area == 'm')$Bachelors), 
                  max(subset(covs, origin.area == 'm')$Bachelors),
                  mean(subset(covs, origin.area != 'm')$Bachelors),
                  sd(subset(covs, origin.area != 'm')$Bachelors),
                  min(subset(covs, origin.area != 'm')$Bachelors), 
                  max(subset(covs, origin.area != 'm')$Bachelors))

table_s2[6,] <- c(mean(subset(covs, origin.area == 'm')$minorityShare),
                  sd(subset(covs, origin.area == 'm')$minorityShare),
                  min(subset(covs, origin.area == 'm')$minorityShare), 
                  max(subset(covs, origin.area == 'm')$minorityShare),
                  mean(subset(covs, origin.area != 'm')$minorityShare),
                  sd(subset(covs, origin.area != 'm')$minorityShare),
                  min(subset(covs, origin.area != 'm')$minorityShare), 
                  max(subset(covs, origin.area != 'm')$minorityShare))


table_s2[7,] <- c(mean(subset(covs, origin.area == 'm')$transit.score),
                  sd(subset(covs, origin.area == 'm')$transit.score),
                  min(subset(covs, origin.area == 'm')$transit.score), 
                  max(subset(covs, origin.area == 'm')$transit.score),
                  mean(subset(covs, origin.area != 'm')$transit.score),
                  sd(subset(covs, origin.area != 'm')$transit.score),
                  min(subset(covs, origin.area != 'm')$transit.score), 
                  max(subset(covs, origin.area != 'm')$transit.score))

table_s2[8,] <- c(mean(subset(covs, origin.area == 'm')$transit.routes),
                  sd(subset(covs, origin.area == 'm')$transit.routes),
                  min(subset(covs, origin.area == 'm')$transit.routes), 
                  max(subset(covs, origin.area == 'm')$transit.routes),
                  mean(subset(covs, origin.area != 'm')$transit.routes),
                  sd(subset(covs, origin.area != 'm')$transit.routes),
                  min(subset(covs, origin.area != 'm')$transit.routes), 
                  max(subset(covs, origin.area != 'm')$transit.routes))

table_s2[9,] <- c(mean(subset(covs, origin.area == 'm')$walk.score),
                  sd(subset(covs, origin.area == 'm')$walk.score),
                  min(subset(covs, origin.area == 'm')$walk.score), 
                  max(subset(covs, origin.area == 'm')$walk.score),
                  mean(subset(covs, origin.area != 'm')$walk.score),
                  sd(subset(covs, origin.area != 'm')$walk.score),
                  min(subset(covs, origin.area != 'm')$walk.score), 
                  max(subset(covs, origin.area != 'm')$walk.score))

table_s2[10,] <- c(mean(subset(covs, origin.area == 'm')$bike.hubs),
                   sd(subset(covs, origin.area == 'm')$bike.hubs),
                   min(subset(covs, origin.area == 'm')$bike.hubs), 
                   max(subset(covs, origin.area == 'm')$bike.hubs),
                   mean(subset(covs, origin.area != 'm')$bike.hubs),
                   sd(subset(covs, origin.area != 'm')$bike.hubs),
                   min(subset(covs, origin.area != 'm')$bike.hubs), 
                   max(subset(covs, origin.area != 'm')$bike.hubs))

table_s2[11,] <- c(mean(subset(covs, origin.area == 'm')$AggTravelTime),
                   sd(subset(covs, origin.area == 'm')$AggTravelTime),
                   min(subset(covs, origin.area == 'm')$AggTravelTime), 
                   max(subset(covs, origin.area == 'm')$AggTravelTime),
                   mean(subset(covs, origin.area != 'm')$AggTravelTime),
                   sd(subset(covs, origin.area != 'm')$AggTravelTime),
                   min(subset(covs, origin.area != 'm')$AggTravelTime), 
                   max(subset(covs, origin.area != 'm')$AggTravelTime))


table_s2[12,] <- c(mean(subset(covs, origin.area == 'm')$total.school),
                   sd(subset(covs, origin.area == 'm')$total.school),
                   min(subset(covs, origin.area == 'm')$total.school), 
                   max(subset(covs, origin.area == 'm')$total.school),
                   mean(subset(covs, origin.area != 'm')$total.school),
                   sd(subset(covs, origin.area != 'm')$total.school),
                   min(subset(covs, origin.area != 'm')$total.school), 
                   max(subset(covs, origin.area != 'm')$total.school))

table_s2[13,] <- c(mean(subset(covs, origin.area == 'm')$k12),
                   sd(subset(covs, origin.area == 'm')$k12),
                   min(subset(covs, origin.area == 'm')$k12), 
                   max(subset(covs, origin.area == 'm')$k12),
                   mean(subset(covs, origin.area != 'm')$k12),
                   sd(subset(covs, origin.area != 'm')$k12),
                   min(subset(covs, origin.area != 'm')$k12), 
                   max(subset(covs, origin.area != 'm')$k12))

table_s2[14,] <- c(mean(subset(covs, origin.area == 'm')$college),
                   sd(subset(covs, origin.area == 'm')$college),
                   min(subset(covs, origin.area == 'm')$college), 
                   max(subset(covs, origin.area == 'm')$college),
                   mean(subset(covs, origin.area != 'm')$college),
                   sd(subset(covs, origin.area != 'm')$college),
                   min(subset(covs, origin.area != 'm')$college), 
                   max(subset(covs, origin.area != 'm')$college))

table_s2 = round(table_s2, 2)

#p-values
t.test(subset(covs, origin.area == 'm')$Population,
       subset(covs, origin.area != 'm')$Population)$p.value

t.test(subset(covs, origin.area == 'm')$MedianAge,
       subset(covs, origin.area != 'm')$MedianAge)$p.value

t.test(subset(covs, origin.area == 'm')$MedianIncome,
       subset(covs, origin.area != 'm')$MedianIncome)$p.value

t.test(subset(covs, origin.area == 'm')$veh.pop,
       subset(covs, origin.area != 'm')$veh.pop)$p.value

t.test(subset(covs, origin.area == 'm')$Bachelors,
       subset(covs, origin.area != 'm')$Bachelors)$p.value

t.test(subset(covs, origin.area == 'm')$minorityShare,
       subset(covs, origin.area != 'm')$minorityShare)$p.value

t.test(subset(covs, origin.area == 'm')$transit.score,
       subset(covs, origin.area != 'm')$transit.score)$p.value

t.test(subset(covs, origin.area == 'm')$transit.routes,
       subset(covs, origin.area != 'm')$transit.routes)$p.value

t.test(subset(covs, origin.area == 'm')$walk.score,
       subset(covs, origin.area != 'm')$walk.score)$p.value

t.test(subset(covs, origin.area == 'm')$bike.hubs,
       subset(covs, origin.area != 'm')$bike.hubs)$p.value

t.test(subset(covs, origin.area == 'm')$AggTravelTime,
       subset(covs, origin.area != 'm')$AggTravelTime)$p.value

t.test(subset(covs, origin.area == 'm')$total.school,
       subset(covs, origin.area != 'm')$total.school)$p.value

t.test(subset(covs, origin.area == 'm')$k12,
       subset(covs, origin.area != 'm')$k12)$p.value

t.test(subset(covs, origin.area == 'm')$college,
       subset(covs, origin.area != 'm')$college)$p.value

#-------------------------------------------
#TABLE S3
#-------------------------------------------

table_s3<- matrix(nrow = 5,ncol = 10, dimnames = list(c("Effect Size","SE", "p-value", "F-stat", "n"),
                                                      c("1 day","2 days", "3 days", "4 days", "5 days", "10 days", "15 days", "20 days", "25 days", "28 days")))

table_s3[1,] <- c(round(coeffs[1],3), round(coeffs[2],3), round(coeffs[3],3), round(coeffs[4],3), round(coeffs[5],3), round(coeffs[10],3), round(coeffs[15],3), round(coeffs[20],3), round(coeffs[25],3), round(coeffs[28],3))
table_s3[2,] <- c(round(se[1],3), round(se[2],3), round(se[3],3), round(se[4],3), round(se[5],3), round(se[10],3), round(se[15],3), round(se[20],3), round(se[25],3), round(se[28],3))
table_s3[3,] <- c(round(p[1],3), round(p[2],3), round(p[3],3), round(p[4],3), round(p[5],3), round(p[10],3), round(p[15],3), round(p[20],3), round(p[25],3), round(p[28],3))
table_s3[4,] <- c(f[1], f[2], f[3], f[4], f[5], f[10], f[15], f[20], f[25], f[28])
table_s3[5,] <- c(observations[1], observations[2], observations[3], observations[4], observations[5], observations[10], observations[15], observations[20], observations[25], observations[28])

#-------------------------------------------
# Figure 2b
#-------------------------------------------

coeffs_marta <- c()
ci_low_marta <- c()
ci_high_marta <- c()
se_marta<- c()
p_marta<- c()
observations_marta <- c()
f_marta = c()
start <- as.Date("2019-08-10")
for(i in 0:43) {
  print(start)
  subset <- MARTA[MARTA$Date <= start,]
  reg_marta <- lm_robust(Travel.Time.per.Mile..Minutes.Mile. ~ treatment + post + post*treatment +  Mon + Wed + Thur + Fri + Sat + Sun + Vehicles + TransRoutes + WalkScore + NumBikeshare + school.population  + event + Precip, data = subset, se_type = "stata")
  start <- start + 1
  coeffs_marta <- c(coeffs_marta, unname(reg_marta$coefficients[["treatment:post"]]))
  ci_low_marta <- c(ci_low_marta, unname(reg_marta$conf.low[["treatment:post"]]))
  ci_high_marta <- c(ci_high_marta, unname(reg_marta$conf.high[["treatment:post"]]))
  se_marta<- c(se_marta, unname(reg_marta$std.error[["treatment:post"]]))
  p_marta<- c(p_marta, unname(reg_marta$p.value[["treatment:post"]]))
  f_marta<- c(f_marta, summary(reg_marta)$fstatistic[["value"]])
  observations_marta <- c(observations_marta, nrow(subset))
}

effects_over_time_marta <- data.frame(ci_low_marta, ci_high_marta, coeffs_marta)

fig2b <- ggplot(effects_over_time_marta, aes(index(effects_over_time_marta))) + 
  geom_ribbon(
    aes(ymin = ci_low_marta, ymax = ci_high_marta), fill = "salmon", alpha = 0.2) + 
  geom_line(aes(y = coeffs_marta), color = "black", linetype=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size = 20, margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(size = 20, margin = unit(c(0, 3, 0, 0), "mm")),
        axis.text = element_text(size = 18)) +
  geom_line(aes(y = ci_low_marta), color = "black", linetype = 3) +
  geom_line(aes(y = ci_high_marta), color = "black", linetype = 3) + 
  xlab("Days Since Policy Implementation") +
  ylab("Effect Size") +
  coord_cartesian(xlim =c(3, 43)) +
  scale_x_continuous(breaks=c(0, 5,10, 15, 20, 25,30,35,40))+
  scale_y_continuous(limits = c(-0.7,1.2), breaks=c(-0.4,-0.2, 0, 0.2, 0.4, 0.6, 0.8, 1))+
  geom_hline(yintercept=0, linetype = 1)


fig2b

#-------------------------------------------
#TABLE S4
#-------------------------------------------

table_s4<- matrix(nrow = 5,ncol = 13, dimnames = list(c("Effect Size","SE", "p-value", "F-stat", "n"),
                                                      c("1 day","2 days", "3 days", "4 days", "5 days", "10 days", "15 days", "20 days", "25 days", "30 days", "35 days", "40 days", "44 days")))

table_s4[1,] <- c(round(coeffs_marta[1],3), round(coeffs_marta[2],3), round(coeffs_marta[3],3), round(coeffs_marta[4],3), round(coeffs_marta[5],3), round(coeffs_marta[10],3), round(coeffs_marta[15],3), round(coeffs_marta[20],3), round(coeffs_marta[25],3), round(coeffs_marta[30],3), round(coeffs_marta[35],3), round(coeffs_marta[40],3), round(coeffs_marta[44],3))
table_s4[2,] <- c(round(se_marta[1],3), round(se_marta[2],3), round(se_marta[3],3), round(se_marta[4],3), round(se_marta[5],3), round(se_marta[10],3), round(se_marta[15],3), round(se_marta[20],3), round(se_marta[25],3), round(se_marta[30],3), round(se_marta[35],3), round(se_marta[40],3), round(se_marta[44],3))
table_s4[3,] <- c(round(p_marta[1],3), round(p_marta[2],3), round(p_marta[3],3), round(p_marta[4],3), round(p_marta[5],3), round(p_marta[10],3), round(p_marta[15],3), round(p_marta[20],3), round(p_marta[25],3), round(p_marta[30],3), round(p_marta[35],3), round(p_marta[40],3), round(p_marta[44],3))
table_s4[4,] <- c(f_marta[1], f_marta[2], f_marta[3], f_marta[4], f_marta[5], f_marta[10], f_marta[15], f_marta[20], f_marta[25], f_marta[30], f_marta[35], f_marta[40], f_marta[44])
table_s4[5,] <- c(observations_marta[1], observations_marta[2], observations_marta[3], observations_marta[4], observations_marta[5], observations_marta[10], observations_marta[15], observations_marta[20], observations_marta[25], observations_marta[30], observations_marta[35], observations_marta[40], observations_marta[44])

#-------------------------------------------
#TABLE S6
#-------------------------------------------

# Generate week number variable
MidtownDDD$weeknumber = format(as.Date(MidtownDDD$Date), "%V")

# Robustness to clustering in Midtown DD
S6_DD1 <- felm(as.numeric(traveltime.mileEvening) ~ treated+ post + treated*post +
                        numvehicles + transRoutes + WalkScore +
                        NumBikeshare+ totalschool + Precip | 0 | 0 |origin.tract,
                      data = MidtownDD, cmethod = "cgm2")
summary(S6_DD1)

S6_DD2 <- felm(as.numeric(traveltime.mileEvening) ~ treated+ post + treated*post +
                 numvehicles + transRoutes + WalkScore +
                 NumBikeshare+ totalschool + Precip | 0 | 0 |origin.tract + dayofweek,
               data = MidtownDD, cmethod = "cgm2")
summary(S6_DD2)

S6_DD3 <- felm(as.numeric(traveltime.mileEvening) ~ treated+ post + treated*post +
                 numvehicles + transRoutes + WalkScore +
                 NumBikeshare+ totalschool + Precip | 0 | 0 |origin.tract + weeknumber,
               data = MidtownDD, cmethod = "cgm2")
summary(S6_DD3)

# Robustness to clustering in Midtown DDD

S6_DDD1 <- felm(traveltime.mile ~ treated+ post + scooter + treated*post + treated*scooter+ post*scooter + treated*post*scooter + numvehicles + transit.routes + walk.score + bike.hubs +
                         Precip + totalschool | 0 | 0 | origin.tract,
                       data = MidtownDDD, cmethod = "cgm2")
summary(S6_DDD1)

S6_DDD2 <- felm(traveltime.mile ~ treated+ post + scooter + treated*post + treated*scooter+ post*scooter + treated*post*scooter + numvehicles + transit.routes + walk.score + bike.hubs +
                  Precip + totalschool | 0 | 0 | origin.tract + dayofweek,
                data = MidtownDDD, cmethod = "cgm2")
summary(S6_DDD2)

S6_DDD3 <- felm(traveltime.mile ~ treated+ post + scooter + treated*post + treated*scooter+ post*scooter + treated*post*scooter + numvehicles + transit.routes + walk.score + bike.hubs +
                  Precip + totalschool | 0 | 0 | origin.tract + weeknumber,
                data = MidtownDDD, cmethod = "cgm2")
summary(S6_DDD3)

table_s6<- matrix(nrow = 6,ncol = 2, dimnames = list(c("One-way (origin tract)","SE 1", "Two-way (origin tract and day of week)", "SE 2", "Two-way (origin tract and week number)", "SE 3"),
                                                     c("DD Estimator", "DDD Estimator")))

table_s6[,1] = c(S6_DD1$coefficients["treatedTRUE:postTRUE",],
                 S6_DD1$cse[["treatedTRUE:postTRUE"]],
                 S6_DD2$coefficients["treatedTRUE:postTRUE",],
                 S6_DD2$cse[["treatedTRUE:postTRUE"]],
                 S6_DD3$coefficients["treatedTRUE:postTRUE",],
                 S6_DD3$cse[["treatedTRUE:postTRUE"]])

table_s6[,2] = c(S6_DDD1$coefficients["treatedTRUE:postTRUE:scooterTRUE",],
                 S6_DDD1$cse[["treatedTRUE:postTRUE:scooterTRUE"]],
                 S6_DDD2$coefficients["treatedTRUE:postTRUE:scooterTRUE",],
                 S6_DDD2$cse[["treatedTRUE:postTRUE:scooterTRUE"]],
                 S6_DDD3$coefficients["treatedTRUE:postTRUE:scooterTRUE",],
                 S6_DDD3$cse[["treatedTRUE:postTRUE:scooterTRUE"]])

table_s6 = round(table_s6, 3)

#-------------------------------------------
#TABLE S7
#-------------------------------------------

S7_DD1 = lm_robust(as.numeric(traveltime.mileEvening) ~ treated+ post +
                     treated*post + as.factor(dayofweek) + as.factor(month)+
                     numvehicles + transRoutes + WalkScore + NumBikeshare +
                     Precip,
                          data=MidtownDD, se_type = "stata", clusters=MidtownDD$origin.tract)   

S7_DD2 = lm_robust(as.numeric(traveltime.mileEvening) ~ treated+ post +
                     treated*post + as.factor(dayofweek) + as.factor(month)+
                     numvehicles + transRoutes + WalkScore + NumBikeshare + totalschool +
                     Precip,
                   data=MidtownDD, se_type = "stata", clusters=MidtownDD$origin.tract)

S7_DDD1 <-lm_robust(traveltime.mile ~ treated+ post + scooter + treated*post + treated*scooter+ post*scooter +
                            treated*post*scooter + as.factor(month) + numvehicles +
                            transit.routes + walk.score + bike.hubs +
                            Precip + as.factor(dayofweek), data=MidtownDDD, clusters=origin.tract, se_type = "stata")

S7_DDD2 <-lm_robust(traveltime.mile ~ treated+ post + scooter + treated*post + treated*scooter+ post*scooter +
                      treated*post*scooter + as.factor(month) + numvehicles +
                      transit.routes + walk.score + bike.hubs + totalschool +
                      Precip + as.factor(dayofweek), data=MidtownDDD, clusters=origin.tract, se_type = "stata")

S7_MARTA1 <- lm_robust(Travel.Time.per.Mile..Minutes.Mile. ~ treatment 
                       + post + post*treatment + as.factor(month) + Vehicles 
                       + TransRoutes + WalkScore + NumBikeshare  
                       + event + as.factor(dayofweek), data = MARTA,
                       cluster = as.factor(origin.tract), se_type = "stata")

S7_MARTA2 <- lm_robust(Travel.Time.per.Mile..Minutes.Mile. ~ treatment 
                       + post + post*treatment + as.factor(month) + Vehicles 
                       + TransRoutes + WalkScore + NumBikeshare + school.population  
                       + event + as.factor(dayofweek), data = MARTA,
                       cluster = as.factor(origin.tract), se_type = "stata")

table_s7<- matrix(nrow = 8,ncol = 2, dimnames = list(c("Midtown Experiment, No School Controls",
                                                       "Midtown Experiment, No School Controls (SE)",
                                                       "Midtown Experiment, School Controls",
                                                       "Midtown Experiment, School Controls (SE)",
                                                       "MARTA Experiment, No School Controls",
                                                       "MARTA Experiment, No School Controls (SE)",
                                                       "MARTA Experiment, School Controls",
                                                       "MARTA Experiment, School Controls (SE)"),
                                                     c("DD Estimator", "DDD Estimator")))

table_s7[,1] = c(S7_DD1$coefficients[["treatedTRUE:postTRUE"]],
                 S7_DD1$std.error[["treatedTRUE:postTRUE"]],
                 S7_DD2$coefficients[["treatedTRUE:postTRUE"]],
                 S7_DD2$std.error[["treatedTRUE:postTRUE"]],
                 S7_MARTA1$coefficients[["treatment:post"]],
                 S7_MARTA1$std.error[["treatment:post"]],
                 S7_MARTA2$coefficients[["treatment:post"]],
                 S7_MARTA2$std.error[["treatment:post"]])

table_s7 = round(table_s7, 3)

table_s7[,2] = c(round(S7_DDD1$coefficients[["treatedTRUE:postTRUE:scooterTRUE"]],3),
                 round(S7_DDD1$std.error[["treatedTRUE:postTRUE:scooterTRUE"]],3),
                 round(S7_DDD2$coefficients[["treatedTRUE:postTRUE:scooterTRUE"]],3),
                 round(S7_DDD2$std.error[["treatedTRUE:postTRUE:scooterTRUE"]],3),
                 "--",
                 "--",
                 "--",
                 "--")
#-------------------------------------------
#TABLE S8
#-------------------------------------------

############################
# Placebo Test: 2018
############################

midtownddd_2018 <- read.csv("Supplementary Information/MidtownDDD_2018.csv", na = "empty")

DDD2018_1 <- felm(minutes_per_mile ~ treated+ post + scooter + treated*post + treated*scooter+ post*scooter +
                    treated*post*scooter + as.factor(month) + numvehicles +
                    transit.routes + walk.score + bike.hubs + totalschool + precip +
                    as.factor(day_of_week)| 0 | 0 | origin_tract, data = midtownddd_2018, cmethod = "cgm2")

DDD2018_2 <- felm(minutes_per_mile ~ treated+ post + scooter + treated*post + treated*scooter+ post*scooter +
                    treated*post*scooter + as.factor(month) + numvehicles +
                    transit.routes + walk.score + bike.hubs + totalschool + precip +
                    as.factor(day_of_week)| 0 | 0 | origin_tract + month, data = midtownddd_2018, cmethod = "cgm2")

DDD2018_3 <- felm(minutes_per_mile ~ treated+ post + scooter + treated*post + treated*scooter+ post*scooter +
                    treated*post*scooter + as.factor(month) + numvehicles +
                    transit.routes + walk.score + bike.hubs + totalschool + precip +
                    as.factor(day_of_week)| 0 | 0 | origin_tract + day_of_week, data = midtownddd_2018, cmethod = "cgm2")

table_s8 <- matrix(nrow = 6,ncol = 2, dimnames = list(c("Origin tract", "SE 1", "Origin tract + month",
                                                        "SE 2", "Origin tract + day of week", "SE 3"),
                                                     c("DDD Estimator", "N")))

table_s8[,1] = c(round(DDD2018_1$coefficients["treated:post:scooter",],3),
                 round(DDD2018_1$cse[["treated:post:scooter"]],3),
                 round(DDD2018_2$coefficients["treated:post:scooter",],3),
                 round(DDD2018_2$cse[["treated:post:scooter"]],3),
                 round(DDD2018_3$coefficients["treated:post:scooter",],3),
                 round(DDD2018_3$cse[["treated:post:scooter"]],3))

table_s8[,2] = c(DDD2018_1$N, "--", DDD2018_2$N, "--", DDD2018_3$N, "--")

#-------------------------------------------
#TABLE S9
#-------------------------------------------

############################
# Placebo Test: Midtown (DDD)
############################

MidtownDDD_Placebo = MidtownDDD %>%
  dplyr::mutate(post.placebo = ifelse(Date <= as.Date("2019-07-17"), 0, 1))

MidtownDDD_preonly = subset(MidtownDDD_Placebo, Date < "2019-08-09")

MidtownDDD_P1 <- felm(traveltime.mile ~ treated+ post.placebo + scooter + treated*post.placebo +
                        treated*scooter+ post.placebo*scooter + treated*post.placebo*scooter +
                        numvehicles + transit.routes + walk.score + bike.hubs +
                        Precip + totalschool
                      | 0 | 0 | origin.tract,
                      data = MidtownDDD_preonly, cmethod = "cgm2")
summary(MidtownDDD_P1)

MidtownDDD_P2 <- felm(traveltime.mile ~ treated+ post.placebo + scooter + treated*post.placebo +
                        treated*scooter+ post.placebo*scooter + treated*post.placebo*scooter +
                        numvehicles + transit.routes + walk.score + bike.hubs +
                        Precip + totalschool
                      | 0 | 0 | origin.tract + as.factor(MidtownDDD_preonly$month),
                      data = MidtownDDD_preonly, cmethod = "cgm2")
summary(MidtownDDD_P2)

MidtownDDD_P3 <- felm(traveltime.mile ~ treated+ post.placebo + scooter + treated*post.placebo +
                        treated*scooter+ post.placebo*scooter + treated*post.placebo*scooter +
                        numvehicles + transit.routes + walk.score + bike.hubs +
                        Precip + totalschool
                      | 0 | 0 | origin.tract + as.factor(MidtownDDD_preonly$dayofweek),
                      data = MidtownDDD_preonly, cmethod = "cgm2")
summary(MidtownDDD_P3)

############################
# Placebo Test: Midtown (DD)
############################

MidtownDD_Placebo = MidtownDD %>%
  dplyr::mutate(post.placebo = ifelse(Date <= as.Date("2019-07-17"), 0, 1))

MidtownDD_preonly = subset(MidtownDD_Placebo, Date < "2019-08-09")

MidtownDD_P1 <- felm(as.numeric(traveltime.mileEvening) ~ treated+ post.placebo + treated*post.placebo +
                       numvehicles + transRoutes + WalkScore + NumBikeshare+ totalschool + Precip
                     | 0 | 0 | origin.tract,
                     data = MidtownDD_preonly, cmethod = "cgm2")
summary(MidtownDD_P1)

MidtownDD_P2 <- felm(as.numeric(traveltime.mileEvening) ~ treated+ post.placebo + treated*post.placebo +
                       numvehicles + transRoutes + WalkScore + NumBikeshare+ totalschool + Precip
                     | 0 | 0 | origin.tract + month,
                     data = MidtownDD_preonly, cmethod = "cgm2")
summary(MidtownDD_P2)

MidtownDD_P3 <- felm(as.numeric(traveltime.mileEvening) ~ treated+ post.placebo + treated*post.placebo +
                       numvehicles + transRoutes + WalkScore + NumBikeshare+ totalschool + Precip
                     | 0 | 0 | origin.tract + dayofweek,
                     data = MidtownDD_preonly, cmethod = "cgm2")
summary(MidtownDD_P3)

############################
# Placebo Test: MARTA
############################

MARTA_Placebo = MARTA %>%
  dplyr::mutate(post.placebo = ifelse(Date <= as.Date("2019-07-17"), 0, 1))

MARTA_preonly = subset(MARTA_Placebo, Date < "2019-08-09")

MARTA_P1 <- felm(Travel.Time.per.Mile..Minutes.Mile. ~ treatment + post.placebo +
                   post.placebo*treatment + Vehicles + TransRoutes + WalkScore +
                   NumBikeshare + school.population + event
                 | 0 | 0 | origin.tract,
                 data = MARTA_preonly, cmethod = "cgm2")
summary(MARTA_P1)

MARTA_P2 <- felm(Travel.Time.per.Mile..Minutes.Mile. ~ treatment + post.placebo +
                   post.placebo*treatment + Vehicles + TransRoutes + WalkScore +
                   NumBikeshare + school.population + event
                 | 0 | 0 | origin.tract + month,
                 data = MARTA_preonly, cmethod = "cgm2")
summary(MARTA_P2)

MARTA_P3 <- felm(Travel.Time.per.Mile..Minutes.Mile. ~ treatment + post.placebo +
                   post.placebo*treatment + Vehicles + TransRoutes + WalkScore +
                   NumBikeshare + school.population + event
                 | 0 | 0 | origin.tract + dayofweek,
                 data = MARTA_preonly, cmethod = "cgm2")
summary(MARTA_P3)

table_s9 <- matrix(nrow = 7,ncol = 3, dimnames = list(c("Origin tract", "SE 1", "Origin tract + month", "SE 2",
                                                        "Origin tract + day of week", "SE 3", "N"),
                                                      c("Midtown (DD)", "Midtown (DDD)", "MARTA (DD)")))

table_s9[,1] = c(MidtownDD_P1$coefficients["treatedTRUE:post.placebo",],
                 MidtownDD_P1$cse[["treatedTRUE:post.placebo"]],
                 MidtownDD_P2$coefficients["treatedTRUE:post.placebo",],
                 MidtownDD_P2$cse[["treatedTRUE:post.placebo"]],
                 MidtownDD_P3$coefficients["treatedTRUE:post.placebo",],
                 MidtownDD_P3$cse[["treatedTRUE:post.placebo"]],
                 MidtownDD_P1$N)

table_s9[,2] = c(MidtownDDD_P1$coefficients["treatedTRUE:post.placebo:scooterTRUE",],
                 MidtownDDD_P1$cse[["treatedTRUE:post.placebo:scooterTRUE"]],
                 MidtownDDD_P2$coefficients["treatedTRUE:post.placebo:scooterTRUE",],
                 MidtownDDD_P2$cse[["treatedTRUE:post.placebo:scooterTRUE"]],
                 MidtownDDD_P3$coefficients["treatedTRUE:post.placebo:scooterTRUE",],
                 MidtownDDD_P3$cse[["treatedTRUE:post.placebo:scooterTRUE"]],
                 MidtownDDD_P1$N)

table_s9[,3] = c(MARTA_P1$coefficients["treatment:post.placebo",],
                 MARTA_P1$cse[["treatment:post.placebo"]],
                 MARTA_P2$coefficients["treatment:post.placebo",],
                 MARTA_P2$cse[["treatment:post.placebo"]],
                 MARTA_P3$coefficients["treatment:post.placebo",],
                 MARTA_P3$cse[["treatment:post.placebo"]],
                 MARTA_P1$N)

table_s9 = round(table_s9, 3)
