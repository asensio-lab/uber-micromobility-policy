
#Please place all active files in the active working directory

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

#-------------------------------------------
#Figure 1
#-------------------------------------------

#-------------------------------------------
#Figure 1A
#-------------------------------------------
#All census tracts in the state of Georgia
shapefile <- tracts(state='13')
#All school districts in the state of Georgia 
Atlanta <- school_districts(state = "13")
#Select the census tracts in the City of Atlanta
Atlanta <- subset(Atlanta, (NAME == "Atlanta City School District"))
#Finding tracts in the City of Atlanta
Atlanta.sub <- st_intersection(shapefile, Atlanta)
Atlanta.sub$NAME <- as.numeric(Atlanta.sub$NAME)
#Select all tracts in the City of Atlanta that are not in the treatment area
Atlanta.sub <- Atlanta.sub[Atlanta.sub$NAMELSAD != "Census Tract 4" & Atlanta.sub$NAMELSAD != "Census Tract 5" & Atlanta.sub$NAMELSAD != "Census Tract 5" & Atlanta.sub$NAMELSAD != "Census Tract 10.01" & Atlanta.sub$NAMELSAD != "Census Tract 12.01" & Atlanta.sub$NAMELSAD != "Census Tract 12.02" & Atlanta.sub$NAMELSAD != "Census Tract 11" & Atlanta.sub$NAMELSAD != "Census Tract 13",]

#Tracts name and code
coa <- read.csv("atlantatracts.csv")

#Selecting tracts in county 67 
shapefile.cum <- tracts(state = "13", "67")
#Geometry on cumberland tracts
shapefile.cum <- merge(coa, shapefile.cum, by.x = "name", by.y = "NAMELSAD")

#List of midtowns tracts
midtown <- (shapefile$NAME == 4.00 | shapefile$NAME == 5.00 | shapefile$NAME == 10.01| shapefile$NAME == 12.01| shapefile$NAME == 12.02 | shapefile$NAME == 11.00| shapefile$NAME == 13.00)  
#List of cumberland tracts
cumberland <- (shapefile$NAMELSAD == "Census Tract 303.20" | shapefile$NAME == 303.39 | shapefile$NAME == 303.44| shapefile$NAME == 303.45| shapefile$NAME == 312.07 | shapefile$NAME == 312.11)

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

#Selecting tracts with scooters availability
sandy <- (shapefile$NAME ==101.10  | shapefile$NAME == 102.05 | shapefile$NAME == 102.04| shapefile$NAME == 101.21| shapefile$NAME == 101.22 | shapefile$NAME == 101.23| shapefile$NAME == 102.10 | 
            shapefile$NAME ==102.09  | shapefile$NAME == 102.08 | shapefile$NAME == 101.17| shapefile$NAME == 101.18| shapefile$NAME == 101.19 | shapefile$NAME == 101.20| shapefile$NAME == 101.06 | 
            shapefile$NAME ==101.07  | shapefile$NAME == 101.08 | shapefile$NAME == 102.06| shapefile$NAME == 102.12| shapefile$NAME == 102.11 | shapefile$NAME == 101.13| shapefile$NAME == 101.14) 

shapefile.sandy <- subset(shapefile, sandy)
shapefile.sandy <- subset(shapefile.sandy, shapefile.sandy$COUNTYFP == 121 |shapefile.sandy$COUNTYFP == "089")

shapefile.sandy$Counterfactual<-"Counterfactual - Sandy Springs"

#Buckhead

#Selecting tracts with scooters availability
buck <- (shapefile$NAME ==91.01  | shapefile$NAME == 95.01 | shapefile$NAME == 95.02| shapefile$NAME == 96.01| shapefile$NAME == 100.01 | shapefile$NAME == 100.02)

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


shapefile2 <- tracts(state='13')
Atlanta <- school_districts(state = "13")
Atlanta <- subset(Atlanta, (NAME == "Atlanta City School District"))
Atlanta.sub <- st_intersection(shapefile2, Atlanta)
Atlanta.sub <- Atlanta.sub[Atlanta.sub$NAMELSAD != "Census Tract 4" & Atlanta.sub$NAMELSAD != "Census Tract 5"& 
                             Atlanta.sub$NAMELSAD != "Census Tract 10.01" & Atlanta.sub$NAMELSAD != "Census Tract 12.01" & 
                             Atlanta.sub$NAMELSAD != "Census Tract 12.02" & Atlanta.sub$NAMELSAD != "Census Tract 11" &
                             Atlanta.sub$NAMELSAD != "Census Tract 13"& Atlanta.sub$NAMELSAD != "Census Tract 18"& Atlanta.sub$NAMELSAD != "Census Tract 19"& 
                             Atlanta.sub$NAMELSAD != "Census Tract 21"& Atlanta.sub$NAMELSAD != "Census Tract 28"& Atlanta.sub$NAMELSAD != "Census Tract 35"& 
                             Atlanta.sub$NAMELSAD != "Census Tract 118"& Atlanta.sub$NAMELSAD != "Census Tract 119"& Atlanta.sub$NAMELSAD != "Census Tract 120"& 
                             Atlanta.sub$NAMELSAD != "Census Tract 26",]

destination <- (shapefile2$NAME == 4.00 | shapefile2$NAME == 5.00 | shapefile2$NAME == 10.01| shapefile2$NAME == 12.01| shapefile2$NAME == 12.02 | shapefile2$NAME == 11.00| shapefile2$NAME == 13.00| shapefile2$NAME == 18.00| shapefile2$NAME == 19.00| shapefile2$NAME == 21.00| shapefile2$NAME == 28.00| shapefile2$NAME == 35.00| shapefile2$NAME == 119.00| shapefile2$NAME == 120.00| shapefile2$NAME == 118.00)  
shapefile2$Location <- ifelse(destination,"Destination",ifelse(shapefile2$NAME == 26.00,"Origin","Policy Zone" ))

dev.off()

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
  scale_fill_manual( values = c)+
  theme(axis.title = element_blank())+
  theme(text=element_text(family="Times New Roman", face="bold", size=20))+
  theme(legend.title = element_blank())

figure_1c


#-------------------------------------------
#Figure 1B
#-------------------------------------------

atl.counties3 <- c("121","89","67","63","135","97","151")
shapefile3 <- tracts(state='13',county=atl.counties3)
Atlanta <- school_districts(state = "13")
Atlanta <- subset(Atlanta, (NAME == "Atlanta City School District"))
Atlanta.sub <- st_intersection(shapefile3, Atlanta)
Atlanta.sub <- Atlanta.sub[Atlanta.sub$NAMELSAD != "Census Tract 4" & Atlanta.sub$NAMELSAD != "Census Tract 25" & Atlanta.sub$NAMELSAD != "Census Tract 85" & Atlanta.sub$NAMELSAD != "Census Tract 100.01" & Atlanta.sub$NAMELSAD != "Census Tract 204" & Atlanta.sub$NAMELSAD != "Census Tract 19" & Atlanta.sub$NAMELSAD != "Census Tract 208.01" & Atlanta.sub$NAMELSAD != "Census Tract 35" & Atlanta.sub$NAMELSAD != "Census Tract 26" & Atlanta.sub$NAMELSAD != "Census Tract 81.02" & Atlanta.sub$NAMELSAD != "Census Tract 30" & Atlanta.sub$NAMELSAD != "Census Tract 119" & Atlanta.sub$NAMELSAD != "Census Tract 76.03" & Atlanta.sub$NAMELSAD != "Census Tract 94.03"& Atlanta.sub$NAMELSAD != "Census Tract 94.02" & Atlanta.sub$NAMELSAD != "Census Tract 12.02" & Atlanta.sub$NAMELSAD != "Census Tract 66.02" & Atlanta.sub$NAMELSAD != "Census Tract 42" & Atlanta.sub$NAMELSAD != "Census Tract 40",]

marta <- read.csv("martatracts.csv")

shapefile3.marta <- tracts(state = "13", county = atl.counties3)
MARTA.out <- (shapefile3.marta$NAME == 226 | shapefile3.marta$NAME == 214.05 | shapefile3.marta$NAME == 212.04 | shapefile3.marta$NAME == 123 | shapefile3.marta$NAME == 225 | shapefile3.marta$NAME == 213.01 |shapefile3.marta$NAME == 212.15 |shapefile3.marta$NAME == 231.11 |shapefile3.marta$NAME == 231.13 |shapefile3.marta$NAME == 101.15 |shapefile3.marta$NAME == 101.22 |shapefile3.marta$NAME == 101.1)        
shapefile3.marta <- subset(shapefile3.marta, (MARTA.out|shapefile3.marta$GEOID == 13121010110))
shapefile3.marta$Counterfactual <- "Counterfactual Tract"


lines <- read.csv("Transit_Routes_2019.csv")
lines.shape <- st_read("Transit_Routes_2019.kml")
lines.together <- bind_cols(lines, lines.shape)
lines.together <- subset(lines.together, lines.together$agency_id == "MARTA")
lines.together <- subset(lines.together, lines.together$rte_type == "subway, metro")


MARTA.in <- (shapefile3$NAME == 4.00 | shapefile3$NAME == 25.00 | shapefile3$NAME == 85.00 | shapefile3$NAME == 100.01 | shapefile3$NAME == 204.00 | shapefile3$NAME == 19.00 |shapefile3$NAME == 208.01 |shapefile3$NAME == 35.00 |shapefile3$NAME == 26.00 |shapefile3$NAME == 81.02 |shapefile3$NAME == 30.00 |shapefile3$NAME == 119.00|shapefile3$NAME == 76.03|shapefile3$NAME == 94.03|shapefile3$NAME == 94.02|shapefile3$NAME == 12.02|shapefile3$NAME == 66.02|shapefile3$NAME == 42.00|shapefile3$NAME == 40.00 )        
shapefile3$Counterfactual <- ifelse(MARTA.in, "Treatment Tract", "None")
marta$location <- "MARTA Station"

dev.off()

c <- c("Counterfactual Tract" = "#D55E00", "Treatment Tract"="#56B4E9", "None"="white", "Policy Zone"="grey")

shapefile3.cropped <- st_crop(shapefile3, xmin = -84.2202, xmax = -84.6279,
                              ymin = 33.600, ymax = 34.020)




shapefile3.cropped <- subset(shapefile3.cropped,shapefile3.cropped$NAME != 4.00 & shapefile3.cropped$NAME != 25.00 & shapefile3.cropped$NAME != 85.00 & shapefile3.cropped$NAME != 100.01 & shapefile3.cropped$NAME != 204.00 & shapefile3.cropped$NAME != 19.00 & shapefile3.cropped$NAME != 208.01 & shapefile3.cropped$NAME != 35.00 & shapefile3.cropped$NAME != 26.00 & shapefile3.cropped$NAME != 81.02 & shapefile3.cropped$NAME != 30.00 & shapefile3.cropped$NAME != 119.00 & shapefile3.cropped$NAME != 76.03 & shapefile3.cropped$NAME != 94.03 & shapefile3.cropped$NAME != 94.02 & shapefile3.cropped$NAME != 12.02 & shapefile3.cropped$NAME != 66.02 & shapefile3.cropped$NAME != 42.00 & shapefile3.cropped$NAME != 40.00 &
                               shapefile3.cropped$NAME != 226 & shapefile3.cropped$NAME != 214.05 & shapefile3.cropped$NAME != 212.04 & shapefile3.cropped$NAME != 123 & shapefile3.cropped$NAME != 225 & shapefile3.cropped$NAME != 213.01 & shapefile3.cropped$NAME != 212.15 & shapefile3.cropped$NAME != 231.11 & shapefile3.cropped$NAME != 231.13 & shapefile3.cropped$NAME != 101.15 & shapefile3.cropped$NAME != 101.22 & shapefile3.cropped$NAME != 101.1)
shapefile3.in <- subset(shapefile3, MARTA.in | shapefile3$NAME == 226 | shapefile3$NAME == 214.05 | shapefile3$NAME == 212.04 | shapefile3$NAME == 123 | shapefile3$NAME == 225 | shapefile3$NAME == 213.01 | shapefile3$NAME == 212.15 | shapefile3$NAME == 231.11 | shapefile3$NAME == 231.13 | shapefile3$NAME == 101.15 | shapefile3$NAME == 101.22 | shapefile3$NAME == 101.1)
shaepfile3.in <- subset(shapefile3.in, shapefile3.in$COUNTYFP == 121)
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

MB <- read.csv("MB_final.csv", na = "empty")
MARTA <- read.csv("MARTA_final.csv", na = "empty")
MidtownDD <- read.csv("MidtownDD_final.csv", na = "empty")
MidtownDDD <- read.csv("MidtownDDD_final.csv", na = "empty")

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

#-------------------------------------------
#MARTA
#-------------------------------------------

MARTA_est <- lm_robust(Travel.Time.per.Mile..Minutes.Mile. ~ post*treatment + June + Aug + Sept +  Mon + Wed + Thur + Fri + Sat + Sun + Vehicles + TransRoutes + WalkScore + NumBikeshare + school.population  + event + Precip, data = MARTA, cluster = MARTA$tract )
summary(MARTA_est)

#-------------------------------------------
#MIDTOWN DD
#-------------------------------------------

MidtownDD_est = lm_robust(as.numeric(traveltime.mileEvening) ~ treated+ post + treated*post + as.factor(dayofweek) + as.factor(month)+ numvehicles + transRoutes + WalkScore + NumBikeshare+ totalschool, data=MidtownDD, cluster=MidtownDD$origin.tract)       
summary(MidtownDD_est)

#-------------------------------------------
#MIDTOWN DDD
#-------------------------------------------

MidtownDDD_est<-lm_robust(traveltime.mile ~ treated+ post + scooter + treated*post + treated*scooter+ post*scooter + treated*post*scooter + as.factor(dayofweek) + as.factor(month) + numvehicles + transit.routes + walk.score + bike.hubs , data=MidtownDDD, clusters=origin.tract)
summary(MidtownDDD_est)

#-------------------------------------------
# Figure 2
#-------------------------------------------

coeffs <- c()
ci_low <- c()
ci_high <- c()
se <- c()
p <- c()
start <- as.Date("2019-08-10")
MidtownDDD <- read.csv("MidtownDDD_final.csv", na = "empty")
MidtownDDD$Date<-as.Date(MidtownDDD$Date)
for(i in 0:27) {
  print(start)
  subset <- MidtownDDD[MidtownDDD$Date <= start,]
  reg <- lm_robust(traveltime.mile ~ treated+ post + scooter + treated*post + treated*scooter+ post*scooter + treated*post*scooter + as.factor(month)+ as.factor(dayofweek) + numvehicles + transit.routes + walk.score + bike.hubs , data=subset)
  start <- start + 1
  coeffs <- c(coeffs, unname(reg$coefficients[["treatedTRUE:postTRUE:scooterTRUE"]]))
  ci_low <- c(ci_low, unname(reg$conf.low[["treatedTRUE:postTRUE:scooterTRUE"]]))
  ci_high <- c(ci_high, unname(reg$conf.high[["treatedTRUE:postTRUE:scooterTRUE"]]))
  se <- c(se, unname(reg$std.error[["treatedTRUE:postTRUE:scooterTRUE"]]))
  p <- c(p, unname(reg$p.value[["treatedTRUE:postTRUE:scooterTRUE"]]))
}

effects_over_time <- data.frame(ci_low, ci_high, coeffs)

fig <- ggplot(effects_over_time, aes(index(effects_over_time))) + 
  geom_ribbon(
    aes(ymin = ci_low, ymax = ci_high), fill = "royalblue2", alpha = 0.2) + 
  geom_line(aes(y = coeffs), color = "black", linetype=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                     panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                     axis.title.x = element_text(size = 14, margin = unit(c(3, 0, 0, 0), "mm")),
                                     axis.title.y = element_text(size = 14, margin = unit(c(0, 3, 0, 0), "mm"))) +
  geom_line(aes(y = ci_low), color = "black", linetype = 3) +
  geom_line(aes(y = ci_high), color = "black", linetype = 3) + 
  xlab("Days Since Policy Implementation") +
  ylab("Effect Size") +
  coord_cartesian(xlim =c(2.2, 27)) +
  scale_x_continuous(breaks=c(1, 5,10, 15, 20, 25,30))+
  scale_y_continuous(breaks=c(-0.2, 0, 0.2, 0.4, 0.6, 0.8, 1))+
  geom_hline(yintercept=0, linetype = 1)
  
fig

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
covs = read_csv("covsfinal_MidtownDDD.csv")
table_s2<- matrix(nrow = 11,ncol = 8, dimnames = list(c("Pop","Med Age", "Med Inc", "Vehicles", "Transit score", "No. routes", "Walk score", "No. bike hubs", "Tot enrollment", "K-12", "College"),
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

table_s2[5,] <- c(mean(subset(covs, origin.area == 'm')$transit.score),
                  sd(subset(covs, origin.area == 'm')$transit.score),
                  min(subset(covs, origin.area == 'm')$transit.score), 
                  max(subset(covs, origin.area == 'm')$transit.score),
                  mean(subset(covs, origin.area != 'm')$transit.score),
                  sd(subset(covs, origin.area != 'm')$transit.score),
                  min(subset(covs, origin.area != 'm')$transit.score), 
                  max(subset(covs, origin.area != 'm')$transit.score))

table_s2[6,] <- c(mean(subset(covs, origin.area == 'm')$transit.routes),
                  sd(subset(covs, origin.area == 'm')$transit.routes),
                  min(subset(covs, origin.area == 'm')$transit.routes), 
                  max(subset(covs, origin.area == 'm')$transit.routes),
                  mean(subset(covs, origin.area != 'm')$transit.routes),
                  sd(subset(covs, origin.area != 'm')$transit.routes),
                  min(subset(covs, origin.area != 'm')$transit.routes), 
                  max(subset(covs, origin.area != 'm')$transit.routes))

table_s2[7,] <- c(mean(subset(covs, origin.area == 'm')$walk.score),
                  sd(subset(covs, origin.area == 'm')$walk.score),
                  min(subset(covs, origin.area == 'm')$walk.score), 
                  max(subset(covs, origin.area == 'm')$walk.score),
                  mean(subset(covs, origin.area != 'm')$walk.score),
                  sd(subset(covs, origin.area != 'm')$walk.score),
                  min(subset(covs, origin.area != 'm')$walk.score), 
                  max(subset(covs, origin.area != 'm')$walk.score))

table_s2[8,] <- c(mean(subset(covs, origin.area == 'm')$bike.hubs),
                  sd(subset(covs, origin.area == 'm')$bike.hubs),
                  min(subset(covs, origin.area == 'm')$bike.hubs), 
                  max(subset(covs, origin.area == 'm')$bike.hubs),
                  mean(subset(covs, origin.area != 'm')$bike.hubs),
                  sd(subset(covs, origin.area != 'm')$bike.hubs),
                  min(subset(covs, origin.area != 'm')$bike.hubs), 
                  max(subset(covs, origin.area != 'm')$bike.hubs))

table_s2[9,] <- c(mean(subset(covs, origin.area == 'm')$total.school),
                  sd(subset(covs, origin.area == 'm')$total.school),
                  min(subset(covs, origin.area == 'm')$total.school), 
                  max(subset(covs, origin.area == 'm')$total.school),
                  mean(subset(covs, origin.area != 'm')$total.school),
                  sd(subset(covs, origin.area != 'm')$total.school),
                  min(subset(covs, origin.area != 'm')$total.school), 
                  max(subset(covs, origin.area != 'm')$total.school))

table_s2[10,] <- c(mean(subset(covs, origin.area == 'm')$k12),
                  sd(subset(covs, origin.area == 'm')$k12),
                  min(subset(covs, origin.area == 'm')$k12), 
                  max(subset(covs, origin.area == 'm')$k12),
                  mean(subset(covs, origin.area != 'm')$k12),
                  sd(subset(covs, origin.area != 'm')$k12),
                  min(subset(covs, origin.area != 'm')$k12), 
                  max(subset(covs, origin.area != 'm')$k12))

table_s2[11,] <- c(mean(subset(covs, origin.area == 'm')$college),
                   sd(subset(covs, origin.area == 'm')$college),
                   min(subset(covs, origin.area == 'm')$college), 
                   max(subset(covs, origin.area == 'm')$college),
                   mean(subset(covs, origin.area != 'm')$college),
                   sd(subset(covs, origin.area != 'm')$college),
                   min(subset(covs, origin.area != 'm')$college), 
                   max(subset(covs, origin.area != 'm')$college))

#p-values
t.test(subset(covs, origin.area == 'm')$Population,
       subset(covs, origin.area != 'm')$Population)$p.value

t.test(subset(covs, origin.area == 'm')$MedianAge,
       subset(covs, origin.area != 'm')$MedianAge)$p.value

t.test(subset(covs, origin.area == 'm')$MedianIncome,
       subset(covs, origin.area != 'm')$MedianIncome)$p.value

t.test(subset(covs, origin.area == 'm')$veh.pop,
       subset(covs, origin.area != 'm')$veh.pop)$p.value

t.test(subset(covs, origin.area == 'm')$transit.score,
       subset(covs, origin.area != 'm')$transit.score)$p.value

t.test(subset(covs, origin.area == 'm')$transit.routes,
       subset(covs, origin.area != 'm')$transit.routes)$p.value

t.test(subset(covs, origin.area == 'm')$walk.score,
       subset(covs, origin.area != 'm')$walk.score)$p.value

t.test(subset(covs, origin.area == 'm')$bike.hubs,
       subset(covs, origin.area != 'm')$bike.hubs)$p.value

t.test(subset(covs, origin.area == 'm')$total.school,
       subset(covs, origin.area != 'm')$total.school)$p.value

t.test(subset(covs, origin.area == 'm')$k12,
       subset(covs, origin.area != 'm')$k12)$p.value

t.test(subset(covs, origin.area == 'm')$college,
       subset(covs, origin.area != 'm')$college)$p.value

t.test(subset(covs, origin.area == 'm')$Bachelors,
       subset(covs, origin.area != 'm')$Bachelors)$p.value

t.test(subset(covs, origin.area == 'm')$AggTravelTime,
       subset(covs, origin.area != 'm')$AggTravelTime)$p.value

covs$minorityShare <- (1 - covs$White/covs$Population)

t.test(subset(covs, origin.area == 'm')$minorityShare,
       subset(covs, origin.area != 'm')$minorityShare)$p.value
#-------------------------------------------
#TABLE S4
#-------------------------------------------

table_s4<- matrix(nrow = 3,ncol = 10, dimnames = list(c("Effect Size","SE", "p-value"),
                                                    c("1 day","2 days", "3 days", "4 days", "5 days", "10 days", "15 days", "20 days", "25 days", "28 days")))

table_s4[1,] <- c(round(coeffs[1],3), round(coeffs[2],3), round(coeffs[3],3), round(coeffs[4],3), round(coeffs[5],3), round(coeffs[10],3), round(coeffs[15],3), round(coeffs[20],3), round(coeffs[25],3), round(coeffs[28],3))
table_s4[2,] <- c(round(se[1],3), round(se[2],3), round(se[3],3), round(se[4],3), round(se[5],3), round(se[10],3), round(se[15],3), round(se[20],3), round(se[25],3), round(se[28],3))
table_s4[3,] <- c(round(p[1],3), round(p[2],3), round(p[3],3), round(p[4],3), round(p[5],3), round(p[10],3), round(p[15],3), round(p[20],3), round(p[25],3), round(p[28],3))

#-------------------------------------------
#TABLE 1
#-------------------------------------------

#Creating confidence intervals and total travel time increases
#MErcedes Benz
MB_lowerCI<- MB_est$coefficients[1]-1.96*summary(MB_est)$coefficients[2]
MB_upperCI<- MB_est$coefficients[1]+1.96*summary(MB_est)$coefficients[2]
MB_CI<-str_c("[",round(MB_lowerCI,3),",",round(MB_upperCI,3),"]")
MB_CI_R<-str_c("[",round(MB_lowerCI*13.4,2),",",round(MB_upperCI*13.4,2),"]")

#MARTA
MARTA_lowerCI<- MARTA_est$coefficients['post:treatment']-1.96*MARTA_est$std.error['post:treatment']
MARTA_upperCI<- MARTA_est$coefficients['post:treatment']+1.96*MARTA_est$std.error['post:treatment']
MARTA_CI<-str_c("[",round(MARTA_lowerCI,3),",",round(MARTA_upperCI,3),"]")
MARTA_CI_R<-str_c("[",round(MARTA_lowerCI*13.4,2),",",round(MARTA_upperCI*13.4,2),"]")


#MIDTOWN
Midtown_lowerCI<- MidtownDDD_est$coefficients['treatedTRUE:postTRUE:scooterTRUE']-1.96*MidtownDDD_est$std.error['treatedTRUE:postTRUE:scooterTRUE']
Midtown_upperCI<- MidtownDDD_est$coefficients['treatedTRUE:postTRUE:scooterTRUE']+1.96*MidtownDDD_est$std.error['treatedTRUE:postTRUE:scooterTRUE']
Midtown_CI<-str_c("[",round(Midtown_lowerCI,3),",",round(Midtown_upperCI,3),"]")
Midtown_CI_R<-str_c("[",round(Midtown_lowerCI*13.4,2),",",round(Midtown_upperCI*13.4,2),"]")



table_1<- matrix(nrow = 6,ncol = 4, dimnames = list(c("Midtown Experiment","SE","MARTA Experiment","SE","Mercedes Benz Experiment","SE"),
                                                    c("Diff-indiff Estimator","DDD Estimator", "CI", "Travel Time Increase")))

table_1[1, ] <- c(round(MidtownDD_est$coefficients['treatedTRUE:postTRUE'],3), round(MidtownDDD_est$coefficients['treatedTRUE:postTRUE:scooterTRUE'],3), Midtown_CI, Midtown_CI_R)
table_1[2, ] <- c(round(MidtownDD_est$std.error['treatedTRUE:postTRUE'],3), round(MidtownDDD_est$std.error['treatedTRUE:postTRUE:scooterTRUE'],3), "-", "-")
table_1[3, ] <- c(round(MARTA_est$coefficients['post:treatment'],3), "-", MARTA_CI, MARTA_CI_R)
table_1[4, ] <- c(round(MARTA_est$std.error['post:treatment'],3), "-", "-", "-")
table_1[5, ] <- c(round(MB_est$coefficients[1],3), "-" , MB_CI , MB_CI_R)
table_1[6, ] <- c(round(summary(MB_est)$coefficients[2],3), "-", "-", "-")

table_1

#-------------------------------------------
#Figure S1
#-------------------------------------------

#FIGURE S1A

MidtownDDD$Date<-as.Date(MidtownDDD$Date)

MidtownDDD.subset<- MidtownDDD[MidtownDDD$Date > "2019-07-09" & MidtownDDD$Date < "2019-09-09", ]

summary(MidtownDDD.subset$traveltime.mile)
table(MidtownDDD$Date)

MidtownDDD.subset$origin.area <- factor(MidtownDDD.subset$origin.area, levels = c("b","c","m","s"), labels = c("Buckhead","Cumberland","Midtown","Sandy Springs"))
p <- ggplot(MidtownDDD.subset, aes(y=traveltime.mile, x=Date, group = origin.area, label=dayofweek))+
  theme_bw()+ 
  stat_summary(aes(group=origin.area, fill = origin.area),geom = "ribbon", fun.data = mean_se, alpha=.35, fun.args = list(mult = 1.96))+
  scale_fill_manual(name="treated",values = c("purple","#D55E00","#56B4E9","green1"))+
  theme(text=element_text(family="Helvetica", face="bold", size=20))+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_vline(xintercept = as.numeric(as.Date("2019-08-09")),color="red")+
  theme(
    legend.position = c(.2,.99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6))+
  scale_x_date(date_breaks = "1 week",date_labels=("%b-%d"),limits = as.Date(c('2019-07-12','2019-09-06')))+
  labs(y= "Travel time per mile (min/mi)")
p


#FIGURE S1B

MARTA$Date<-as.Date(MARTA$Date)

MARTA.subset<- MARTA[MARTA$Date > "2019-07-09" & MARTA$Date < "2019-09-09", ]

#Plotting All
MARTA.subset$treatment <- factor(MARTA.subset$treatment, levels = c("1","0"), labels = c("Policy Zone","No-Policy Zone"))
p <- ggplot(MARTA.subset, aes(y=Travel.Time.per.Mile..Minutes.Mile., x=Date, group = treatment, label=DoW))+
  theme_bw()+ 
  stat_summary(aes(group=treatment, fill = treatment),geom = "ribbon", fun.data = mean_se, alpha=.35, fun.args = list(mult = 1.96))+
  geom_vline(xintercept = as.numeric(as.Date("2019-08-09")),color="red")+
  scale_fill_manual(name="treated",values = c("#56B4E9","#D55E00"))+
  theme(text=element_text(family="Helvetica", face="bold", size=20))+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    legend.position = c(.2,.99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(0, 6, 6, 6))+
  scale_x_date(date_breaks = "1 week",date_labels=("%b-%d"),limits = as.Date(c('2019-07-12','2019-09-06')))+
  labs(y= "Travel time per mile (min/mi)")
p


