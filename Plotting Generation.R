###PLOT
library(tidyverse)
library(od)
library(sf)
library(readxl)

################################################################################
# Geodaten Einlesen ############################################################
# SHP of the Microzones
ZT <- read_sf(file.choose())

# Socio demographic features fo the Microzones
features <- read_excel(file.choose(),range = "A2:G1010", col_names = T)
View(features)
colnames(features)
nrow(features)

################################################################################
# SHP of Minimarkets ######
fm_p <- read_sf(file.choose())
View(fm_p)

# ZT SHP for background plotting
#Test
plot(ZT$geometry)                                         #Original SHP
plot(a_ftr$geometry, border = "red", add = T)             #Map after regrouping
plot(fm_p$geometry, col = 5, pch = 20, cex = 0.2,add = T) #Food_Markets


################################################################################
# Data Manupulation from Table Generalization ##################################
## Number of Minimarkets for each microzone
fm_p_pro_ZT <- a_ftr %>% 
  st_join(., fm_p) %>%
  group_by(TRAFFICZON) %>% summarise(Number_fm = n()) %>% st_drop_geometry()

# Notation Standardization setting a "0" before each number
ZT$TRAFFICZON <- str_pad(ZT$TRAFFICZON, 4, side = c("left"),pad = 0)

# Data to work in "a_ftr" ZONA701ASU deduced from the Data observation. 
# Observation and unification and summary of rebundacies

a_ftr <- ZT %>% group_by(TRAFFICZON, ZONA701ASU) %>% summarise(Area = sum(AREA)) %>% 
  right_join(., features, by = c("ZONA701ASU" = "ZT_LIMA")) %>%
  group_by(TRAFFICZON, DISTRITO) %>% 
  summarise(across(c("Area", "POB","EMPLEOS","EMPLEOS", "PZASESC", "VEH"), 
                   ~ sum(.x, na.rm = TRUE))) %>% 
  left_join(., fm_p_pro_ZT, by = "TRAFFICZON") %>% 
  mutate(pop_dens = POB/Area ,fm_dens = Number_fm/Area) %>% na.omit(TRAFFICZON)  

# Geocenter
a_ftr$center_GEO <- st_centroid(a_ftr$geometry)


#############################################################
#############################################################
#############################################################
#############################################################
#############################################################

# Data of the mobility survey ##################################################
D_b_a <- read_csv(file.choose()) #  OD Matrix of Lima of all travel 
D_b_a_frt <- read_csv(file.choose()) #  OD Matrix of Lima of only shopping as travel reason

# Generating Centroids and 
ZT_centroids <- a_ftr %>% st_drop_geometry() %>% 
  select(TRAFFICZON, center_GEO) %>% st_as_sf()

# All ##########
od_data_all <- D_b_a %>% select(Origin, Destiny, flow_per_day) %>% 
  filter(flow_per_day > 0)
# Generating Lines
odc_all <- od_to_sf(od_data_all, ZT_centroids)

# Only Shoppin  ##########
od_data_fm <- D_b_a_frt %>% select(Origin, Destiny, flow_per_day) %>% 
  filter(flow_per_day > 0)
# Generating Lines
odc_fm <- od_to_sf(od_data_fm, ZT_centroids)








#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#Plotting OD-MATRIX#############################################################
par(mfrow=c(1,2))

##########################First Image###########################################
# Considering all travel reasons
plot(ZT[which(ZT$REGION == 1),]$geometry, reset = F,
     main = "Strongest Movement Flow \n considering all 
     Travel Reasons from Microzones \n 3301 & 3304", cex.main=0.8)
plot(ZT[which(ZT$REGION == 2),]$geometry, add= T)

plot(D_b_a_frt[which(D_b_a_frt$Origin == "3301"),]$geometry, 
     col  = "black", add= T)
plot(D_b_a_frt[which(D_b_a_frt$Origin == "3304"),]$geometry, 
     col = "black", add= T)

# Food Markets
plot(fm_p$geometry, pch = 20, cex = 0.1, add = T)
# Centroids
plot(a_ftr$center_GEO, pch = 20, cex = 0.02, col = "green", add = T)

#Selected Microzon "3301"
unique(odc_all[which(odc_all$Origin == "3301"),]$flow_per_day)/10

#Mobilitätsströme "lwd muss an der Frequence angepasst werden"
plot(odc_all[which(odc_all$Origin == "3301" & odc_all$flow_per_day == 45),]$geometry, 
     col = "red", lwd = 5, add = T)
plot(odc_all[which(odc_all$Origin == "3301" & odc_all$flow_per_day == 28),]$geometry, 
     col = "red", lwd = 5, add = T)
plot(odc_all[which(odc_all$Origin == "3301" & odc_all$flow_per_day == 21),]$geometry, 
     col = "red", lwd = 5, add = T)
plot(odc_all[which(odc_all$Origin == "3301" & odc_all$flow_per_day == 19),]$geometry, 
     col = "red", lwd = 3.5, add = T)
plot(odc_all[which(odc_all$Origin == "3301" & odc_all$flow_per_day < 9),]$geometry, 
     col = "red", lwd = 0.2, add = T)

#Selected Microzone "3304"
unique(odc_all[which(odc_all$Origin == "3304"),]$flow_per_day)

#Mobilitätsströme "lwd muss an der Frequence angepasst werden"
plot(odc_all[which(odc_all$Origin == "3304" & odc_all$flow_per_day == 24),]$geometry, 
     col = "blue", lwd = 4.2, add = T)
plot(odc_all[which(odc_all$Origin == "3304" & odc_all$flow_per_day == 16),]$geometry, 
     col = "blue", lwd = 3.4, add = T)
plot(odc_all[which(odc_all$Origin == "3304" & odc_all$flow_per_day == 11),]$geometry, 
     col = "blue", lwd = 3, add = T)
plot(odc_all[which(odc_all$Origin == "3304" & odc_all$flow_per_day == 10),]$geometry, 
     col = "blue", lwd = 2.9, add = T)
plot(odc_all[which(odc_all$Origin == "3304" & odc_all$flow_per_day < 10),]$geometry, 
     col = "blue", lwd = 0.2, add = T)

legend(x = "bottomleft", title = "Legend",
       legend = c("Mobility Flow form 3301",
                  "Mobility Flow form 3304", 
                  "Food Market",
                  "Centroid of each Microzone"), 
       lty = c(1,2), col = c("Red","Blue","Black", "Green", "Black"),
       cex = 0.5)


##########################Second Image#########################################
# Considering only "shopping" as travel reasons
plot(ZT[which(ZT$REGION == 1),]$geometry, reset = F,
     main = "Strongest Movement Flow \n considering only Shopping  
     from Microzones \n 3301 & 3304", cex.main=0.8)
plot(ZT[which(ZT$REGION == 2),]$geometry, add= T)
plot(D_b_a_frt[which(D_b_a_frt$Origin == "3301"),]$geometry, 
     col  = "black", add= T)
plot(D_b_a_frt[which(D_b_a_frt$Origin == "3304"),]$geometry, 
     col = "black", add= T)

# Food Markets
plot(fm_p$geometry, pch = 20, cex = 0.1, add = T)
# Centroids
plot(a_ftr$center_GEO, pch = 20, cex = 0.09, col = "green", add = T)

#Microzones "3301"
unique(odc_fm[which(odc_fm$Origin == "3301"),]$flow_per_day)
#Mobilitätsströme "lwd muss an der Frequence angepasst werden"
plot(odc_fm[which(odc_fm$Origin == "3301" & odc_fm$flow_per_day == 36),]$geometry, 
     col = "red", lwd = 4, add = T)
plot(odc_fm[which(odc_fm$Origin == "3301" & odc_fm$flow_per_day < 4),]$geometry, 
     col = "red", lwd = 0.5, add = T)

#Microzones "3304"
unique(odc_fm[which(odc_fm$Origin == "3304"),]$flow_per_day)

#Mobilitätsströme "lwd muss an der Frequence angepasst werden"
plot(odc_fm[which(odc_fm$Origin == "3304" & odc_fm$flow_per_day < 6),]$geometry, 
     col = "blue", lwd = 1, add = T)

#Legende

legend(x = "bottomleft", title = "Legend",
       legend = c("Mobility Flow form 3301",
                  "Mobility Flow form 3304", 
                  "Food Market",
                  "Centroid of each Microzone"), 
       lty = c(1,2), col = c("Red","Blue","Black", "Green", "Black"),
       cex = 0.5)

