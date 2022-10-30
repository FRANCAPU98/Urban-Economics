## Cleaning up Environment
rm(list = ls())


library(sf)
library(tidyverse)
library(readxl)
library(stringr)
library(sp)

# SPATIAL STATISTICS
#library(rgdal)
library(rgdal)
library(raster)
library(spatstat)
library(maptools)
library(MASS)
library(ks)
library(reshape2)
library(ggspatial)


# Geodaten Einlesen ############################################################
# SHP of the Microzones
ZT <- read_sf(file.choose())
colnames(ZT)
head(ZT)
View(ZT)


# Notation Standarization puting a 0 before each number
ZT$TRAFFICZON <- str_pad(ZT$TRAFFICZON, 4, side = c("left"),pad = 0)

# Socio demographic features fo the Microzones
features <- read_excel(file.choose(),range = "A2:G1010", col_names = T)
colnames(features)
View(features)

################################################################################
# Minimarkets GEO-Data ######
fm_p <- read_sf(file.choose())
colnames(fm_p)
View(fm_p)
# ZT SHP for background plotting

#Test
plot(ZT$geometry, reset = F)                                         #Original SHP
plot(a_ftr$geometry, border = "red", add = T)             #Map after regrouping
plot(a_ftr$center_GEO, pch = 20, cex = 0.1,col = "blue", add = T) # Centroids
plot(fm_p$geometry, pch = 20, cex = 0.1, col = "green",add = T) #Food_Markets
legend(x = "bottomleft", title = "Legend",
       legend = c("Studied Microzone Border",
                  "Centroids of each Microzone", 
                  "Food Market"), 
       lty = c(1, NA, NA), pch = c(NA,20,20), col = c("red","blue","green"),
       cex = 0.5,
       bty = "n")


######## 
# Smooth 
# Convert points into ppp object
fm_p_ppp <- as.ppp(fm_p)
#Convert ZT into owin + add window to ppp_points.
Window(fm_p_ppp) <- as.owin(ZT)

par(mar = rep(0, 4))
plot(fm_p_ppp, main = "",cols=rgb(0,0,0,.2), pch=20, legend = F)
density_spatstat <- density(fm_p_ppp, dimyx = 256)
density_stars <- stars::st_as_stars(density_spatstat)
density_sf <- st_as_sf(density_stars) %>%
  st_set_crs(32718)
plot(density_sf)
ggplot(density_sf) + geom_sf(aes(fill = v))


# General 
density_sf <- density.ppp(fm_p_ppp, edge = T, sigma = 1000) %>%
  stars::st_as_stars() %>% st_as_sf() %>% 
  st_set_crs(32718) %>% ggplot() +
  geom_sf(data = density_sf, aes(fill = v), col = NA) +
  scale_fill_viridis_c(option = 'magma', direction = -1) +
  geom_sf(data = st_boundary(ZT), fill = NA) +
  geom_sf(data = fm_p, size = 0.1, col = "black") + theme_void()+
  ggtitle("2d Density Plot of the Food Markets in Lima, Peru", 
          subtitle = "Based on the data given by INEI") +
  theme(panel.grid.major =
          element_line(color = gray(0.5), linetype = "dashed", 
                       size = 0.5), panel.background = 
          element_rect(fill = "aliceblue"))



density.ppp(fm_p_ppp, edge = T, sigma = 1000) %>%
  stars::st_as_stars() %>% st_as_sf() %>% 
  st_set_crs(32718) %>% ggplot() +
  geom_sf(aes(fill = v), col = NA) +
  scale_fill_viridis_c(option = 'magma', direction = -1) +
  geom_sf(data = st_boundary(ZT), fill = NA) +
  geom_sf(data = fm_p, size = 0.1, col = "black") + theme_void() + 
  labs(fill = expression ("Food Market Density in"~km^2)) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  ggtitle("2d Density Plot of the Food Markets in Lima, Peru", 
          subtitle = "Based on the data given by INEI") +
  xlab("Longitude") + ylab("Latitude") +
  theme(panel.grid.major = element_line(color = gray(0.5),
                                        linetype = "dashed", 
                                        size = 0.5), panel.background = 
          element_rect(fill = "aliceblue"))

ggplot() +  geom_sf(data = st_boundary(ZT), fill = NA) +
  geom_sf(data = fm_p, aes(col = P29), size = 0.5) + theme_void()


# Table Generalization #########################################################
## Number of Minimarkets for each microzone #classification of minimarket type
fm_p_pro_ZT <- st_join(ZT, fm_p) #%>% 
#  mutate(cla =if_else(P29 == "Minorista","1",
#                 if_else(P29 == "Mixto", "2",
#                         if_else(P29 == "Mayorista", "3", "NA")))) 
View(ZT)
View(fm_p)
View(fm_p_pro_ZT)

# Data to work in "a_ftr" ZONA701ASU deduced from the Data observation. 
# Observation and unification and summary of rebundacies
a_ftr <- ZT %>% group_by(TRAFFICZON, ZONA701ASU) %>% 
  summarise(Area = sum(AREA)) %>% 
  right_join(., features, by = c("ZONA701ASU" = "ZT_LIMA")) %>%
  group_by(TRAFFICZON, DISTRITO) %>% 
  summarise(across(c("Area", "POB","EMPLEOS","EMPLEOS", "PZASESC", "VEH"), 
                   ~ sum(.x, na.rm = TRUE))) %>% 
  right_join(., fm_p_pro_ZT, by = "TRAFFICZON") %>% 
  mutate(pop_dens = POB/Area ,fm_dens = Number_fm/Area) %>% 
  na.omit(TRAFFICZON)

names(a_ftr)
View(a_ftr)

#TEST to corraborate the correspondance of the characteristics
anti_join(ZT, features, by = c("ZONA701ASU" = "ZT_LIMA"))
table(is.na(a_ftr$TRAFFICZON))
which(is.na(a_ftr), arr.ind=TRUE)

# Geocenter
a_ftr$center_GEO <- st_centroid(a_ftr$geometry)

# TEST PLOTING
#General
plot(ZT$geometry)
plot(a_ftr$geometry, border = "green", add = T)
plot(a_ftr$center_GEO, pch = 20, cex = 0.3,col = "blue", add = T)


# A given District
plot(a_ftr$geometry[which(a_ftr$DISTRITO == 33 ),], col = "green", reset = F)
plot(a_ftr$center_GEO, pch = 20, cex = 0.9, add = T)
plot(fm_p$geometry, col = "orange", pch = 20, cex = 0.9,add = T)

## SOME MICROZONES ARE GONE, BUT WE HAVE THE SOCIO DEMOGRAPHIC DATA OF THE BIG 
# POLYGONONS, WHICH IS GOOD IN OUR DATASET ONLY 3 MICROZONES ARE GONE.




################################################################################
################################################################################
################################################################################
# Calculate the travel intensity (mobility flow) and filter by "Proposito"
#Checking the Dataset
trvl_clean <- read_csv(file.choose())
View(trvl_clean)
colnames(trvl_clean)
head(trvl_clean)
nrow(trvl_clean)

## put "0" in each NA of P30_01 & P33_01 before manipulating data
trvl_clean$P30_01 <- str_pad(trvl_clean$P30_01, 4, side = c("left"),pad = 0)
trvl_clean$P33_01<- str_pad(trvl_clean$P33_01, 4, side = c("left"),pad = 0)

### Mobility Flow and averages GENERAL TRIP
mov_flu <- trvl_clean %>% mutate_all(~replace(., is.na(.),0)) %>% 
  group_by(P30_01,P33_01) %>% 
  summarise(flow_per_day = n(), 
            average_cost = sum(Cost_1, Cost_2, Cost_3, Cost_4, 
                               Cost_5, Cost_6, Cost_7)/flow_per_day, 
            Average_time_2 = sum(Tiempo_MIN_1, Tiempo_MIN_2, Tiempo_MIN_3,
                                 Tiempo_MIN_4, Tiempo_MIN_5, Tiempo_MIN_6, 
                                 Tiempo_MIN_7)/flow_per_day) %>%     
  filter(P30_01 != P33_01) %>%                            
  unite_(col = "Mov_Flow_ID", c("P30_01","P33_01"),      
         sep = "_") %>% 
  distinct(Mov_Flow_ID,.keep_all = T)

#Test
nrow(mov_flu)
colnames(mov_flu)

# Mobility Flow and averages ONLY SHOPPING
mov_flu_fm <- trvl_clean %>% mutate_all(~replace(., is.na(.),0)) %>%
  filter(proposito == "Compras"|destino_lugar == "Comercial") %>% 
  group_by(P30_01,P33_01) %>% 
  summarise(flow_per_day = n(), 
            average_cost = sum(Cost_1, Cost_2, Cost_3, Cost_4, 
                               Cost_5, Cost_6, Cost_7)/flow_per_day, 
            Average_time_2 = sum(Tiempo_MIN_1, Tiempo_MIN_2, Tiempo_MIN_3,
                                 Tiempo_MIN_4, Tiempo_MIN_5, Tiempo_MIN_6, 
                                 Tiempo_MIN_7)/flow_per_day) %>%                                                
  filter(P30_01 != P33_01) %>%                                            
  unite_(col = "Mov_Flow_ID", c("P30_01","P33_01"),                       
         sep = "_") %>% 
  distinct(Mov_Flow_ID,.keep_all = T)

#Test
nrow(mov_flu_fm)
colnames(mov_flu_fm)


# Euclidean Distance to other points and combination with the Data above
# without filtering
D_b_a <- st_distance(a_ftr$center_GEO, a_ftr$center_GEO) %>% as.tibble() %>% 
  `colnames<-`(c(a_ftr$TRAFFICZON)) %>% 
  add_column(TRAFFICZON = a_ftr$TRAFFICZON) %>%
  relocate(TRAFFICZON) %>% 
  pivot_longer(cols = -TRAFFICZON, names_to = "TRAFFICZON_oposed",
               values_to = "distance") %>% 
  filter(TRAFFICZON < TRAFFICZON_oposed) %>% 
  mutate(distance = as.numeric(distance)) %>%  
  unite_(col = "Mov_Flow_ID", c("TRAFFICZON","TRAFFICZON_oposed")) %>% 
  left_join(., mov_flu, by = "Mov_Flow_ID") %>% 
  separate(Mov_Flow_ID, c("Origin", "Destiny"), remove = F)  %>%
  left_join(., a_ftr, by = c("Origin" = "TRAFFICZON")) %>% 
  left_join(., a_ftr, by = c("Destiny" = "TRAFFICZON")) %>% 
  rename(Distrito = DISTRITO.x, 
         Area_total = Area.x, 
         pop_total = POB.x, 
         job_total = EMPLEOS.x, 
         PZASESC = PZASESC.x, 
         Veh_total= VEH.x, 
         pop_dens = pop_dens.x, 
         geometry = geometry.x,
         Number_fm = Number_fm.x,
         fm_dens = fm_dens.x,
         center_GEO  = center_GEO.x,
         Distrito_opposed = DISTRITO.y, 
         Area_total_aposed = Area.y, 
         pop_total_oppoed = POB.y, 
         job_total_opposed =EMPLEOS.y, 
         PZASESC_opposed = PZASESC.y, 
         Veh_total_opposed= VEH.y, 
         pop_dens_opposed = pop_dens.y, 
         geometry_opposed = geometry.y, 
         fm_dens_opposed = fm_dens.y, 
         Number_fm_opposed= Number_fm.y,
         center_GEO_opposed  = center_GEO.y) %>% 
  mutate_all(~replace(., is.na(.),0))


#TEST
names(D_b_a)
View(D_b_a)
which(is.na(D_b_a), arr.ind=TRUE)
summary(D_b_a)

# Save
#write_csv(D_b_a, "D_b_a.cvs")

# with filtering ONLY SUPER MARKETS
D_b_a_frt <- st_distance(a_ftr$center_GEO, a_ftr$center_GEO) %>% as.tibble() %>% 
  `colnames<-`(c(a_ftr$TRAFFICZON)) %>% 
  add_column(TRAFFICZON = a_ftr$TRAFFICZON) %>%
  relocate(TRAFFICZON) %>% 
  pivot_longer(cols = -TRAFFICZON, names_to = "TRAFFICZON_oposed",
               values_to = "distance") %>% 
  filter(TRAFFICZON < TRAFFICZON_oposed) %>% 
  mutate(distance = as.numeric(distance)) %>%  
  unite_(col = "Mov_Flow_ID", c("TRAFFICZON","TRAFFICZON_oposed")) %>% 
  left_join(., mov_flu_fm, by = "Mov_Flow_ID") %>% 
  separate(Mov_Flow_ID, c("Origin", "Destiny"), remove = F)  %>%
  left_join(., a_ftr, by = c("Origin" = "TRAFFICZON")) %>% 
  left_join(., a_ftr, by = c("Destiny" = "TRAFFICZON")) %>% 
  rename(Distrito = DISTRITO.x, 
         Area_total = Area.x, 
         pop_total = POB.x, 
         job_total = EMPLEOS.x, 
         PZASESC = PZASESC.x, 
         Veh_total= VEH.x, 
         pop_dens = pop_dens.x, 
         geometry = geometry.x,
         Number_fm = Number_fm.x,
         fm_dens = fm_dens.x,
         center_GEO  = center_GEO.x,
         Distrito_opposed = DISTRITO.y, 
         Area_total_aposed = Area.y, 
         pop_total_oppoed = POB.y, 
         job_total_opposed =EMPLEOS.y, 
         PZASESC_opposed = PZASESC.y, 
         Veh_total_opposed= VEH.y, 
         pop_dens_opposed = pop_dens.y, 
         geometry_opposed = geometry.y, 
         fm_dens_opposed = fm_dens.y, 
         Number_fm_opposed= Number_fm.y,
         center_GEO_opposed  = center_GEO.y) %>% 
  mutate_all(~replace(., is.na(.),0))

#TEST
names(D_b_a_frt)
nrow(D_b_a_frt)

#write_csv(D_b_a_frt, "D_b_a_frt.csv")





