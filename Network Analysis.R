## Network Analysis

library(od)
library(igraph)
library(tidyverse)
library(RColorBrewer)
library(sf)
library(ggmap)
library(geosphere)
library(gravity)
library(stargazer)
library(lmtest)
library(zoo)
library(corrplot)
library(ggcorrplot)
library(car)
library(caTools)
library(quantmod)
library(GGally)

summary(a_ftr$fm_dens)

plot(a_ftr$geometry, main = "Food Market Density in AMLC", reset = F)
plot(a_ftr[which(a_ftr$fm_dens < 0.701544),]$geometry, 
     col = "red", add = T)
plot(a_ftr[which(a_ftr$fm_dens > 0.701544 & a_ftr$fm_dens < 1.620719),]$geometry, 
     col = "yellow", add = T)
plot(a_ftr[which(a_ftr$fm_dens > 1.620719 & a_ftr$fm_dens < 3.225389),]$geometry, 
     col = "green" ,add = T)
plot(a_ftr[which(a_ftr$fm_dens > 3.225389),]$geometry, 
     col = "blue", add = T)

legend(x = "bottomleft", title = "Quantil of the Food Market Density",
       legend = c("Microzones bellow the 1st Quantil",
                  "Microzones between the 1st and 2nd Quantil", 
                  "Microzones between the 2st and 3nd Quantil",
                  "Microzones above the 3th Quantil"), 
       fill = c("Red","Blue","Black", "Green", "Black"),
       cex = 0.5, 
       bty = "n")


#### Table with different datasets
nrow(D_b_a_frt[which(D_b_a_frt$flow_per_day >0 ),])/nrow(D_b_a[which(D_b_a$flow_per_day >0 ),])

nrow(D_b_a_frt)/nrow(D_b_a)

################################################################################
################################################################################
#### Gravity Model


# D_b_a_frt Data-set
View(D_b_a_frt)
names(D_b_a_frt)

## Checking Multicollianity
D_b_a_frt[which(D_b_a_frt$flow_per_day > 0 &
                  D_b_a_frt$average_cost > 0 &
                  D_b_a_frt$Average_time_2 > 0),] %>%
  dplyr::select(where(is.numeric) & -Distrito &
                  -Distrito_opposed & 
                  -job_total & 
                  -PZASESC & 
                  -pop_total & 
                  -Veh_total & 
                  - job_total_opposed & 
                  -PZASESC_opposed & 
                  -Veh_total_opposed)  %>% 
  mutate_if(is.numeric,log) %>% 
  cor() %>% 
  ggcorrplot(method = "circle", hc.order = TRUE, type = "lower",
             insig = "blank")

#VIF Values
x_dependent <- D_b_a_frt[which(D_b_a_frt$flow_per_day > 0 &
                  D_b_a_frt$average_cost > 0 &
                  D_b_a_frt$Average_time_2 > 0),] %>% 
  dplyr::select(where(is.numeric) & -Distrito &
                  -Distrito_opposed & 
                  -job_total & 
                  -PZASESC & 
                  -pop_total & 
                  -Veh_total & 
                  - job_total_opposed & 
                  -PZASESC_opposed & 
                  -Veh_total_opposed) %>% 
  mutate_if(is.numeric,log)

x_cor <- cor(x_dependent)
x_inv <- ginv(x_cor)      


colnames(x_inv) <- colnames(x_dependent)
rownames(x_inv) <- colnames(x_dependent)

corrplot(x_inv,method='number',is.corr = F, main = "VIF of D_b_a_frt Data-set")  

## Correlations and Correlations significance
D_b_a_frt[which(D_b_a_frt$flow_per_day > 0 &
                  D_b_a_frt$average_cost > 0 &
                  D_b_a_frt$Average_time_2 > 0),] %>% 
  dplyr::select(where(is.numeric) & -Distrito &
                  -Distrito_opposed & 
                  -job_total & 
                  -PZASESC & 
                  -pop_total & 
                  -Veh_total & 
                  - job_total_opposed & 
                  -PZASESC_opposed & 
                  -Veh_total_opposed) %>% mutate_if(is.numeric,log) %>% 
  ggpairs()


# First Approach
#First Scenario
g_m1 <- lm(log(flow_per_day) ~ 
             log(distance) +
             log(fm_dens) + 
             log(fm_dens_opposed),
           data = D_b_a_frt[which(D_b_a_frt$flow_per_day > 0 &
                                      D_b_a_frt$average_cost > 0 &
                                      D_b_a_frt$Average_time_2 > 0),], 
           na.action = na.omit) 
summary(g_m1)
#plot(g_m1)

# Multicollinearität
vif(g_m1)  %>% barplot(main = "VIF Values", horiz = TRUE, col = "steelblue")

#Heteroskedasticity Prove and Fix
D_b_a_frt_pro_het<- D_b_a_frt[which(D_b_a_frt$flow_per_day > 0 &
                                      D_b_a_frt$average_cost > 0 &
                                      D_b_a_frt$Average_time_2 > 0),] %>% na.omit()

D_b_a_frt_pro_het$red_mode_1 <- g_m1$residuals

# Grafics Reconozation
ggplot(data = D_b_a_frt_pro_het, 
        aes(x = log(fm_dens_opposed), y =red_mode_1)) +  geom_point(col = 'blue') +
  geom_abline(slope = 0)

# Grafics Reconozation
ggplot(data = D_b_a_frt_pro_het, 
       aes(x = log(fm_dens), y =red_mode_1)) +  geom_point(col = 'blue') +
  geom_abline(slope = 0)

# Grafics Reconozation
ggplot(data = D_b_a_frt_pro_het, 
       aes(x = log(distance), y =red_mode_1)) +  geom_point(col = 'blue') +
  geom_abline(slope = 0)

# The Breusch-Pagan Test
bptest(g_m1)

# Autocorrelation
lmtest::dwtest(g_m1)

# Checking Standart Errors
library(sandwich)

summary(g_m1)
coeftest(g_m1, vcov = vcovHC(g_m1, "HC1")) 

# 
varfunc.ols <- lm(log(red_mode_1^2) ~  
                    log(distance) +
                    log(fm_dens) + 
                    log(fm_dens_opposed), data = D_b_a_frt_pro_het)
summary(varfunc.ols)
plot(varfunc.ols)


D_b_a_frt_pro_het$varfunc <- exp(varfunc.ols$fitted.values)

g_m1.gls <- lm(log(flow_per_day) ~ 
                 log(distance) +
                 log(fm_dens) + 
                 log(fm_dens_opposed), 
               weights = 1/sqrt(varfunc), 
               data = D_b_a_frt_pro_het)

summary(g_m1.gls)

plot(g_m1.gls)

# Second Scenario
g_m2 <- lm(log(flow_per_day) ~ 
             log(distance) +
             log(pop_dens) + 
             log(pop_dens_opposed),
           data = D_b_a_frt[which(D_b_a_frt$flow_per_day > 0 & 
                                D_b_a_frt$average_cost > 0 &
                                D_b_a_frt$Average_time_2 > 0),], 
           na.action = na.omit)

summary(g_m2)

# Multicolinearitätscheck
vif(g_m2) %>% barplot(main = "VIF Values", horiz = TRUE, col = "steelblue")

#Heteroskedasticity Prove and Fix
D_b_a_frt_pro_het<- D_b_a_frt[which(D_b_a_frt$flow_per_day > 0 &
                                      D_b_a_frt$average_cost > 0 &
                                      D_b_a_frt$Average_time_2 > 0),] %>% na.omit()

D_b_a_frt_pro_het$red_mode_2 <- g_m2$residuals

# Grafics Reconozation
ggplot(data = D_b_a_frt_pro_het, 
       aes(x = log(pop_dens_opposed), y =red_mode_2)) +  geom_point(col = 'blue') +
  geom_abline(slope = 0)

# Grafics Reconozation
ggplot(data = D_b_a_frt_pro_het, 
       aes(x = log(pop_dens), y =red_mode_2)) +  geom_point(col = 'blue') +
  geom_abline(slope = 0)

# Grafics Reconozation
ggplot(data = D_b_a_frt_pro_het, 
       aes(x = log(distance), y =red_mode_2)) +  geom_point(col = 'blue') +
  geom_abline(slope = 0)


# The Breusch-Pagan Test
bptest(g_m2)

# Autocorrelation Check
lmtest::dwtest(g_m2)


# Checking Standart Errors
library(sandwich)
coeftest(g_m2, vcov = vcovHC(g_m2)) 

# 
varfunc_g_m2.ols <- lm(log(red_mode_2^2) ~  
                    log(distance) +
                    log(pop_dens) + 
                    log(pop_dens_opposed), data = D_b_a_frt_pro_het)
summary(varfunc_g_m2.ols)
plot(varfunc_g_m2.ols)


D_b_a_frt_pro_het$varfunc_model2 <- exp(varfunc_g_m2.ols$fitted.values)

g_m2.gls <- lm(log(flow_per_day) ~ 
                 log(distance) +
                 log(pop_dens) + 
                 log(pop_dens_opposed), 
               weights = 1/sqrt(varfunc_model2), 
               data = D_b_a_frt_pro_het)

summary(g_m2.gls)
plot(g_m2.gls)

summary(g_m2)

# Second Approach
# first Scenario
g_m3 <- lm(log(flow_per_day) ~ 
             log(distance) +
             log(fm_dens) + 
             log(fm_dens_opposed) +
             log(pop_dens) +
             log(pop_dens_opposed) +
             log(Area_total) +
             log(Area_total_aposed),
           data = D_b_a[which(D_b_a$flow_per_day > 0 & 
                                D_b_a$average_cost > 0 &
                                D_b_a$Average_time_2 > 0),], 
           na.action = na.omit)

summary(g_m3)
barplot(vif(g_m3), main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)


# The Breusch-Pagan Test
bptest(g_m3)

#Autocorelation 
lmtest::dwtest(g_m3)

#Heteroskedasticity Prove and Fix
D_b_a_pro_het<- D_b_a[which(D_b_a$flow_per_day > 0 &
                                      D_b_a$average_cost > 0 &
                                      D_b_a$Average_time_2 > 0),] %>% na.omit()

D_b_a_pro_het$red_mode_1 <- g_m3$residuals


# Checking Standart Errors
library(sandwich)
coeftest(g_m3, vcov = vcovHC(g_m3)) 

# 
varfunc_g_m3.ols <- lm(log(red_mode_1^2) ~ 
                         log(distance) +
                         log(fm_dens) + 
                         log(fm_dens_opposed) +
                         log(pop_dens) +
                         log(pop_dens_opposed) +
                         log(Area_total) +
                         log(Area_total_aposed), data = D_b_a_pro_het)
summary(varfunc_g_m3.ols)
plot(varfunc_g_m3.ols)


D_b_a_pro_het$varfunc_model1 <- exp(varfunc_g_m3.ols$fitted.values)

g_m3.gls <- lm(log(flow_per_day) ~ 
                 log(distance) +
                 log(fm_dens) + 
                 log(fm_dens_opposed) +
                 log(pop_dens) +
                 log(pop_dens_opposed) +
                 log(Area_total) +
                 log(Area_total_aposed), 
               weights = 1/sqrt(varfunc_model1), 
               data = D_b_a_pro_het)

summary(g_m3.gls)
plot(g_m2.gls)

summary(g_m3)



# LATEX
stargazer(g_m1, g_m2, g_m3, title="Results", align=TRUE, no.space = T)

########## Gravity Package ########
library(gravity)

#Poisson Pseudo Maximum Likelihood (PPML)
g_m2_ppml <- ppml(dependent_variable = "flow_per_day", 
                  distance = "distance", 
                  additional_regressors = c("fm_dens", 
                                            "fm_dens_opposed", 
                                            "pop_dens",
                                            "pop_dens_opposed"),data = D_b_a)

summary(g_m2_ppml)

#Gamma Pseudo Maximum Likelihood (GPML)
g_m2_gpml <- gpml(dependent_variable = "flow_per_day",
                 distance = "distance", 
                 additional_regressors = c("fm_dens",
                                           "fm_dens_opposed", 
                                           "pop_dens",
                                           "pop_dens_opposed"), 
                 data = D_b_a)


summary(g_m2_gpml)

#Non-linear Least Squares (NLS)

g_m2_nls <- nls(dependent_variable = "flow_per_day",
                  distance = "distance", 
                  additional_regressors = c("fm_dens",
                                            "fm_dens_opposed", 
                                            "pop_dens",
                                            "pop_dens_opposed"), 
                  data = D_b_a)


summary(g_m2_nls)

# LATEX
stargazer(g_m2_ppml, g_m2_gpml, 
          g_m2_nls, titel = "Results", align=TRUE, no.space = T)


################################################################################
################################################################################

#### O-D Plot
# Generating centroid and filtering Junk data
ZT_centroids <- a_ftr %>% st_drop_geometry() %>% 
  select(TRAFFICZON, center_GEO) %>% st_as_sf()

# All Travel Reasons ##########
od_data_all <- D_b_a %>% 
  select(Origin, Destiny, flow_per_day) %>% 
  filter(flow_per_day > 0)

# Generating Lines
odc_all <- od_to_sf(od_data_all, ZT_centroids)

# Only Shoppin as Travel Reason ##########
od_data_fm <- D_b_a_frt %>% select(Origin, Destiny, flow_per_day) %>% 
  filter(flow_per_day > 0)

# Generating Lines
odc_fm <- od_to_sf(od_data_fm, ZT_centroids)


# Plotting OD-MATRIX
################################################################################
################################################################################

# Two figures in one Plot
par(mfrow=c(1,2))

# Considering all travel reasons
plot(ZT[which(ZT$REGION == 1),]$geometry, reset = F,
     main = "Strongest Movement Flow \n considering all 
     Travel Reasons from Microzones \n 3301 & 3304", cex.main=0.8)
plot(ZT[which(ZT$REGION == 2),]$geometry, add= T)
plot(D_b_a_frt[which(D_b_a_frt$Origin == "3301"),]$geometry, 
     col  = "black", add= T)
plot(D_b_a_frt[which(D_b_a_frt$Origin == "3304"),]$geometry, 
     col = "black", add= T)

#Microzones 3301
unique(odc_all[which(odc_all$Origin == "3301"),]$flow_per_day)

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


#Microzones 3304
unique(odc_all[which(odc_all$Origin == "3304"),]$flow_per_day)

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
# Food Markets
plot(fm_p$geometry, pch = 20, cex = 0.1,  col = "green", add = T)
# Centroids
plot(a_ftr$center_GEO, pch = 20, cex = 0.1, add = T)

#Legend
legend(x = "bottomleft", title = "Legend",
       legend = c("Mobility Flow form 3301",
                  "Mobility Flow form 3304", 
                  "Food Market",
                  "Centroid of each Microzone",
                  "Microzone Border"), 
       lty = c(1,1,NA,NA,1), 
       pch = c(NA, NA, 20, 20),
       col = c("Red","Blue","Black", "Green", "Black"),
       cex = 0.5, 
       bty = "n")

# Considering only shopping as travel reasons
plot(ZT[which(ZT$REGION == 1),]$geometry, reset = F,
     main = "Strongest Movement Flow \n considering only Shopping  
     from Microzones \n 3301 & 3304", cex.main=0.8)
plot(ZT[which(ZT$REGION == 2),]$geometry, add= T)
plot(D_b_a_frt[which(D_b_a_frt$Origin == "3301"),]$geometry, 
     col  = "black", add= T)
plot(D_b_a_frt[which(D_b_a_frt$Origin == "3304"),]$geometry, 
     col = "black", add= T)

#Microzones 3301
unique(odc_fm[which(odc_fm$Origin == "3301"),]$flow_per_day)

plot(odc_fm[which(odc_fm$Origin == "3301" & odc_fm$flow_per_day == 36),]$geometry, 
     col = "red", lwd = 4, add = T)
plot(odc_fm[which(odc_fm$Origin == "3301" & odc_fm$flow_per_day < 4),]$geometry, 
     col = "red", lwd = 0.5, add = T)

#Microzones 3304
unique(odc_fm[which(odc_fm$Origin == "3304"),]$flow_per_day)

plot(odc_fm[which(odc_fm$Origin == "3304" & odc_fm$flow_per_day < 6),]$geometry, 
     col = "blue", lwd = 1, add = T)

# Food Markets
plot(fm_p$geometry, pch = 20, cex = 0.1, col = "green", add = T)
# Centroids
plot(a_ftr$center_GEO, pch = 20, cex = 0.09, add = T)

#Legend
legend(x = "bottomleft", title = "Legend",
       legend = c("Mobility Flow form 3301",
                  "Mobility Flow form 3304", 
                  "Food Market",
                  "Centroid of each Microzone",
                  "Microzone Border"), 
       lty = c(1,1,NA,NA,1), 
       pch = c(NA, NA, 20, 20),
       col = c("Red","Blue","Black", "Green", "Black"),
       cex = 0.5, 
       bty = "n")
