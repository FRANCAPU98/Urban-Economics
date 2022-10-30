
murder <- subset(crime, offense == "murder")
qmplot(lon, lat, data = murder, colour = I('red'), size = I(3), darken = .3)
?qmplot

View(murder)


set.seed(500)
df <- round(data.frame(
  x = jitter(rep(-95.36, 50), amount = .3),
  y = jitter(rep( 29.76, 50), amount = .3)
), digits = 2)
map <- get_googlemap('houston', markers = df, path = df, scale = 2)
ggmap(map, extent = 'device')


# Mit Open-Street Maps

AMLC_GEO <- get_stamenmap(
  bbox = c(left = -77.4110, bottom = -12.3085, right = -76.6626, top = -11.8156), 
  maptype = "terrain", crop = F,
  zoom = 11)
ggmap(AMLC_GEO)

#O
ZT_ORIGIN <- ZT %>% select(ID,TRAFFICZON, DISTRITO, REGION, 
                           DIST, geometry,center_GEO)
colnames(ZT_ORIGIN)[colnames(ZT_ORIGIN) == "TRAFFICZON"] <- "Origin"

mov_flu_ORIGIN <- mov_flu %>% select(1,2,4) %>% 
  left_join(., ZT_ORIGIN, by = c("Origin")) %>% 
  select(Mov_Flow_ID, Origin, mov_conc, DISTRITO, REGION, center_GEO, geometry) %>% 
  rename_with(~ gsub("center_GEO", "Center_GEO_Depart", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("geometry", "geometry_Depart", .x, fixed = TRUE)) %>% 
  na.omit()

colnames(mov_flu_ORIGIN)
# Prove with anti_join() <- External ZT that are not in AMLC  are 25 excluded

provin_1 <- anti_join(mov_flu, ZT_ORIGIN,  by = c("Origin"))
length(table(provin_1$Origin))/length(table(mov_flu$Origin))

#D
ZT_DESTINY <- ZT %>% select(ID,TRAFFICZON, DISTRITO, REGION, 
                            DIST, geometry,center_GEO)
colnames(ZT_DESTINY)[colnames(ZT_DESTINY) == "TRAFFICZON"] <- "Destiny"

mov_flu_DESTINY <- mov_flu[-c(2)] %>% 
  left_join(., ZT_DESTINY, by = c("Destiny")) %>% 
  select(Mov_Flow_ID, Destiny, mov_conc, DISTRITO, REGION, center_GEO, geometry) %>% 
  rename_with(~ gsub("center_GEO", "Center_GEO_Arrive", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("geometry", "geometry_Arrive", .x, fixed = TRUE)) %>% 
  na.omit()

# Prove with anti_join() <- External ZT that are not in AMLC are27 excluded
provin_2 <- anti_join(mov_flu, ZT_DESTINY,  by = c("Destiny"))
length(table(provin_2$Destiny))/length(table(mov_flu$Destiny))

# Georefeernzing 
colnames(mov_flu_ORIGIN)
nrow(mov_flu_ORIGIN)
colnames(mov_flu_DESTINY)
nrow(mov_flu_DESTINY)


OD_Matrix_Points <- full_join(mov_flu_ORIGIN[-c(3,4,5)], mov_flu_DESTINY, 
                              by = c("Mov_Flow_ID")) %>% 
  na.omit()

colnames(OD_Matrix_Points)
anti_join(mov_flu_ORIGIN[-c(3,4,5)], mov_flu_DESTINY, 
          by = c("Mov_Flow_ID"))

View(OD_Matrix_Points)
nrow(OD_Matrix_Points)



# Graphing

ZT[which(ZT$REGION == 1),] %>% ggplot() + 
  geom_sf() + theme_void()


#Prueba
plot(ZT[which(ZT$REGION == 1),]$geometry, reset = F)
plot(mov_flu_ORIGIN[which(mov_flu_ORIGIN$DISTRITO == 42),]$geometry,col = "red", add = T)
plot(mov_flu_ORIGIN[which(mov_flu_ORIGIN$Origin == 4202),]$geometry,col = "blue", add = T)

trvl_depart <- trvl_clean %>% 
  rename_with(~ gsub("P30_01", "TRAFFICZON", .x, fixed = TRUE)) %>% 
  group_by(TRAFFICZON) %>% 
  summarise(number_Arr = n()) %>% left_join(ZT[,c(6,8,11,16)], 
                                            by = "TRAFFICZON") %>% distinct()


anti_join(trvl_depart, ZT)

View(trvl_depart)
colnames(trvl_depart)

st_as_sf(trvl_depart[which(trvl_depart$REGION == 2),]) %>% ggplot()+
  geom_sf(aes(fill = number_Arr), show.legend = NA) + coord_sf(datum = NA) 

?geom_sf




install.packages("mapview")



mapview(st_as_sf(mov_flu_ORIGIN[which(mov_flu_ORIGIN$REGION == 1),]), 
        col.regions = mov_conc)

?mapview




##### First approach for point density plot
ggplot() +
  stat_density_2d(data = fm_p, 
                  mapping = aes(x = purrr::map_dbl(geometry, ~.[1]),
                                y = purrr::map_dbl(geometry, ~.[2]),
                                fill = stat(density)),
                  geom = 'tile',
                  contour = FALSE,
                  alpha = 0.8) +
  geom_sf(data = ZT, fill = NA) + 
  #  geom_sf(data = fm_p, color = 'red') + 
  scale_fill_viridis_c(option = 'magma', direction = -1) +
  theme_bw()


?geom_density_2d

##### Raster Data

plot(fm_p$geometry, pch = 20)
names(fm_p)
head(fm_p)


ggplot(ZT) + geom_sf(colour = "black", fill = NA) + 
  geom_sf(aes(colour = P29), size = 0.5, data = fm_p) + theme_bw()


fm_p_kde_hexagonal <- create_grid_hexagonal(fm_p, cell_size = 1000)

fm_p_kde <- kde(fm_p, band_width = 15000 ,grid = fm_p_kde_hexagonal)

fm_p_raster <- create_raster(fm_p, cell_size = 100)



plot(fm_p_raster)
plot(fm_p_kde)
?create_raster

?kde2d



# Convert points into ppp object
fm_p_ppp <- as.ppp(fm_p)


#Convert ZT into owin + add window to ppp_points.
Window(fm_p_ppp) <- as.owin(ZT)

par(mar = rep(0, 4))
plot(fm_p_ppp, main = "",cols=rgb(0,0,0,.2), pch=20, legend = F)


density_spatstat <- density(fm_p_ppp, dimyx = 256, sigma=900)
density_stars <- stars::st_as_stars(density_spatstat)

density_sf <- st_as_sf(density_stars) %>%
  st_set_crs(32718)

ggplot() +
  geom_sf(data = density_sf, aes(fill = v), col = NA) +
  scale_fill_viridis_c(option = 'magma', direction = -1) +
  geom_sf(data = st_boundary(ZT)) +
  geom_sf(data = fm_p, size = 0.9, col = "black") + theme_void()



# 2dDensity Plot
fm_p_ppp <- as.ppp(fm_p)

density_fm_lima <- density.ppp(fm_p_ppp, sigma = 100)

plot(density_fm_lima)

ggplot() + 
  
  
  qq <- quadratcount(fm_p_ppp, nx = 10, ny = 10)
plot(qq)


