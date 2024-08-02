#############################################################################################################
#############################################################################################################

## ---------------------------
##
## OSM Data Pre-Processing
##
## Author: Dra. Daria Dementeva
##
##
## Email: daria.dementeva@kuleuven.be
##
## ---------------------------
##
##   2022-2023
##
## ---------------------------

## Set Working Directory 

# setwd("...")  

# For Reproducibility

set.seed(05081997)

# Install packages
# Uncomment if necessary
# install.packages("BelgiumMaps.StatBel")
# install.packages("data.table")
# install.packages("sf")
# install.packages("sp")
# install.packages("tidyverse")

#############################################################################################################
#############################################################################################################


# Load Packages

library(BelgiumMaps.StatBel)
library(data.table)
library(sf)
library(sp)
library(tidyverse)

#############################################################################################################
#############################################################################################################


## 1. Reading in Spatial Data 

data(BE_ADMIN_SECTORS)
# class(BE_ADMIN_SECTORS)

#############################################################################################################
#############################################################################################################

## 2. Extract OSM data

data_landuse <- st_read("/Users/dariadementeva/Desktop/Bel.shp", # change the path for another OSM shapefile
                        layer = "gis_osm_landuse_a_free_1")

# colSums(is.na(data_landuse))

data_natural <- st_read("/Users/dariadementeva/Desktop/Bel.shp",
                        layer = "gis_osm_natural_a_free_1")

# colSums(is.na(data_natural))

data_transport <- st_read("/Users/dariadementeva/Desktop/Bel.shp",
                          layer = "gis_osm_transport_free_1")

# colSums(is.na(data_transport))

data_roads <- st_read("/Users/dariadementeva/Desktop/Bel.shp",
                      layer = "gis_osm_roads_free_1")

# colSums(is.na(data_roads))

data_pofw <- st_read("/Users/dariadementeva/Desktop/Bel.shp",
                     layer = 'gis_osm_pofw_a_free_1')

# colSums(is.na(data_pofw))

data_pois <- st_read("/Users/dariadementeva/Desktop/Bel.shp",
                     layer = "gis_osm_pois_free_1")

# colSums(is.na(data_pois))

############################################################################################################
#############################################################################################################


## 3. Helper Function to Go Around with the Geometry entry

# Convert sfc_POINT to x/y Columns 

# Adapted from https://github.com/r-spatial/sf/issues/231

sfc_as_cols <- function(x, geometry, names = c("x","y")) {
  if (missing(geometry)) {
    geometry <- sf::st_geometry(x)
  } else {
    geometry <- rlang::eval_tidy(enquo(geometry), x)
  }
  stopifnot(inherits(x,"sf") && inherits(geometry,"sfc_POINT"))
  ret <- sf::st_coordinates(geometry)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}


#############################################################################################################
#############################################################################################################

# NB: longitude is X and latitude is Y

## 4. Project OSM Features over Statistical Sectors

#############################################################################################################
#############################################################################################################

# Landuse Features

data_landuse$centroids <- st_centroid(data_landuse$geometry)

data_landuse_upd <- sfc_as_cols(data_landuse, data_landuse$centroids)

x_long = as.vector(data_landuse_upd$x) # set longitude vector for SpatialPoints format
y_lat =  as.vector(data_landuse_upd$y) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_SECTORS))) # set CRS 
pt.in.poly_landuse <- sp::over(pts, BE_ADMIN_SECTORS, fn = NULL) 

data_landuse_upd_df <- as.data.frame(data_landuse_upd)

data_landuse_overlayed <- cbind(data_landuse_upd_df, pt.in.poly_landuse)


#############################################################################################################
#############################################################################################################

# Natural Features

data_natural$centroids <- st_centroid(data_natural$geometry)

data_natural_upd <- sfc_as_cols(data_natural, data_natural$centroids)

x_long = as.vector(data_natural_upd$x) # set longitude vector for SpatialPoints format
y_lat =  as.vector(data_natural_upd$y) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_SECTORS))) # overwrite CRS once again to be sure
pt.in.poly_natural <- sp::over(pts, BE_ADMIN_SECTORS, fn = NULL) 

data_natural_upd_df <- as.data.frame(data_natural_upd)

data_natural_overlayed <- cbind(data_natural_upd_df, pt.in.poly_natural)


###########################################################################################################
#############################################################################################################

# Transport Features

data_transport_upd <- sfc_as_cols(data_transport, data_transport$geometry)

x_long = as.vector(data_transport_upd$x) # set longitude vector for SpatialPoints format
y_lat =  as.vector(data_transport_upd$y) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_SECTORS))) # overwrite CRS once again to be sure
pt.in.poly_transport <- sp::over(pts, BE_ADMIN_SECTORS, fn = NULL) 

data_transport_upd_df <- as.data.frame(data_transport_upd)

data_transport_overlayed <- cbind(data_transport_upd_df, pt.in.poly_transport)



#############################################################################################################
#############################################################################################################

# Road Features

data_roads$centroids <- st_centroid(data_roads$geometry)

data_roads_upd <- sfc_as_cols(data_roads, data_roads$centroids)

x_long = as.vector(data_roads_upd$x) # set longitude vector for SpatialPoints format
y_lat =  as.vector(data_roads_upd$y) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_SECTORS))) # overwrite CRS once again to be sure
pt.in.poly_roads <- sp::over(pts, BE_ADMIN_SECTORS, fn = NULL) 

data_roads_upd_df <- as.data.frame(data_roads_upd)

data_roads_overlayed <- cbind(data_roads_upd_df, pt.in.poly_roads)

#############################################################################################################
#############################################################################################################

# Points of Worship Features

data_pofw$centroids <- st_centroid(data_pofw$geometry)

data_pofw_upd <- sfc_as_cols(data_pofw, data_pofw$centroids)

x_long = as.vector(data_pofw_upd$x) # set longitude vector for SpatialPoints format
y_lat =  as.vector(data_pofw_upd$y) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_SECTORS))) # overwrite CRS once again to be sure
pt.in.poly_pofw <- sp::over(pts, BE_ADMIN_SECTORS, fn = NULL) 

data_pofw_upd_df <- as.data.frame(data_pofw_upd)

data_pofw_overlayed <- cbind(data_pofw_upd_df, pt.in.poly_pofw)

#############################################################################################################
#############################################################################################################

# Points of Interest Features

data_pois_upd <- sfc_as_cols(data_pois)
x_long = as.vector(data_pois_upd$x) # set longitude vector for SpatialPoints format
y_lat =  as.vector(data_pois_upd$y) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_SECTORS))) # overwrite CRS once again to be sure
pt.in.poly2_pois <- sp::over(pts, BE_ADMIN_SECTORS, fn = NULL) 

data_pois_upd_df <- as.data.frame(data_pois_upd)

data_pois_overlayed <- cbind(data_pois_upd_df, pt.in.poly2_pois)

#############################################################################################################
#############################################################################################################

## 5. R-Bind Data

data_landuse_overlayed_reduced <- subset(data_landuse_overlayed, select = c("osm_id", "code", "fclass", "name", "x", "y", 
                                                                            "CD_REFNIS_SECTOR", 
                                                                            "CD_SECTOR",
                                                                            "TX_SECTOR_DESCR_NL",
                                                                            "TX_SECTOR_DESCR_FR",
                                                                            "CD_MUNTY_REFNIS",
                                                                            "TX_MUNTY_DESCR_NL",
                                                                            "TX_MUNTY_DESCR_FR",
                                                                            "CD_DSTR_REFNIS",
                                                                            "TX_ADM_DSTR_DESCR_NL", 
                                                                            "TX_ADM_DSTR_DESCR_FR",
                                                                            "CD_PROV_REFNIS",
                                                                            "TX_PROV_DESCR_NL",
                                                                            "TX_PROV_DESCR_FR", 
                                                                            "CD_RGN_REFNIS",
                                                                            "TX_RGN_DESCR_NL",
                                                                            "TX_RGN_DESCR_FR"))

data_landuse_overlayed_reduced$fclass <- as.factor(data_landuse_overlayed_reduced$fclass)
levels(data_landuse_overlayed_reduced$fclass)[levels(data_landuse_overlayed_reduced$fclass)=="park"] <- "park_natural"

data_natural_overlayed_reduced <- subset(data_natural_overlayed, select = c("osm_id", "code", "fclass", "name", "x", "y", 
                                                                            "CD_REFNIS_SECTOR", 
                                                                            "CD_SECTOR",
                                                                            "TX_SECTOR_DESCR_NL",
                                                                            "TX_SECTOR_DESCR_FR",
                                                                            "CD_MUNTY_REFNIS",
                                                                            "TX_MUNTY_DESCR_NL",
                                                                            "TX_MUNTY_DESCR_FR",
                                                                            "CD_DSTR_REFNIS",
                                                                            "TX_ADM_DSTR_DESCR_NL", 
                                                                            "TX_ADM_DSTR_DESCR_FR",
                                                                            "CD_PROV_REFNIS",
                                                                            "TX_PROV_DESCR_NL",
                                                                            "TX_PROV_DESCR_FR", 
                                                                            "CD_RGN_REFNIS",
                                                                            "TX_RGN_DESCR_NL",
                                                                            "TX_RGN_DESCR_FR"))

data_transport_overlayed_reduced <- subset(data_transport_overlayed, select = c("osm_id", "code", "fclass", "name", "x", "y", 
                                                                                "CD_REFNIS_SECTOR", 
                                                                                "CD_SECTOR",
                                                                                "TX_SECTOR_DESCR_NL",
                                                                                "TX_SECTOR_DESCR_FR",
                                                                                "CD_MUNTY_REFNIS",
                                                                                "TX_MUNTY_DESCR_NL",
                                                                                "TX_MUNTY_DESCR_FR",
                                                                                "CD_DSTR_REFNIS",
                                                                                "TX_ADM_DSTR_DESCR_NL", 
                                                                                "TX_ADM_DSTR_DESCR_FR",
                                                                                "CD_PROV_REFNIS",
                                                                                "TX_PROV_DESCR_NL",
                                                                                "TX_PROV_DESCR_FR", 
                                                                                "CD_RGN_REFNIS",
                                                                                "TX_RGN_DESCR_NL",
                                                                                "TX_RGN_DESCR_FR"))

data_roads_overlayed_reduced <- subset(data_roads_overlayed, select = c("osm_id", "code", "fclass", "name", "x", "y", 
                                                                        "CD_REFNIS_SECTOR", 
                                                                        "CD_SECTOR",
                                                                        "TX_SECTOR_DESCR_NL",
                                                                        "TX_SECTOR_DESCR_FR",
                                                                        "CD_MUNTY_REFNIS",
                                                                        "TX_MUNTY_DESCR_NL",
                                                                        "TX_MUNTY_DESCR_FR",
                                                                        "CD_DSTR_REFNIS",
                                                                        "TX_ADM_DSTR_DESCR_NL", 
                                                                        "TX_ADM_DSTR_DESCR_FR",
                                                                        "CD_PROV_REFNIS",
                                                                        "TX_PROV_DESCR_NL",
                                                                        "TX_PROV_DESCR_FR", 
                                                                        "CD_RGN_REFNIS",
                                                                        "TX_RGN_DESCR_NL",
                                                                        "TX_RGN_DESCR_FR"))

data_pofw_overlayed_reduced <- subset(data_pofw_overlayed, select = c("osm_id", "code", "fclass", "name", "x", "y", 
                                                                      "CD_REFNIS_SECTOR", 
                                                                      "CD_SECTOR",
                                                                      "TX_SECTOR_DESCR_NL",
                                                                      "TX_SECTOR_DESCR_FR",
                                                                      "CD_MUNTY_REFNIS",
                                                                      "TX_MUNTY_DESCR_NL",
                                                                      "TX_MUNTY_DESCR_FR",
                                                                      "CD_DSTR_REFNIS",
                                                                      "TX_ADM_DSTR_DESCR_NL", 
                                                                      "TX_ADM_DSTR_DESCR_FR",
                                                                      "CD_PROV_REFNIS",
                                                                      "TX_PROV_DESCR_NL",
                                                                      "TX_PROV_DESCR_FR", 
                                                                      "CD_RGN_REFNIS",
                                                                      "TX_RGN_DESCR_NL",
                                                                      "TX_RGN_DESCR_FR"))

data_pois_overlayed_reduced <- subset(data_pois_overlayed, select = c("osm_id", "code", "fclass", "name", "x", "y", 
                                                                      "CD_REFNIS_SECTOR", 
                                                                      "CD_SECTOR",
                                                                      "TX_SECTOR_DESCR_NL",
                                                                      "TX_SECTOR_DESCR_FR",
                                                                      "CD_MUNTY_REFNIS",
                                                                      "TX_MUNTY_DESCR_NL",
                                                                      "TX_MUNTY_DESCR_FR",
                                                                      "CD_DSTR_REFNIS",
                                                                      "TX_ADM_DSTR_DESCR_NL", 
                                                                      "TX_ADM_DSTR_DESCR_FR",
                                                                      "CD_PROV_REFNIS",
                                                                      "TX_PROV_DESCR_NL",
                                                                      "TX_PROV_DESCR_FR", 
                                                                      "CD_RGN_REFNIS",
                                                                      "TX_RGN_DESCR_NL",
                                                                      "TX_RGN_DESCR_FR"))

be_built_environment_data <- rbind(data_landuse_overlayed_reduced,
                                   data_natural_overlayed_reduced, 
                                   data_transport_overlayed_reduced,
                                   data_roads_overlayed_reduced,
                                   data_pofw_overlayed_reduced,
                                   data_pois_overlayed_reduced)

#############################################################################################################
#############################################################################################################

## 5. Summarize and Widen Data

be_built_environment_osm_grouped <- be_built_environment_data %>%
  group_by(CD_REFNIS_SECTOR, TX_SECTOR_DESCR_NL, CD_MUNTY_REFNIS, TX_MUNTY_DESCR_NL, fclass)%>% 
  summarise(n= n())

be_built_environment_osm_grouped_wide <- be_built_environment_osm_grouped  %>%
  pivot_wider(names_from = fclass,
              values_from = n)    

#############################################################################################################
#############################################################################################################

## 6. Create the Typology of Spaces of Encounter

osm_cat_processed_weighted_sums <- be_built_environment_osm_grouped_wide %>% 
  group_by(CD_REFNIS_SECTOR, TX_SECTOR_DESCR_NL) %>% 
  mutate(
    
    Educational_Institutonal_Spaces = sum(kindergarten, school, college, university, 
                                          na.rm = TRUE) * 1/4,  
    
    Functional_Spaces = sum(bench, drinking_water, waste_basket, post_box, toilet, telephone, 
                            camera_surveillance, recycling_glass, recycling_clothes, recycling, recycling_paper, recycling_metal, 
                            na.rm = TRUE) * 1/12, 
    
    Medical_Institutonal_Spaces = sum(dentist, doctors, pharmacy, veterinary, hospital, clinic, 
                                      na.rm = TRUE) * 1/6, 
    
    Money_Related_Institutonal_Spaces = sum(bank, atm, 
                                            na.rm = TRUE) * 1/2, 
    
    Christian_Spaces_of_Worship = sum(christian, christian_catholic, christian_evangelical,
                                      christian_protestant, christian_anglican,christian_orthodox,
                                      christian_lutheran, christian_methodist, 
                                      na.rm = TRUE) * 1/8, 
    
    Muslim_Spaces_of_Worship = sum(muslim, muslim_shia, muslim_sunni, 
                                   na.rm = TRUE) * 1/3, 
    
    Public_Open_Spaces = sum(fountain, artwork, memorial, viewpoint, shelter, monument, 
                             dog_park, park, castle, observation_tower, ruins, archaeological, 
                             beach, cave_entrance, cliff, peak, spring, fort, 
                             graveyard, attraction, 
                             na.rm = TRUE) * 1/20, 
    
    Retail_Spaces = sum(bakery, beauty_shop, bookshop, butcher, clothes, convenience, 
                        florist, hairdresser, laundry, newsagent, optician, travel_agent, 
                        garden_centre, supermarket, car_dealership, bicycle_shop, furniture_shop, doityourself,
                        greengrocer, shoe_shop, kiosk, beverages, bicycle_rental, car_sharing,
                        gift_shop, jeweller, mobile_phone_shop, sports_shop, chemist, stationery,
                        toy_shop, outdoor_shop, computer_shop, car_rental, car_wash, video_shop,
                        general, vending_any, market_place, vending_parking, vending_machine, department_store, 
                        mall,
                        na.rm = TRUE) * 1/43, 
    
    Social_Service_Instituional_Spaces = sum(post_office, police, nursing_home, prison, 
                                             embassy, fire_station, town_hall, courthouse, 
                                             na.rm = TRUE) * 1/8, 
    
    Third_Place_Spaces = sum(fast_food, pub, restaurant, cafe, bar, food_court, 
                             biergarten, library, community_centre, playground, theatre, nightclub, 
                             arts_centre, cinema, museum, zoo, tourist_info, guesthouse, 
                             hostel, hotel, motel, chalet,
                             na.rm = TRUE)* 1/22 , 
    
    User_Selecting_Spaces = sum(golf_course, pitch, camp_site, sports_centre, picnic_site, theme_park,
                                swimming_pool, stadium, 
                                na.rm = TRUE) * 1/8,
    
    Civic_Spaces = sum(cycleway, living_street, pedestrian, unclassified, 
                       na.rm = T) * 1/4, 
    
    Natural_Semi_Natural_Spaces = sum(forest, nature_reserve, park_natural, 
                                      na.rm = T) * 1/3,  # exclude allotments 
    
    Interchange_Spaces = sum(tram_stop, railway_station, bus_stop, ferry_terminal, helipad, taxi, 
                             bus_station, railway_halt, airport, 
                             na.rm = T) * 1/9
  )
