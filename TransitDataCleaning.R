library(tidyverse)
library(janitor)
library(lubridate)
library(sf)
library(ggplot2)
library(tigris)
library(tidycensus)
library(tidymodels)
library(tidyclust)
library(jsonlite)
library(httr)

# Read in all transportation-related datasets

traffic <- geojsonsf::geojson_sf("data/2017_Traffic_Volume.geojson")  #2017 
commute <- read_csv("data/ACS_Economic_Characteristics_DC_Census_Tract.csv")
bike_lanes <- geojsonsf::geojson_sf("data/Bicycle_Lanes.geojson") 
bus_stops <- read_csv("Metro_Bus_Stops.csv") #2017
metro <- read_csv("data/Metro_Lines_in_DC.csv") 
ped_friendly <- geojsonsf::geojson_sf("data/Pedestrian_Friendliness_Index_Census_Blocks.geojson") 
shared_mobility <- geojsonsf::geojson_sf("data/Shared_Mobility_Preferred_Parking_Zones.geojson") 
taxi_limo <- read_csv("data/Taxi_and_Limousine_Stands_.csv") 
truck_restrict <- geojsonsf::geojson_sf("data/Truck_Restriction.geojson") 
bikeshare <- st_read("data/Walkshed_Bikeshare/Walkshed_Bikeshare.shp") 

# Read in air quality data
air_monitors <- read_csv("Air_Quality_Realtime.csv") 
trees <- read_csv("Urban_Tree_Canopy_by_Census_Block_in_2020.csv")
aq <- read_csv("data/ejscreen.csv")

# Demographic data (API)
census_api_key("b89a4920b06155446b2127cebe34a3122d308ccd")

acs_dc <- get_acs(variables = c("DP02_0001", "DP02_0112", "DP02_0095E", "DP02_0011",
                                "DP03_0001E", "DP03_0019E", "DP03_0020E", "DP03_0021E", "DP03_0022E",
                                "DP03_0025E", "DP03_0119E", "DP03_0108E", "DP03_0113E", "DP05_0037E",
                                "DP05_0038E", "DP05_0044E", "DP05_0071PE"), 
                  state = "DC", geography = "tract", 
                  output = "wide", year = 2017)%>%
  rename("total_households_est" = "DP02_0001E", "homelang_est" = "DP02_0112E",
         "foreignbornpop_est" = "DP02_0095E", "singlemom_est" = "DP02_0011E", "employed_est" = "DP03_0001E",
  #variables below describe how people get to work
         "drovealone_est" = "DP03_0019E", "drovecarpool_est" = "DP03_0020E", "pubtrans_est" = "DP03_0021E",
        "walked_est" = "DP03_0022E", "avgcommutetime_est" = "DP03_0025E", "poverty_est" = "DP03_0119E",
  #These variables describe estimate of labor force that is without health insurance 
  "emp_nohealth_est" = "DP03_0108E", "unemp_nohealth_est" = "DP03_0113E", "race_white" = "DP05_0037E",
  "race_blackAA" = "DP05_0038E", "race_asian" = "DP05_0044E", "race_hispanic" = "DP05_0071PE"
         )%>%
  janitor::clean_names()%>%
  #removing the margin of error variables since we won't be using them
  select(-starts_with("dp"))


#################################################################

# Clean traffic volume
traffic <- traffic %>% 
  clean_names() %>% 
  group_by(routeid) %>% 
  summarize(aadt = mean(aadt)) %>% 
  ungroup %>%
  select(routeid, aadt, geometry) %>% 
  st_transform(crs = 4326) 

# Clean commute
commute <- commute %>% 
  clean_names() %>% 
  select(geoid, dp03_0021e, dp03_0022e, dp03_0025e, dp03_0019e, dp03_0020e) %>% 
  rename("transit_comm" = "dp03_0021e", "walk_comm" = "dp03_0022e", "time_comm" = "dp03_0025e", "car_alone_comm" = "dp03_0019e", "carpool_comm" = "dp03_0020e")

# DP03_0021E = COMMUTING TO WORK: Workers 16 years and over: Public transportation (excluding taxicab)
# DP03_0022E = COMMUTING TO WORK: Workers 16 years and over: Walked
# DP03_0025E = COMMUTING TO WORK: Workers 16 years and over: Mean travel time to work (minutes)
# DP03_0019E = COMMUTING TO WORK: Workers 16 years and over: Car, truck, or van -- drove alone
# DP03_0020E = COMMUTING TO WORK: Workers 16 years and over: Car, truck, or van -- carpooled
  

# Clean bike lanes
bike_lanes <- bike_lanes %>% 
  clean_names() %>% 
  select(totalbikelanes, bikelane_protected, bikelane_dual_protected) %>% 
  mutate(bikelane_protected = if_else(is.na(bikelane_protected) == TRUE, 0, 1)) %>% 
  mutate(bikelane_dual_protected = if_else(is.na(bikelane_dual_protected) == TRUE, 0, 1))


# Clean bus stops
bus_stops <- bus_stops %>% 
  clean_names %>% 
  st_as_sf(coords = c("bstp_lon", "bstp_lat"), crs = st_crs(4326)) %>% 
  mutate(bstp_has_bkrs = if_else(bstp_has_bkrs == "N", 0, 1)) %>% 
  #changing bstp_bnh_cnt to bstp_bst_tcd because INS doesn't exist in the former variable
  mutate(shelter = if_else(bstp_bst_tcd == "INS", 1, 0)) %>% 
  mutate(bstp_bnh_cnt = if_else(bstp_bnh_cnt < 0, 0, 1)) %>% 
  mutate(school_stop = if_else(bstp_bst_tcd == "SCH", 1, 0)) %>% 
  mutate(clean_eff_date = ymd_hms(bstp_eff_date))%>%
  mutate(year = year(clean_eff_date))%>%
  select("bstp_has_bkrs","shelter", "bstp_bnh_cnt", "school_stop", "year")
 

# SCH = school stop
# BSTP_BNH_CNT = bench count
# INS = inside shelter
# CIRC = DC Circulator
# BSTP_HAS_BKRS = bus stop has bike racks
  
# Clean metro
metro <- metro %>% 
  clean_names() %>% 
  st_as_sf(coords = c("x", "y"), crs = st_crs(4326)) %>% 
  select(name, line) %>% 
  mutate(red = if_else(grepl("red", line), 1, 0), 
         green = if_else(grepl("green", line), 1, 0), 
         blue = if_else(grepl("blue", line), 1, 0), 
         orange = if_else(grepl("orange", line), 1, 0), 
         silver = if_else(grepl("silver", line), 1, 0), 
         yellow = if_else(grepl("yellow", line), 1, 0), 
         ) %>% 
  mutate(num_lines = red + green + blue + orange + silver + yellow)
  

# Clean taxi_limo
taxi_limo <- taxi_limo %>% 
  clean_names() %>% 
  st_as_sf(coords = c("x", "y"), crs = st_crs(4326)) %>% 
  select(taxi_capacity, limo_capacity)

# Clean pedestrian friendliness
ped_friendly <- ped_friendly %>% 
  clean_names() %>% 
  select(density_score, four_way_intersection_ratio, sidewalk_score, pfi_score)

# Clean shared mobility zones
shared_mobility <- shared_mobility %>% 
  clean_names() %>% 
  select(gis_id)

# Clean truck restrictions
truck_restrict <- truck_restrict %>% 
  clean_names() %>% 
  mutate(restrict1 = if_else(is.na(over_1and1qtr_ton_not_allowed) == TRUE, 0, 1)) %>% 
  mutate(restrict2 = if_else(is.na(over_2_axles_not_allowed) == TRUE, 1, 0)) %>% 
  mutate(local_deliv = if_else(is.na(local_deliveries_allowed) == TRUE, 0, 1)) %>% 
  select(restrict1, restrict2, local_deliv)

# Clean bikeshare

#visualize existing bikeshare walksheds
bikeshare%>%
  ggplot()+
  geom_sf()+
  labs(title = "test")+
  theme_void()

# Clean DC monitor air quality- create a POSIXct time variable, then create year, month, date, and time variables from it. 
# After that, remove unnecessary variables and the non-POSIXct date variable
air_monitors <- air_monitors %>% 
  janitor::clean_names() %>%
  mutate(datetime_clean = ymd_hms(datetime_local))%>%
  mutate(year = year(datetime_clean))%>%
  mutate(day = wday(datetime_clean, label = FALSE))%>%
  mutate(month = month(datetime_clean, label = FALSE))%>%
  mutate(time = hour(datetime_clean))%>%
  select(-datasource, -objectid, -aqsid, -datetime_local, -datetime_clean)%>%
  drop_na()


# Clean EJ Screen data on air quality
aq <- aq %>% 
  clean_names() %>% 
  filter(st_abbrev == "DC") %>% 
  select(id, st_abbrev, pm25) %>% 
  rename("geoid" = "id")


# Clean urban tree canopy. I left statefp20 in since it might be useful for joining to other datasets
# I removed all possible planting areas since increasing the tree canopy isn't our primary focus
trees <- trees%>%
  select(-OBJECTID, -COUNTYFP20, -NAME20, -MTFCC20, -FUNCSTAT20, -PPA_V_AC, -PPA_V_PCT, -TO_PPA_AC,
         -TO_PPA_PCT, -PPA_IA_AC, -PPA_IA_PCT, -UN_V_AC, -UN_V_PCT, -TO_UN_AC, -TO_UN_PCT, -UN_IA_AC, -UN_IA_PCT)

# Clean demographic data


#################################################################
  
# Overview Graph: Traffic volume
dc <- states(progress_bar = FALSE) %>%
  filter(STUSPS == "DC")

ggplot() +
  geom_sf(data = dc) +
  geom_sf(data = traffic, aes(color = aadt)) +
  labs(title = "Annual Average Daily Traffic") +
  theme_void()
  
# Summary: Commute 
summary(commute)

# Summary: Bike lanes
summary(bike_lanes)

# Summary: Bus stops
dc <- states(progress_bar = FALSE) %>%
  filter(STUSPS == "DC")

md <- counties("MD", progress_bar = FALSE) %>%
  filter(COUNTYFP == "031" | COUNTYFP == "033") 

va <- counties("VA", progress_bar = FALSE) %>%
  filter(COUNTYFP == "013" |COUNTYFP == "059" | COUNTYFP == "510")  

ggplot() +
  geom_sf(data = dc) +
  geom_sf(data = md) +
  geom_sf(data = va) +
  geom_sf(data = bus_stops, aes(color = bstp_has_bkrs)) +
  labs(title = "Bike at Bus Stop") +
  theme_void()

# Summary: Metro
dc <- states(progress_bar = FALSE) %>%
  filter(STUSPS == "DC")

ggplot() +
  geom_sf(data = dc) +
  geom_sf(data = metro, aes(color = line)) +
  labs(title = "Metro Lines") +
  theme_void()

# Summary: Taxi and Limo Stands
dc <- states(progress_bar = FALSE) %>%
  filter(STUSPS == "DC")

ggplot() +
  geom_sf(data = dc) +
  geom_sf(data = taxi_limo) +
  labs(title = "Taxi and Limo Stands") +
  theme_void()


##########################################################################

#Spatial joins to get counts within each census tract
dc_tracts <- tracts(state = 11) %>% 
  st_transform(crs = 4326) %>% 
  clean_names() %>% 
  select(geoid)

traffic_tract <- st_join(dc_tracts, traffic, join = st_intersects) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  group_by(geoid) %>% 
  summarize(aadt = mean(aadt)) %>% 
  ungroup 
 

metro_tract <- st_join(dc_tracts, metro, join = st_intersects) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  group_by(geoid) %>% 
  summarize(num_lines = sum(num_lines)) %>% 
  ungroup 
  

bike_tract <- st_join(dc_tracts, bike_lanes, join = st_intersects) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  group_by(geoid) %>% 
  summarize(total_bike_lanes = sum(totalbikelanes), bikelane_protected = mean(bikelane_protected), bikelane_dual_protected = mean(bikelane_dual_protected)) %>% 
  ungroup 

bus_tract <- st_join(dc_tracts, bus_stops, join = st_intersects) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  group_by(geoid) %>% 
  summarize(bstp_has_bkrs = mean(bstp_has_bkrs), shelter = mean(shelter), bstp_bnh_cnt = mean(bstp_bnh_cnt), school_stop = sum(school_stop)) %>% 
  ungroup 


sharedmob_tract <- st_join(dc_tracts, shared_mobility, join = st_intersects) %>% 
  mutate(gis_id = if_else(is.na(gis_id) == TRUE, 0, 1)) %>% 
  group_by(geoid) %>% 
  summarize(shared_mobility = sum(gis_id)) %>% 
  ungroup


taxi_tract <- st_join(dc_tracts, taxi_limo, join = st_intersects) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  group_by(geoid) %>% 
  summarize(taxi_capacity = sum(taxi_capacity), limo_capacity = sum(limo_capacity)) %>% 
  ungroup


# Return to joining these!
# notruck_tract <- st_join(dc_tracts, truck_restrict, join = st_intersects)
# ped_tract <- st_join(dc_tracts, ped_friendly, join = st_intersects)


# Merge all tract-level data
transit_merge <- merge(as_tibble(metro_tract), as_tibble(bus_tract))
transit_merge <- merge(transit_merge, aq)
transit_merge <- merge(transit_merge, as_tibble(traffic_tract))
transit_merge <- merge(transit_merge, commute)
transit_merge <- merge(transit_merge, as_tibble(bike_tract))
transit_merge <- merge(transit_merge, as_tibble(sharedmob_tract))
transit_merge <- merge(transit_merge, as_tibble(taxi_tract))

# Export to csv
write_csv(transit_merge, "finalproject.csv")

