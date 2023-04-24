library(tidyverse)
library(janitor)
library(lubridate)
library(sf)
library(ggplot2)
library(tigris)
library(tidycensus)
library(tidymodels)
library(tidyclust)

#Read in all transportation-related datasets
traff_vol <- read_csv("data/2020_Traffic_Volume.csv") #Will need shape file

commute <- read_csv("data/ACS_Economic_Characteristics_DC_Census_Tract.csv") #Census tract-level

bike_lanes <- read_csv("data/Bicycle_Lanes.csv") #Will need shape file

bus_stops <- read_csv("data/Metro_Bus_Stops.csv") #Has coordinates

metro <- read_csv("data/Metro_Lines_in_DC.csv") # Has coordinates

ped_friendly <- read_csv("data/Pedestrian_Friendliness_Index_Census_Blocks.csv") #Will need shape file

shared_mobility <- read_csv("data/Shared_Mobility_Preferred_Parking_Zones.csv") #Will need shape file

taxi_limo <- read_csv("data/Taxi_and_Limousine_Stands_.csv") #Has coordinates

truck_restrict <- read_csv("data/Truck_Restriction.csv") #Will need shape file

bikeshare <- read_csv("data/Walkshed_Bikeshare.csv") #Look into issues with downloading this


# Read in air quality data
air_qual <- read_csv("data/Air_Quality_Realtime.csv") #AQ sensors are only in 5 locations

trees <- read_csv("data/Urban_Tree_Canopy_by_Census_Block_in_2020.csv")

#################################################################

# Clean traffic volume
traff_vol <- traff_vol %>% 
  clean_names() %>% 
  mutate(from_date = date(fromdate)) %>% 
  mutate(to_date = date(todate)) 
  #For analysis, keep: 
    #location info 
    #to/from?

#Clean bike lanes
bike_lanes <- bike_lanes %>% 
  clean_names() 
  #For analysis, keep: 
  #location info 
  #bike lane characteristics 

#Clean bus stops
bus_stops <- bus_stops %>% 
  clean_names() %>% 
  st_as_sf(coords = c("x", "y"), crs = st_crs(4326))

#Clean metro
metro <- metro %>% 
  clean_names() %>% 
  st_as_sf(coords = c("x", "y"), crs = st_crs(4326))

#Clean taxi_limo
taxi_limo <- taxi_limo %>% 
  clean_names() %>% 
  st_as_sf(coords = c("x", "y"), crs = st_crs(4326))

#################################################################
  
# Summary: Traffic volume
traff_vol %>% 
  group_by(from_date) %>% 
  summarize(n()) %>% 
  ungroup

traff_vol %>% 
  group_by(to_date) %>% 
  summarize(n()) %>% 
  ungroup

traff_vol %>% 
  group_by(from_date, to_date) %>% 
  summarize(n()) %>% 
  ungroup

# Summary: Bike lanes
bike_lanes #Look at documentation for what "BD" means

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
  geom_sf(data = taxi_limo, aes(color = type_of_vehicle)) +
  labs(title = "Taxi and Limo Stands") +
  theme_void()


#Spatial joins to get counts within each census tract
dc_tracts <- tracts(state = 11) %>% 
  st_transform(crs = 4326) 

transit_tract <- st_join(dc_tracts, metro, join = st_intersects)

transit_tract %>%
  as_tibble() %>% 
  group_by(GEOID) %>%
  summarize(num_metro_stations = n()) %>%
  arrange(desc(num_metro_stations)) %>% 
  ungroup

transit_tract <- st_join(transit_tract, bus_stops, join = st_intersects)


