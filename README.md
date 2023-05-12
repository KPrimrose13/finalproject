# Air Quality in the District of Columbia: An Analysis
By Diana Schoder and Kelly Primrose

Prepared for: PPOL 670 Introduction to Data Science, Final Project

Motivating Question: Can we use the tools learned in this course to delve into air quality trends in the District of Columbia?

## Project Files
The data cleaning section of our project can be accessed [here](https://github.com/KPrimrose13/finalproject/blob/eaf53bac9ae6c3d7c1254675be14ca839dc25c0f/TransitDataCleaning.R). It was separated out from the main project to improve readability and center our analysis results. Where possible, we used 2017 data to match the EJ Screen air quality data, which also avoids differences in trends due to the Covid-19 pandemic.

The qmd file for our final project analysis can be accessed [here](https://github.com/KPrimrose13/finalproject/blob/eaf53bac9ae6c3d7c1254675be14ca839dc25c0f/finalproject.qmd).

The page for our final product can be accessed [here](https://github.com/KPrimrose13/finalproject/blob/f8ff8fbd1e02ea1b474163b52c75a3ee70ce695c/finalproject.html) 

## Data Sources

1. Open Data DC. Air Quality Real Time. https://opendata.dc.gov/datasets/DCGIS::air-quality-realtime/about 
2. U.S. Census Bureau. American Community Survey 5-year 2021 - 2017. Accessed via API.
3. Open Data DC. Bus Stops. https://opendata.dc.gov/datasets/b6521725905c48b89c2d661649fb8692/api
4. Open Data DC. Urban Tree Canopy by Census Block. https://opendata.dc.gov/datasets/DCGIS::urban-tree-canopy-by-census-block-in-2020/explore?location=38.893730%2C-77.014456%2C12.78 
5. Open Data DC. Taxi & Limo Stands. https://opendata.dc.gov/datasets/DCGIS::taxi-and-limousine-stands-/explore?location=38.894944%2C-77.015000%2C12.62
6. Open Data DC. Bike Lanes. https://opendata.dc.gov/datasets/DCGIS::bicycle-lanes/explore 
7. Open Data DC. Walkshed of Bikeshare Stands. https://opendata.dc.gov/datasets/DCGIS::walkshed-bikeshare/explore?location=38.898101%2C-77.004680%2C13.09 
8. Open Data DC. Truck restriction. https://opendata.dc.gov/datasets/DCGIS::truck-restriction/explore 
9. Open Data DC. 2017 traffic Volume. https://opendata.dc.gov/datasets/DCGIS::2017-traffic-volume/explore?location=38.892506%2C-77.020630%2C12.70 
10. Open Data DC. Commute to work by car, transit, etc. https://opendata.dc.gov/datasets/DCGIS::acs-economic-characteristics-dc-census-tract/about 
11. Open Data DC. Metro stations. https://opendata.dc.gov/datasets/DCGIS::metro-stations-district/explore?location=38.892509%2C-77.020630%2C12.70 
12. Open Data DC. Pedestrian friendliness. https://opendata.dc.gov/datasets/DCGIS::pedestrian-friendliness-index-census-blocks/explore 


## Final Dataset Variables

* **Aadt:** The 2017 “annual average daily traffic” (AADT) per route ID, averaged within each census tract.
* **Bikelane_dual_protected:** Proportion of protected dual bike lanes within each census tract.
* **Bikelane_protected:** Proportion of protected bike lanes within each census tract.
* **Bstp_bnh_cnt:** Proportion of bus stops with a bench within a census tract. (2017)
* **Bstp_geo_id:** Total number of bus stops within a census tract. (2017)
* **Bstp_has_bkrs:** Proportion of bus stops with bike racks within a census tract. (2017)
* **Car_alone_comm:** Driving alone as a mode of commute for workers 16 years and over (car, truck, or van).
* **Carpool_comm:** Carpooling as a mode of commute for workers 16 years and over (car, truck, or van).
* **Density_score:** This was created by converting the polygon for pedestrian friendliness to surface points, with the mean score of these points taken for each tract.
* **Four_way_intersection_ratio:** This was created by converting the polygon for pedestrian friendliness to surface points, with the mean score of these points taken for each tract.
* **Gis_id:** Total sum of shared mobility parking within a tract.
* **Limo_capacity:** Sum of limo capacity within a census tract.
* **Local_deliv:** Total number of roads where local deliveries are coded as allowed (0 = true, 1 = false for each road.)
* **Num_lines:** Total number of metro line stops within a census tract (red + green + blue + orange + silver + yellow)
* **Parametername:** Name of pollutant measured at an air quality monitoring station.
* **Pfi_score:** Pedestrian friendliness index score. This was created by converting the polygon for pedestrian friendliness to surface points, with the mean score of these points taken for each tract.
* **Pm25:** Amount of particulate matter measured in 2017 per census tract, according to EJ Screen.
* **Poverty_est:** Percentage of families and people whose income in the past 12 months is below the poverty level in each tract in 2017. 
* **Race:** Any variable beginning with race is the number of people in that category in a census tract in 2017.
* **Reportingunits:** Units of the pollutant measured in parametername.
* **Restrict1:** Total number of roads where trucks over 1.25 tons are coded as not allowed (0 = true, 1 = false for each road.)
* **Restrict2:** Total number of roads where trucks with more than two axles are coded as not allowed (0 = false, 1 = true for each road.)
* **School_stop:** Total number of bus stops at schools within a census tract. (2017)
* **Shelter:** Proportion of bus stops inside a shelter within a census tract. (2017)
* **Sidewalk_score:** This was created by converting the polygon for pedestrian friendliness to surface points, with the mean score of these points taken for each tract.
* **Sitename:** Name of the air quality monitor station.
* **Taxi_capacity:** Sum of taxi capacity within a census tract.
* **Time_comm:** Time of commute in minutes.
* **Totalbikelanes:** Total miles of bike lanes within each census tract.
* **Transit_comm:** Public transportation as a mode of commute for workers 16 years and over (excluding taxicab). 
* **Utc_pct:** average percentage of tree cover in each sub-unit of a census tract.
* **Value:** Amount of pollutant.
* **Walk_comm:** Walking as a mode of commute for workers 16 years and over. 


