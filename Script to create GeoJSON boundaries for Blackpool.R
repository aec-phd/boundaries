## Based on Trafford Data Lab script at https://github.com/traffordDataLab/boundaries/blob/master/pre-processing/script.R

## Spatial data pre-processing and exporting Pennine Lancs vector boundaries to GeoJSON ##

# load necessary packages
library(sf) ; library(tidyverse)

setwd('C:/Users/aecun/OneDrive/Documents/Blackpool Mapping/boundaries')

# -------------------  Local Authority Districts
# Source: https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-may-2022-uk-bfc-v3/about
st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD_MAY_2022_UK_BFC_V3/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>% 
#  filter(grepl('Blackpool|Fylde|Wyre', LAD22NM)) %>%
  filter(LAD22NM %in% c("Blackpool","Fylde","Wyre")) %>%
  st_as_sf(crs = 4326, coords = c("LONG", "LAT")) %>%
  st_write("local_authorities.geojson", driver = "GeoJSON")
  
# ------------------- Wards (NB these are the generalised ones)
# Source: https://geoportal.statistics.gov.uk/datasets/ons::wards-may-2022-boundaries-uk-bgc-v3/about
st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Wards_May_2022_Boundaries_UK_BGC_V3/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>% 
  filter(LAD22NM %in% c("Blackpool","Fylde","Wyre")) %>%
  st_as_sf(crs = 4326, coords = c("LONG", "LAT")) %>% 
  st_write("wards.geojson", driver = "GeoJSON")

# ------------------- Middle Super Output Areas
# Source: https://geoportal.statistics.gov.uk/datasets/ons::middle-layer-super-output-areas-december-2021-boundaries-generalised-clipped-ew-bgc/about
st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Middle_layer_Super_Output_Areas_December_2021_EW_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>% 
  filter(grepl('Blackpool|Fylde|Wyre', MSOA21NM)) %>% 
  filter(!grepl('Wyre Forest',MSOA21NM)) %>%
  st_as_sf(crs = 4326, coords = c("LONG", "LAT")) %>% 
  st_write("msoa.geojson", driver = "GeoJSON")

# ------------------- Lower Super Output Areas
# Source: https://geoportal.statistics.gov.uk/datasets/ons::lower-layer-super-output-areas-december-2021-boundaries-generalised-clipped-ew-bgc/about
st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Lower_layer_Super_Output_Areas_Decemeber_2021_EW_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>% 
  filter(grepl('Blackpool|Fylde|Wyre', LSOA21NM)) %>% 
  filter(!grepl('Wyre Forest',LSOA21NM)) %>% 
  st_as_sf(crs = 4326, coords = c("LONG", "LAT")) %>% 
  st_write("lsoa.geojson", driver = "GeoJSON")

# ------------------- Output Areas
# Source: https://geoportal.statistics.gov.uk/datasets/ons::output-areas-december-2021-boundaries-generalised-clipped-ew-bgc/about
OA_lookup <- read.csv("https://www.arcgis.com/sharing/rest/content/items/9f0bc2c6fbc9427ba11db01759e5f6d8/data") %>%
  filter(lad22nm %in% c("Blackpool","Fylde","Wyre")) # Need lookup, as no LA names provided in OA file
st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Output_Areas_December_2021_Boundaries_EW_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>% 
  filter(OA21CD %in% OA_lookup$oa21cd) %>%
  st_as_sf(crs = 4326, coords = c("long", "lat")) %>% 
  st_write("oa.geojson", driver = "GeoJSON")

# ------------------ Downtown Area of Blackpool
msoas <- st_read("msoa.geojson")
msoas$central <- ifelse(msoas$MSOA21CD %in% c("E02002638","E02002640","E02002642","E02002643","E02002645"),"Downtown Blackpool","No")
msoas %>% group_by(central) %>% summarise() %>% filter(central == "Downtown Blackpool") %>% st_write("central.geojson",driver = "GeoJSON")
