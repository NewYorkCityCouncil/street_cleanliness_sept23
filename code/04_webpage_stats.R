source("code/00_load_dependencies.R")


################################################################################
# Created by: Anne Driscoll
# Last edited on: 4/30/2024
#
# Get stats for webpage
################################################################################


# number of litter baskets -----------------------------------------------------
baskets = st_read("https://data.cityofnewyork.us/resource/8znf-7b2c.geojson?$limit=999999") %>%
  st_as_sf(crs = 4326) %>% 
  select(basketid)
nrow(baskets)

# oath dirty sidewalk violations -----------------------------------------------
# 311 Service Requests from 2010 to Present ------------------------------------
oath = fread(URLencode("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=charge_1_code_description like '%DIRTY SIDEWALK%' and violation_date >= '2023-04-01' and violation_date < '2024-04-01'"))
oath %>%
  filter(violation_location_borough != "NOT NYC", 
         hearing_result != "DISMISSED") %>%
  nrow()



