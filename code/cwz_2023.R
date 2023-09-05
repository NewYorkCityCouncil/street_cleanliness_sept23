source("code/00_load_dependencies.R")
##
## Sanitation Hearing 9/26/23
## Brook Frye 

# load pluto --------------------------------------------------------------
pluto_url <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/nyc_mappluto_23v2_shp.zip"
pluto_file <- unzip_sf(pluto_url) 
plt_sf <- st_read(pluto_file) %>% 
  st_simplify(preserveTopology = TRUE,
              dTolerance = 100) %>%
  st_cast("MULTIPOLYGON") 

# load commercial waste zone boundaries -----------------------------------
cwurl <- "https://data.cityofnewyork.us/api/geospatial/8ev8-jjxq?accessType=DOWNLOAD&method=export&format=Shapefile"
cwzf <- unzip_sf(cwurl)
cwz_sf <- st_read(cwzf) %>% 
  st_simplify(preserveTopology = TRUE,
  dTolerance = 100) %>%
  st_cast("MULTIPOLYGON") 
