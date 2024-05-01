source("code/00_load_dependencies.R")
library(htmlwidgets)

################################################################################
# Created by: Brook Frye
# Edited by: Anne Driscoll
# Last edited on: 4/25/2024
#
# Create map of business complaints per business
################################################################################

# ------------------------------------------------------------------------------
# load in data
# ------------------------------------------------------------------------------

# boro boundaries  -------------------------------------------------------------
boro_sf = unzip_sf("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=Shapefile") %>%
  st_read() %>%
  st_as_sf(crs = 4326) %>%
  select("geometry")

# cd boundaries  -------------------------------------------------------------
council_districts = unzip_sf("https://www.nyc.gov/assets/planning/download/zip/data-maps/open-data/nycc_21d.zip") %>% 
  st_read() %>% 
  st_transform(st_crs(4326))

# PLUTO data -------------------------------------------------------------------
# Primary Land Use Tax Lot Output (PLUTO) --------------------------------------
pluto = fread("https://data.cityofnewyork.us/resource/64uk-42ks.csv?$limit=999999999&$select=bbl,latitude,longitude,bldgarea,comarea,retailarea,bldgclass") %>%
  mutate(bbl = as.character(bbl))


# restaurant data  -------------------------------------------------------------
# DOHMH New York City Restaurant Inspection Results ----------------------------
restaurants = fread("https://data.cityofnewyork.us/resource/43nn-pn8j.csv?$limit=9999999999") %>%
  select(bbl, latitude, longitude) %>%
  mutate(bbl = as.character(bbl)) %>%
  filter(!is.na(latitude)) %>%
  group_by(bbl) %>%
  filter(row_number() == 1) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 


# storefront data --------------------------------------------------------------
# DCWP Legally Operating Businesses --------------------------------------------
businesses = fread("https://data.cityofnewyork.us/resource/w7w3-xahh.csv?$limit=9999999999") %>%
  select(bbl, latitude, longitude) %>%
  filter(!is.na(latitude)) %>%
  mutate(bbl = as.character(bbl)) %>%   
  group_by(bbl) %>%
  filter(row_number() == 1) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 


# 311 complaint data -----------------------------------------------------------
# 311 Service Requests from 2010 to Present ------------------------------------
dsny_311 = fread("https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?agency='DSNY'&$where=created_date>='2023-04-01'&$limit=999999999999") %>%
  mutate(bbl = as.character(bbl)) %>%
  filter(!resolution_description %in% 
           c("The Department of Sanitation investigated this complaint and found no violation at the location.",
             "The Department of Sanitation investigated this complaint and found no condition at the location."), 
         !is.na(latitude), 
         complaint_type %in% c("Commercial Disposal Complaint", "Retailer Complaint"), 
         created_date < "2024-04-01") %>% 
  select(unique_key, created_date, latitude, longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 


# OATH data --------------------------------------------------------------------
# OATH Hearings Division Case Status -------------------------------------------
oath = fread(URLencode("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=issuing_agency like '%SANITATION%' and violation_date >= '2023-04-01' and violation_date < '2024-04-01'"))

original_filter_oath = oath %>%
  filter(grepl("commercial|food|restaurant|business|retail|drink|bar|beverage|store|hotel|alcohol|chain|closing|organics", 
               charge_1_code_description, ignore.case=T), 
         hearing_result != "DISMISSED") %>%
  rename(boro = violation_location_borough,
         block = violation_location_block_no,
         lot = violation_location_lot_no, 
         unique_key = ticket_number) %>%
  select(boro, block, lot, unique_key, violation_date, charge_1_code_description) %>%
  mutate(boro = ifelse(boro == "QUEENS", 4, boro), 
         boro = ifelse(boro == "BROOKLYN", 3, boro), 
         boro = ifelse(boro == "MANHATTAN", 2, boro), 
         boro = ifelse(boro == "BRONX", 2, boro),  
         boro = ifelse(boro == "STATEN ISLAND", 5, boro), 
         bbl = paste0(boro,
                      str_pad(block, 5, "left", pad = 0),
                      str_pad(lot, 4, "left", pad = 0))) %>% 
  merge(pluto, by = "bbl", all.x = T) %>%
  filter(!is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 
  

new_filter_oath = oath %>%
  filter(hearing_result != "DISMISSED") %>%
  rename(boro = violation_location_borough,
         block = violation_location_block_no,
         lot = violation_location_lot_no, 
         unique_key = ticket_number) %>%
  select(boro, block, lot, unique_key, respondent_first_name, respondent_last_name, 
         violation_date, charge_1_code_description) %>%
  mutate(boro = ifelse(boro == "QUEENS", 4, boro), 
         boro = ifelse(boro == "BROOKLYN", 3, boro), 
         boro = ifelse(boro == "MANHATTAN", 2, boro), 
         boro = ifelse(boro == "BRONX", 2, boro),  
         boro = ifelse(boro == "STATEN ISLAND", 5, boro), 
         bbl = paste0(boro,
                      str_pad(block, 5, "left", pad = 0),
                      str_pad(lot, 4, "left", pad = 0))) %>%
  merge(pluto, by = "bbl", all.x = T) %>%
  filter(!is.na(latitude), 
        # comarea > 0 | 
        #   retailarea > 0 | 
           bldgclass %in% c("R5", "R7", "R8", "RA", "RB", "RH", "RK", 
                            "L1", "L8") | 
          startsWith(bldgclass, "G") | 
          startsWith(bldgclass, "J") | 
          startsWith(bldgclass, "K") | 
          startsWith(bldgclass, "P"), 
        !grepl("RESIDENTIAL", charge_1_code_description), 
        !grepl("DWELLING UNITS", charge_1_code_description), 
        !grepl("VEHICLE", charge_1_code_description),  
        !grepl("PUBLIC URINATION", charge_1_code_description)) %>% 
        #!charge_1_code_description %in% c("FAILURE TO CLEAN 18 INCHES INTO STREET", 
        #                                  "DIRTY SIDEWALK", 
        #                                  "SIDEWALK OBSTRUCTION", "DIRTY AREA", "POSTING OF SIGN PERMIT", "STREET OBSTRUCTION")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 


# ------------------------------------------------------------------------------
# get the oath, 311 complaints, and businesses at the council district ---------
# ------------------------------------------------------------------------------

oath_count = st_intersects(council_districts, new_filter_oath)
oath_count_n = sapply(oath_count, length)

dsny_count = st_intersects(council_districts, dsny_311)
dsny_count_n = sapply(dsny_count, length)

restaurant_count = st_intersects(council_districts, restaurants)
restaurant_count_n = sapply(restaurant_count, length)

businesses_count = st_intersects(council_districts, businesses)
businesses_count_n = sapply(businesses_count, length)

council_districts = council_districts %>%
  mutate(oath_count = oath_count_n, 
         dsny_count = dsny_count_n, 
         restaurant_count = restaurant_count_n, 
         businesses_count = businesses_count_n, 
         
         oath_per_business = oath_count/(restaurant_count + businesses_count)*1000, 
         dsny_per_business = dsny_count/(restaurant_count + businesses_count)*1000, 
         complaints_per_business = (dsny_count + oath_count)/(restaurant_count + businesses_count)*1000, 
         tooltip = paste0("<strong>District:</strong> ", CounDist, 
                "<br><strong>311 Complaints:</strong> ", dsny_count, 
                "<br><strong>OATH Violations:</strong> ", oath_count, 
                "<br><strong>Issues per 100 businesses:</strong> ", round(complaints_per_business, 2)))


# ------------------------------------------------------------------------------
# just oath violations
quantile(council_districts$oath_per_business[council_districts$oath_per_business!= 0], seq(0, 1, length.out = 6))
pal = councildown::colorBin(
  palette = "cool",
  bins = c(0, 135, 175, 260, 400, 610), 
  domain = c(0, council_districts$oath_per_business)
) 

map = leaflet(options = leafletOptions(attributionControl=FALSE, 
                                       zoomControl = FALSE, 
                                       minZoom = 10, 
                                       maxZoom = 15)) %>%
  addPolygons(data = council_districts, 
              fillColor = ~pal(oath_per_business), 
              weight = 0, fillOpacity = 1, smoothFactor = 0, 
              popup = ~tooltip) %>%
  addCouncilStyle(add_dists = TRUE, 
                  highlight_dists = council_districts$CounDist[council_districts$oath_per_business >= 260]) %>%
  addLegend_decreasing(position = "topleft", pal,
                       values = council_districts$oath_per_business, 
                       opacity = 1, 
                       title = "# of OATH violations <br> per 1000 businesses <br> (Apr 23-Mar 24)") 
saveWidget(map, file="visuals/map_oath_violations_per1000businesses.html")

# ------------------------------------------------------------------------------
# just 311 complaints
quantile(council_districts$dsny_per_business[council_districts$dsny_per_business!= 0], seq(0, 1, length.out = 6))
pal = councildown::colorBin(
  palette = "warm",
  bins = c(0, 4, 5, 6, 10, 80), 
  domain = c(0, council_districts$dsny_per_business)
) 

map = leaflet(options = leafletOptions(attributionControl=FALSE, 
                                       zoomControl = FALSE, 
                                       minZoom = 10, 
                                       maxZoom = 15)) %>%
  addPolygons(data = council_districts, 
              fillColor = ~pal(dsny_per_business), 
              weight = 0, fillOpacity = 1, smoothFactor = 0, 
              popup = ~tooltip) %>%
  addCouncilStyle(add_dists = TRUE, 
                  highlight_dists = council_districts$CounDist[council_districts$dsny_per_business >= 10]) %>%
  addLegend_decreasing(position = "topleft", pal,
                       values = council_districts$oath_per_business, 
                       opacity = 1, 
                       title = "# of 311 complaints <br> per 1000 businesses <br> (Apr 23-Mar 24)") 
mapview::mapshot(map, "visuals/map_311_complaints_per1000businesses.html")

# ------------------------------------------------------------------------------
# get the oath, 311 complaints, and businesses at the HEX ----------------------
# ------------------------------------------------------------------------------

# make hex polys
hex = boro_sf %>%
  st_transform(2263) %>% 
  st_make_grid(cellsize = c(2000, 2000), square = FALSE) %>% # 2000 ft
  st_sf() %>% # from sfc to sf
  st_transform(4326) %>%
  st_intersection(boro_sf %>% st_transform(4326)) %>%
  mutate(id = row_number())

# get counts
oath_count = st_intersects(hex, new_filter_oath)
oath_count_n = sapply(oath_count, length)

dsny_count = st_intersects(hex, dsny_311)
dsny_count_n = sapply(dsny_count, length)

restaurant_count = st_intersects(hex, restaurants)
restaurant_count_n = sapply(restaurant_count, length)

businesses_count = st_intersects(hex, businesses)
businesses_count_n = sapply(businesses_count, length)

# add to hex
hex = hex %>%
  mutate(oath_count = oath_count_n, 
         dsny_count = dsny_count_n, 
         restaurant_count = restaurant_count_n, 
         businesses_count = businesses_count_n, 
         
         oath_per_business = oath_count/(restaurant_count + businesses_count)*100, 
         dsny_per_business = dsny_count/(restaurant_count + businesses_count)*100, 
         complaints_per_business = (dsny_count + oath_count)/(restaurant_count + businesses_count)*100, 
         tooltip = paste0("<strong>311 Complaints:</strong> ", dsny_count, 
                          "<br><strong>OATH Violations:</strong> ", oath_count, 
                          "<br><strong>Issues per 100 businesses:</strong> ", round(complaints_per_business, 2)))

quantile(hex$complaints_per_business[hex$complaints_per_business != 0], seq(0, 1, length.out = 6), na.rm=T)
pal = councildown::colorBin(
  palette = "warm",
  bins = c(0, 5, 15, 25, 35, 1300), 
  domain = c(0, hex$complaints_per_business)
) 

map = leaflet(options = leafletOptions(attributionControl=FALSE, 
                                       zoomControl = FALSE, 
                                       minZoom = 10, 
                                       maxZoom = 15)) %>%
  addPolygons(data = hex, 
              fillColor = ~pal(complaints_per_business), 
              weight = 0, fillOpacity = 1, smoothFactor = 0, 
              popup = ~tooltip) %>%
  addCouncilStyle(add_dists = TRUE) %>%
  addLegend_decreasing(position = "topleft", pal,
                       values = council_districts$complaints_per_business, 
                       opacity = 1, 
                       title = "# of 311 complaints <br> and OATH violations <br> per 100 businesses <br> (Apr 23-Mar 24)") 


# ------------------------------------------------------------------------------
# just oath violations
quantile(hex$oath_per_business[hex$oath_per_business != 0], seq(0, 1, length.out = 6), na.rm=T)
pal = councildown::colorBin(
  palette = "warm",
  bins = c(0, 5, 15, 25, 35, 1300), 
  domain = c(0, hex$complaints_per_business)
) 

leaflet(options = leafletOptions(attributionControl=FALSE, 
                                       zoomControl = FALSE, 
                                       minZoom = 10, 
                                       maxZoom = 15)) %>%
  addPolygons(data = hex, 
              fillColor = ~pal(oath_per_business), 
              weight = 0, fillOpacity = 1, smoothFactor = 0, 
              popup = ~tooltip) %>%
  addCouncilStyle(add_dists = TRUE) %>%
  addLegend_decreasing(position = "topleft", pal,
                       values = council_districts$complaints_per_business, 
                       opacity = 1, 
                       title = "# of OATH violations <br> per 100 businesses <br> (Apr 23-Mar 24)") 

# ------------------------------------------------------------------------------
# just dsny complaints
quantile(hex$dsny_per_business[hex$dsny_per_business != 0], seq(0, 1, length.out = 6), na.rm=T)
pal = councildown::colorBin(
  palette = "warm",
  bins = c(0, 1.5, 2, 3.5, 5, 250), 
  domain = c(0, hex$dsny_per_business)
) 

leaflet(options = leafletOptions(attributionControl=FALSE, 
                                 zoomControl = FALSE, 
                                 minZoom = 10, 
                                 maxZoom = 15)) %>%
  addPolygons(data = hex, 
              fillColor = ~pal(dsny_per_business), 
              weight = 0, fillOpacity = 1, smoothFactor = 0, 
              popup = ~tooltip) %>%
  addCouncilStyle(add_dists = TRUE) %>%
  addLegend_decreasing(position = "topleft", pal,
                       values = council_districts$complaints_per_business, 
                       opacity = 1, 
                       title = "# of 311 complaints <br> per 100 businesses <br> (Apr 23-Mar 24)") 
