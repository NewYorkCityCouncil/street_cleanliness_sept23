source("code/00_load_dependencies.R")
sf_use_s2(FALSE)


################################################################################
# Created by: Anne Driscoll
# Last edited on: 4/25/2024
#
# Create map of litter basket complaints
################################################################################

# ------------------------------------------------------------------------------
# load in data
# ------------------------------------------------------------------------------

# 311 complaints ---------------------------------------------------------------
basket_complaints = fread("https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?agency='DSNY'&$where=created_date>='2022-04-01'&$limit=999999999999") %>%
  filter(!is.na(latitude), 
         complaint_type %in% c("Litter Basket Complaint", "Overflowing Litter Baskets"), 
         created_date < "2024-03-01",
         !resolution_description %in% 
           c("The Department of Sanitation investigated this complaint and found no condition at the location.", 
             "The Department of Sanitation investigated this complaint and found no violation at the location.")) %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>%
  mutate(ym = as.Date(paste(format(created_date, "%Y-%m"), "-01", sep=""), "%Y-%m-%d"))
  
# litter baskets ---------------------------------------------------------------
baskets = st_read("https://data.cityofnewyork.us/resource/8znf-7b2c.geojson?$limit=999999") %>%
  st_as_sf(crs = 4326) %>% 
  select(basketid)
  
# spatial areas ----------------------------------------------------------------
tracts = st_read("https://data.cityofnewyork.us/resource/63ge-mke6.geojson?$limit=5000")

council_districts = unzip_sf("https://www.nyc.gov/assets/planning/download/zip/data-maps/open-data/nycc_21d.zip") %>% 
  st_read() %>% 
  st_transform(st_crs(4326))

boro_sf = unzip_sf("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=Shapefile") %>%
  st_read() %>%
  st_as_sf(crs = 4326) %>%
  select("geometry")



# ------------------------------------------------------------------------------
# map each basket with number of complaints
# ------------------------------------------------------------------------------

# litter baskets ----------------------------------------------------------
baskets = st_read("https://data.cityofnewyork.us/resource/8znf-7b2c.geojson") %>% 
  st_as_sf(crs = 4326)


d = st_distance(basket_complaints, baskets)
min.d = apply(d, 1, function(x) order(x, decreasing=F)[2])
matched_basket_id = baskets$basketid[min.d] %>% table() %>% as.data.frame()

b = baskets %>% 
  merge(matched_basket_id, by.x = "basketid", by.y = ".", all.x = T) %>%
  mutate(Freq = ifelse(is.na(Freq), 0, Freq))


quantile(b$Freq[b$Freq != 0], seq(0, 1, length.out = 6))
pal = councildown::colorBin(
  palette = "warm",
  bins = c(0, 1, 2, 3, 4, 60), 
  domain = c(0, b$Freq)
) 

map = leaflet(options = leafletOptions(attributionControl=FALSE, 
                                       zoomControl = FALSE, 
                                       minZoom = 10, 
                                       maxZoom = 15)) %>%
  addCircles(data = b, 
             fillColor = ~pal(Freq), color = ~pal(Freq),
             weight = 0, radius = ~(Freq*20), fillOpacity = 1) %>%
  addCouncilStyle(add_dists = TRUE) %>%
  addLegend_decreasing(position = "topleft", pal,
                       values = council_districts$complaints_per_business, 
                       opacity = 1, 
                       title = "# of 311 complaints <br> and OATH violations <br> per 100 businesses <br> (Apr 23-Mar 24)") 


# ------------------------------------------------------------------------------
# map 311 complaints at the hex level ------------------------------------------
# ------------------------------------------------------------------------------

hex = boro_sf %>%
  st_transform(2263) %>% 
  st_make_grid(cellsize = c(1500, 1500), square = FALSE) %>% # 2000 ft
  st_sf() %>% # from sfc to sf
  st_transform(4326) %>%
  st_intersection(boro_sf %>% st_transform(4326)) %>%
  mutate(id = row_number())

basket_count = st_intersects(hex, baskets)
basket_count_n = sapply(basket_count, length)

complaint_count = st_intersects(hex, basket_complaints)
complaint_count_n = sapply(complaint_count, length)

hex = hex %>%
  mutate(complaint_count = complaint_count_n, 
         basket_count = basket_count_n, 
         
         complaints_per_basket = complaint_count/basket_count, 
         complaints_per_basket = ifelse(basket_count == 0, 0, complaints_per_basket),
         tooltip = paste0("<strong>311 Complaints:</strong> ", complaint_count, 
                          "<br><strong>Baskets:</strong> ", basket_count, 
                          "<br><strong>Complaints per basket:</strong> ", round(complaints_per_basket, 2)))

quantile(hex$complaints_per_basket[hex$complaints_per_basket != 0]*100, seq(0, 1, length.out = 6), na.rm=T)
pal = councildown::colorBin(
  palette = "warm",
  bins = c(0.01, 5, 10, 15, 35, 1100), 
  domain = c(0, hex$complaints_per_basket*100), 
  na.color = "grey"
) 

map = leaflet(options = leafletOptions(attributionControl=FALSE, 
                                 zoomControl = FALSE, 
                                 minZoom = 10, 
                                 maxZoom = 15)) %>%
  addPolygons(data = hex, 
              fillColor = ~pal(complaints_per_basket*100), 
              weight = 0, fillOpacity = 1, smoothFactor = 0, 
              popup = ~tooltip) %>%
  addPolygons(data = hex[hex$complaint_count == 0, ], 
              fillColor = "white", 
              weight = 0, fillOpacity = 1, smoothFactor = 0, 
              popup = ~tooltip) %>%
  addCouncilStyle(add_dists = TRUE) %>%
  addLegend_decreasing(position = "topleft", pal,
                       values = hex$complaints_per_basket, 
                       opacity = 1, 
                       na.label = "Complaints, but no baskets",
                       title = "# of 311 complaints <br> per 100 litter baskets <br> (Apr 23-Mar 24)") 

# ------------------------------------------------------------------------------
# map 311 complaints at community district level -------------------------------
# ------------------------------------------------------------------------------

basket_count = st_intersects(council_districts, baskets)
basket_count_n = sapply(basket_count, length)

complaint_count = st_intersects(council_districts, basket_complaints)
complaint_count_n = sapply(complaint_count, length)

council_districts = council_districts %>%
  mutate(complaint_count = complaint_count_n, 
         basket_count = basket_count_n, 
         
         complaints_per_basket = complaint_count/basket_count, 
         tooltip = paste0("<strong>District:</strong> ", CounDist, 
                          "<br><strong>311 Complaints:</strong> ", complaint_count, 
                          "<br><strong>Complaints per basket:</strong> ", round(complaints_per_basket, 2)))

quantile(council_districts$complaints_per_basket*100, seq(0, 1, length.out = 6))
pal = councildown::colorBin(
  palette = "indigo",
  bins = c(0, 3, 5, 7, 10, 35), 
  domain = c(0, council_districts$complaints_per_basket*100)
) 

map = leaflet(options = leafletOptions(attributionControl=FALSE, 
                                 zoomControl = FALSE, 
                                 minZoom = 10, 
                                 maxZoom = 15)) %>%
  addPolygons(data = council_districts, 
              fillColor = ~pal(complaints_per_basket*100), 
              weight = 0, fillOpacity = 1, smoothFactor = 0, 
              popup = ~tooltip) %>%
  addCouncilStyle(add_dists = TRUE, 
                  highlight_dists = council_districts$CounDist[council_districts$complaints_per_basket*100 >= 10]) %>%
  addLegend_decreasing(position = "topleft", pal,
                       values = council_districts$complaints_per_basket, 
                       opacity = 1, 
                       title = "# of 311 complaints <br> per 100 litter baskets <br> (Apr 23-Mar 24)") 
mapview::mapshot(map, file="visuals/map_311_complaints_per100baskets.html")