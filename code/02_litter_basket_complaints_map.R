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
  palette = "warm",
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
                  highlight_dists = council_districts$CounDist[council_districts$complaints_per_basket > 0.1]) %>%
  addLegend_decreasing(position = "topleft", pal,
                       values = council_districts$complaints_per_basket, 
                       opacity = 1, 
                       title = "# of 311 complaints <br> per 100 litter baskets <br> (Apr 23-Mar 24)") 
mapview::mapshot(map, file=file.path("visuals", "map_311_complaints_per100baskets.html"))




# what are the units 
st_crs(baskets, parameters = TRUE)$units_gdal 
 
lb_hex <- baskets %>% 
    st_transform(2263) %>% 
    st_make_grid(cellsize = c(2000, 2000), square = FALSE) %>% # 2000 ft 
    st_sf() %>% # from sfc to sf 
    st_intersection(boro_sf %>% st_transform(2263))  %>% 
    st_join(baskets) %>% 
    mutate(id = row_number())
 
lb_comp_hex_dt <- lb_hex %>% 
    st_join(lbcomp_sf) %>%  
  as.data.table()
  
lbdt_sub <- lb_comp_hex_dt[!is.na(basketid) & !is.na(unique_key), ]
lbdt_sub$basketid <- as.character(lbdt_sub$basketid)

lbdt_sub[, n_lbs := length(unique(basketid)), by = "id"]
lbdt_sub[, n_complts :=  length(unique(unique_key)), by = "id"]
lbdt_sub[, comps_per_lb := n_complts/n_lbs]
summary(lbdt_sub$comps_per_lb)
bad_lbs <- lbdt_sub[comps_per_lb > 15, .(id, geometry, comps_per_lb)] 
quantile(lbdt_sub$comps_per_lb, .9)
bdsf <- bad_lbs %>% 
  distinct() %>% 
  st_as_sf() 
  

dcm <- st_read("data/input/DCM_ArterialsMajorStreets.shp", "DCM_ArterialsMajorStreets") %>% 
  st_as_sf(crs = 4326) %>% 
  st_transform(2263)

bad_sts <- st_intersection(dcm, bdsf)
mapview::mapview(bad_sts)

# pull out streets or some location info (lions)
# get centroid of hexbin -- get streets 

sub_sf <- lbdt_sub[, .(comps_per_lb, geometry)] %>%
  distinct() %>% 
  st_as_sf() %>% 
  st_transform('+proj=longlat +datum=WGS84') 


# check distribution - very skewed
plot(density(sub_sf$comps_per_lb, na.rm = T))
ints = classIntervals(sub_sf$comps_per_lb, n = 5, style = "headtails")
  
pal <- leaflet::colorBin(palette = pal_nycc("cool"),
                           bins = ints$brks,
                           domain = sub_sf$comps_per_lb,
                           na.color = "#FFFFFF")

m_lb <- leaflet() %>% 
  addCouncilStyle(add_dists = TRUE) %>% 
  addPolygons(data=sub_sf, fillColor = ~pal(comps_per_lb), 
              label = ~paste0(comps_per_lb), 
              opacity = 0, fillOpacity = 0.9) %>% 
  addLegend_decreasing(pal = pal, decreasing = T,
            values = sub_sf$comps_per_lb, 
            position = "topleft", 
            title = "Litter Basket Complaints <br/> per Litter Basket",
            labFormat = labelFormat(digits = 1))


 mapview::mapshot(m_lb, "visuals/lb_desserts.html")


# sanitation zones --------------------------------------------------------
# szurl <- "https://data.cityofnewyork.us/api/geospatial/ak2e-nbe8?method=export&format=Shapefile"
# szz <- unzip_sf(szurl)
# szsf <- st_read(szz)  %>% 
#   st_as_sf(crs = 4326) %>% 
#   st_transform(2263)
# 

# litter_desc <- c("E5 Loose Rubbish", "E3 Dirty Sidewalk", "10 Litter Basket / Request", 
#                   "Sidewalk", "Trash", "Litter")
 
 
# dsny_sub[descriptor %in% litter_desc, unique(complaint_type)]
 # lions shape -------------------------------------------------------------
 # get zip from:
 # "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/dcm_20230731shp.zip"
 
 # lion <- read_sf("data/lion/lion.gdb/", "lion") 
 # lion$types <- as.character(st_geometry_type(lion$SHAPE))
 # 
 # fix_geom<-lion
 # fixed=c()
 # for(i in 1:dim(fix_geom)[1]){
 #   fix_geom$SHAPE[i]=st_cast(fix_geom$SHAPE[i], "MULTILINESTRING")
 # }
 # 
 # 
 # 
 # 
 # 
 # cast_all <- function(xg) {
 #   lapply(c("MULTIPOLYGON", "MULTILINESTRING", "MULTIPOINT", "POLYGON", "LINESTRING", "POINT"), 
 #          function(x) st_cast(xg, x))
 # }
 # 
 # cast_all(multis$SHAPE)
 # 
 # badlion <- st_join(bad_lbs, multissf)
 # 
