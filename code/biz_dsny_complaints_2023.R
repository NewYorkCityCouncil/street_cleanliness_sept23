source("code/00_load_dependencies.R")
library(councilverse)
sf_use_s2(FALSE)

b_url <- "https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=Shapefile"
boro_zip <- unzip_sf(b_url)
boro_sf <- st_read(boro_zip) %>%
  st_as_sf(crs = 4326) %>%
  st_transform(2263)
##
## Sanitation Hearing 9/26/23
## Brook Frye

# restaurants -------------------------------------------------------------
rest <- fread("https://data.cityofnewyork.us/resource/43nn-pn8j.csv?$limit=9999999999")
rest_sub <- unique(rest[, .(latitude, longitude, bbl = as.character(bbl))])
rest_sf <- rest_sub[!is.na(latitude), ] %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# using vacant storefront data & dca data for businesses --------------------------------------------
vsd <- fread("https://data.cityofnewyork.us/resource/92iy-9c3n.csv?$limit=9999999999")
vsd_sub <- unique(vsd[, .(bbl = as.character(borough_block_lot),
                          latitude, longitude)])
vsd_sf <- vsd_sub[!is.na(latitude), ] %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# dca data
biz <- fread("https://data.cityofnewyork.us/resource/p2mh-mrfv.csv?$limit=999999")
# subset down 
biz_sub <- unique(biz[license_type %in% "Business" & license_status %in% "Active" & !location %in% "" &
                        !bbl %in% vsd_sub$bbl & !bbl %in% 
                        rest_sub$bbl, 
                      .(geometry = location, 
                        bbl = as.character(bbl))])
biz_sf <-  biz_sub %>% 
  st_as_sf(wkt="geometry", crs=4326) 

# concat
big_biz <- rbind(vsd_sf, biz_sf)
big_biz2 <- rbind(big_biz, rest_sf)
bigbiz_sf <- st_as_sf(big_biz2, crs = 4326) %>% 
  st_transform(2263) 
  
# make hex polys 
 bizhex <-  boro_sf %>% 
  st_make_grid(cellsize = c(2000, 2000), square = FALSE) %>% # 2000 ft 
  st_sf() %>% # from sfc to sf 
  st_intersection(boro_sf)  %>% 
   mutate(id = row_number()) %>% 
   st_join(bigbiz_sf) 

# join biz w complaints
dsny_311 <- fread("https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?agency='DSNY'&$where=created_date>='2022-08-01'&$limit=999999999999")
dsny_311[, bbl := as.character(bbl)]
 
# remove dismissed complaints
dsny_sub <- dsny_311[!resolution_description %in% 
                        c("The Department of Sanitation investigated this complaint and found no violation at the location.", 
                          "The Department of Sanitation investigated this complaint and found no condition at the location.") & !is.na(latitude), ] 

business_complaints <- c("Commercial Disposal Complaint", "Retailer Complaint")

dsny_311_biz <- dsny_sub[complaint_type %in% business_complaints,.(location, unique_key)] %>% 
  st_as_sf(wkt = "location", crs = 4326) %>% 
  st_transform(2263) 

biz_comp_dt <-  bizhex %>% 
  st_join(dsny_311_biz) %>%  
  as.data.table()
wdt <- biz_comp_dt[!is.na(unique_key), ]

# how many complaints re biz per geom
wdt[, .N, by = c("id", "unique_key")][order(N, decreasing = T)]

# biz per geom/# cmplnts per geom
wdt[, n_bz := length(unique(bbl)), by = "id"]
wdt[, n_cmp := length(unique(unique_key)), by = "id"]
wdt[, cmp_2_biz := round(n_cmp/n_bz, 2)]

wdt_sf <- wdt[, .(geometry, cmp_2_biz, id)] %>% 
  distinct() %>% 
  st_as_sf() %>% 
  st_transform('+proj=longlat +datum=WGS84') 

pal <- leaflet::colorBin(palette = "Blues", domain = unique(wdt_sf$cmp_2_biz))

m_biz <- leaflet() %>% 
  addCouncilStyle(add_dists = TRUE) %>% 
  leaflet::addPolygons(data = wdt_sf, 
                       fillColor = ~pal(cmp_2_biz), 
                       color =  ~pal(cmp_2_biz), 
                       label = ~paste0(cmp_2_biz), 
                       opacity = 0, 
                       fillOpacity = 1) %>% 
  addLegend(pal = pal, 
            values = unique(wdt_sf$cmp_2_biz), 
            position = "topleft", 
            title = "Complaints per Business")


mapview::mapshot(m_biz, "visuals/dsny_biz_cmplts_2022-sept2023.html")

# commercial waste zones --------------------------------------------------
# cwurl <- "https://data.cityofnewyork.us/api/geospatial/8ev8-jjxq?accessType=DOWNLOAD&method=export&format=Shapefile"
# cwzf <- unzip_sf(cwurl)
# cwz_sf <- st_read(cwzf) %>% 
#   st_simplify(preserveTopology = TRUE,
#               dTolerance = 100) %>%
#   st_cast("MULTIPOLYGON") 

