source("code/00_load_dependencies.R")
sf_use_s2(FALSE)

# dsny complaints ---------------------------------------------------------
dsny_311 <- fread("https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?agency='DSNY'&$where=created_date>='2018-01-01'&$limit=999999999999")
dsny_311[, bbl := as.character(bbl)][, created_date := as.Date(created_date)]
dsny_sub <- dsny_311[!resolution_description %in% 
  c("The Department of Sanitation investigated this complaint and found no violation at the location.", 
    "The Department of Sanitation investigated this complaint and found no condition at the location.") & !is.na(latitude), ]


business_complaints <- c("Commercial Disposal Complaint", "Retailer Complaint", "Dirty Condition")
business_locations <- c("Sidewalk", "Street")
litter_complaints <- c("Litter Basket Complaint", "Dirty Condition", "Illegal Dumping")

lbcomp_sf <- dsny_sub[complaint_type %in% litter_complaints, ] %>% 
  st_as_sf(wkt = "location", crs = 4326) %>% 
  st_transform(2263) 

# litter baskets ----------------------------------------------------------
# get litter basket locations, buffer and create polygon
lburl <- "https://data.cityofnewyork.us/api/geospatial/8znf-7b2c?accessType=DOWNLOAD&method=export&format=Shapefile"
lbzip <- unzip_sf(lburl)
lb_points <- st_read(lbzip) %>% 
  st_as_sf(crs = 4326) %>% 
  st_transform(2263)

# what are the units 
st_crs(lb_points, parameters = TRUE)$units_gdal 
 
 lb_hex <- lb_points %>% 
    st_make_grid(cellsize = c(2000,2000), square = FALSE) %>% # 2000 ft 
    st_sf() %>% # from sfc to sf 
    st_intersection(boro_sf)  %>% 
    st_join(lb_points) %>% 
    mutate(id = row_number())
 
lb_comp_hex_dt <-  lb_hex %>% 
    st_join(lbcomp_sf) %>%  
  as.data.table()
  
lbdt_sub <- lb_comp_hex_dt[, .(hex_id = id, basketid,  compl_id = unique_key, created_date, geometry)]
lbdt_sub[, n_lbs := length(!is.na(basketid)), by = "hex_id"][is.na(basketid), n_lbs := 0]
lbdt_sub[, n_complts := length(!is.na(compl_id)), by = "hex_id"][is.na(compl_id), n_complts := 0]

lbdt_sub[, comps_per_lb := n_complts/n_lbs]
unique(lbdt_sub$comps_per_lb)

sub_dt <- lbdt_sub[!comps_per_lb %in% "NaN" & is.infinite(comps_per_lb), ] # 0 complaints, 0 litter baskets
summary(sub_dt$n_complts)


sub_dt %>% 
  left_join(lb_hex, by = "hex_id")

lb_comp_sf <- sub_dt %>% 
  st_as_sf() %>% 
  st_transform('+proj=longlat +datum=WGS84') # reproject for mapping


pal <- colorQuantile(palette = c("#b3b3ff","#1850b5","#1f3a70","#ba9f64"), domain = unique(sub_dt$n_complts))

   

 





# sanitation zones --------------------------------------------------------
# szurl <- "https://data.cityofnewyork.us/api/geospatial/ak2e-nbe8?method=export&format=Shapefile"
# szz <- unzip_sf(szurl)
# szsf <- st_read(szz)  %>% 
#   st_as_sf(crs = 4326) %>% 
#   st_transform(2263)
# 
# b_url <- "https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=Shapefile"
# boro_zip <- unzip_sf(b_url)
# boro_sf <- st_read(boro_zip) %>% 
#   st_as_sf(crs = 4326) %>% 
#   st_transform(2263)



