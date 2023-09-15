source("code/00_load_dependencies.R")
sf_use_s2(FALSE)

b_url <- "https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=Shapefile"
boro_zip <- unzip_sf(b_url)
boro_sf <- st_read(boro_zip) %>%
  st_as_sf(crs = 4326) %>%
  st_transform(2263)

# dsny complaints ---------------------------------------------------------
dsny_311 <- vroom("https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?agency='DSNY'&$where=created_date>='2022-08-01'&$limit=999999999999") %>% as.data.table()
dsny_311[, bbl := as.character(bbl)][, created_date := as.Date(created_date)]

# remove dismissed complaints
dsny_sub <- dsny_311[!resolution_description %in% 
  c("The Department of Sanitation investigated this complaint and found no violation at the location.", 
    "The Department of Sanitation investigated this complaint and found no condition at the location.") & !is.na(latitude), ]

# dsny_sub[grep("litter", complaint_type, ignore.case = T), unique(complaint_type)]
# subset to categories we want
litter_complaints <- c("Litter Basket Complaint", "Overflowing Litter Baskets")

lbcomp_sf <- dsny_sub[complaint_type %in% litter_complaints, .(location, unique_key)] %>% 
  st_as_sf(wkt = "location", crs = 4326) %>% 
  st_transform(2263) 


# litter baskets ----------------------------------------------------------
# get litter basket locations, buffer and create polygon
lburl <- "https://data.cityofnewyork.us/api/geospatial/8znf-7b2c?accessType=DOWNLOAD&method=export&format=Shapefile"
lbzip <- unzip_sf(lburl)
lb_points <- st_read(lbzip) %>% 
  st_as_sf(crs = 4326) %>% 
  st_transform(2263) %>% 
  select(c("basketid", "geometry"))

# what are the units 
st_crs(lb_points, parameters = TRUE)$units_gdal 
 
 lb_hex <- lb_points %>% 
    st_make_grid(cellsize = c(2000, 2000), square = FALSE) %>% # 2000 ft 
    st_sf() %>% # from sfc to sf 
    st_intersection(boro_sf)  %>% 
    st_join(lb_points) %>% 
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
