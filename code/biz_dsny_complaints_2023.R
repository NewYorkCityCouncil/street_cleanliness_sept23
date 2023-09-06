source("code/00_load_dependencies.R")
library(councilverse)
##
## Sanitation Hearing 9/26/23
## Brook Frye 

# biz data ----------------------------------------------------------------
biz <- fread("https://data.cityofnewyork.us/resource/p2mh-mrfv.csv?$limit=999999")

# subset down 
biz_sub <- unique(biz[license_type %in% "Business" & license_status %in% "Active" & !location %in% "", 
                      .(industry, location, bbl = as.character(bbl), cd=council_district)])
biz_sub[, n_biz_cd := .N, by = "cd"]

# checks 
# biz_sub[location %in% "", ]
# how many businesses per cd 
# biz_sub[,n_biz_cd := .N, by = "cd"]
# how many empty bbls 
# biz_sub[bbl %in% "", ] # 862/28315 - not bad

# 311 complaints - dirty sidewalks ----------------------------------------
dsny_311 <- fread("https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?agency='DSNY'&$where=created_date>='2022-01-01'&$limit=999999999999")
dsny_311[, bbl := as.character(bbl)]
dsny_comps <- dsny_311[complaint_type %in% c("Commercial Disposal Complaint", "Retailer Complaint", "Dirty Condition") & 
                         location_type %in% c("Sidewalk", "Street"), ]
dsdt <- unique(dsny_comps[, .(descriptor, complaint = complaint_type, location_type, 
                              date = as.Date(created_date), bbl = as.character(bbl))])
# join biz w complaints
biz_cmpts <- merge(biz_sub, dsdt, by = "bbl")
biz_cmpts[, n_cmpts := .N, by = "cd"]

# normalize by number of complaints by n biz/cd
biz_cmpts[, n_cmpts_p_biz := round(n_cmpts/n_biz_cd, 2)]

# let's subset and join w cd sf
mapdt <- unique(biz_cmpts[, .(cd, n_cmpts, n_cmpts_p_biz)])

# cds
url <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/nycc_22c.zip"
cd_shp <- unzip_sf(url)

cd <- read_sf(cd_shp) %>% 
  st_transform('+proj=longlat +datum=WGS84')

cd_biz <- cd %>% 
  left_join(mapdt, by = c("CounDist" = "cd")) %>% 
  st_as_sf() %>% 
  st_transform('+proj=longlat +datum=WGS84')

ggplot(cd_biz, aes(x = n_cmpts_p_biz)) + geom_histogram() + 
  theme_bw()

# map dsny complaints/biz -------------------------------------------------
int_cd_biz <-
  classInt::classIntervals(cd_biz$n_cmpts_p_biz, n = 7, style = "headtails")

pal_biz = leaflet::colorBin(
  # issues with councildown's colorBin
  palette = c("#660000","#1850b5","#ba9f64","#1f3a70","#b3b3ff","#af6d46","#666666"),
  bins = int_cd_biz$brks,
  domain = round(cd_biz$n_cmpts_p_biz, 2),
  na.color = "White",
  reverse = TRUE
)

leaflet() %>% 
  addCouncilStyle(add_dists = TRUE) %>% 
  leaflet::addPolygons(data=cd_biz, 
                       fillColor = ~pal_biz(n_cmpts_p_biz), 
                       label = ~paste0(n_cmpts_p_biz), 
                       opacity = 0) %>% 
  addLegend(pal = pal_biz, 
            values = int_cd_biz$brks, 
            position = "topleft", 
            title = "Complaints per Business by <br/> Council District")


# load commercial waste zone boundaries -----------------------------------
cwurl <- "https://data.cityofnewyork.us/api/geospatial/8ev8-jjxq?accessType=DOWNLOAD&method=export&format=Shapefile"
cwzf <- unzip_sf(cwurl)
cwz_sf <- st_read(cwzf) %>% 
  st_simplify(preserveTopology = TRUE,
              dTolerance = 100) %>%
  st_cast("MULTIPOLYGON") 


# sfs  --------------------------------------------------------------

# biz_sf <- biz_sub %>% 
#   st_as_sf(wkt = "location") 

# pluto_url <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/nyc_mappluto_23v2_shp.zip"
# pluto_file <- unzip_sf(pluto_url)
# plt_sf <- st_read(pluto_file) %>%
#   st_simplify(preserveTopology = TRUE,
#               dTolerance = 100) %>%
#   st_cast("MULTIPOLYGON")

