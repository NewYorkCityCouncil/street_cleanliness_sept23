source("code/00_load_dependencies.R")
library(councilverse)
##
## Sanitation Hearing 9/26/23
## Brook Frye 


# using vacant storefront data & dca data --------------------------------------------
vsd <- fread("https://data.cityofnewyork.us/resource/92iy-9c3n.csv?$limit=9999999999")
# vsd[borough_block_lot %in% "", ]
vsd_sub <- unique(vsd[, .(bbl = as.character(borough_block_lot), 
                                             biz=primary_business_activity, 
                                             latitude, longitude, 
                                             cd=council_district)])
vsd_sf <- vsd_sub[!is.na(latitude), ] %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# dca data
biz <- fread("https://data.cityofnewyork.us/resource/p2mh-mrfv.csv?$limit=999999")
# subset down 
biz_sub <- unique(biz[license_type %in% "Business" & license_status %in% "Active" & !location %in% "" &
                        !bbl %in% vsd_sub$bbl, 
                      .(biz = industry, 
                        geometry = location, 
                        bbl = as.character(bbl), 
                        cd=council_district)])

biz_sf <-  biz_sub %>% 
  st_as_sf(wkt="geometry", crs=4326)

# concat
big_biz <- rbind(vsd_sf, biz_sf)
bigbizdt <- as.data.table(big_biz)

bigbizdt[, n_biz_cd := .N, by = .(cd)]

# 311 complaints - dirty sidewalk + street complaints about retailers  ----------------------------------------
dsny_311 <- fread("https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?agency='DSNY'&$where=created_date>='2022-01-01'&$limit=999999999999")
dsny_311[, bbl := as.character(bbl)]
dsny_comps <- dsny_311[complaint_type %in% c("Commercial Disposal Complaint", "Retailer Complaint", "Dirty Condition") & 
                         location_type %in% c("Sidewalk", "Street") & !latitude %in% NA, ]


dsdt <- unique(dsny_comps[, .(descriptor, complaint = complaint_type, location_type, 
                              date = as.Date(created_date), bbl = as.character(bbl))])

ds_litter <- dsny_311[complaint_type %in% "Litter Basket Complaint", ]
ds_litter_sub <- ds_litter[!resolution_description %in% 
                             c("The Department of Sanitation investigated this complaint and found no violation at the location.", 
                               "The Department of Sanitation investigated this complaint and found no condition at the location."), ]
# join biz w complaints
vsd_dsny <- merge(bigbizdt, dsdt, by = "bbl")
vsd_dsny[, n_cmpts := .N, by = .(cd)]

# normalize by number of complaints by n biz/cd
vsd_dsny[, n_cmpts_p_biz := round(n_cmpts/n_biz_cd, 2)]

biz_comps <- unique(vsd_dsny[, .(cd, n_biz_cd, n_cmpts, n_cmpts_p_biz)])

# vsd_dsny %>% distinct()

# council district shapes -------------------------------------------------
url <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/nycc_22c.zip"
cd_shp <- unzip_sf(url)

cd <- read_sf(cd_shp) %>% 
  st_transform('+proj=longlat +datum=WGS84')

# dsny biz complaints per cd ----------------------------------------------
cd_biz <- cd %>% 
  left_join(biz_comps, by = c("CounDist" = "cd")) %>% 
  st_as_sf() %>% 
  st_transform('+proj=longlat +datum=WGS84')

# commercial waste zones --------------------------------------------------
cwurl <- "https://data.cityofnewyork.us/api/geospatial/8ev8-jjxq?accessType=DOWNLOAD&method=export&format=Shapefile"
cwzf <- unzip_sf(cwurl)
cwz_sf <- st_read(cwzf) %>% 
  st_simplify(preserveTopology = TRUE,
              dTolerance = 100) %>%
  st_cast("MULTIPOLYGON") 

# checks 
# biz_sub[location %in% "", ]
# how many businesses per cd 
# biz_sub[,n_biz_cd := .N, by = "cd"]
# how many empty bbls 
# biz_sub[bbl %in% "", ] # 862/28315 - not bad

