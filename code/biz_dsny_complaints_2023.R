source("code/00_load_dependencies.R")
library(councilverse)
sf_use_s2(FALSE)

b_url <- "https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=Shapefile"
boro_zip <- unzip_sf(b_url)
boro_sf <- st_read(boro_zip) %>%
  st_as_sf(crs = 4326) %>%
  st_transform(2263) %>% 
  select("geometry")
##
## Sanitation Hearing 9/26/23
## Brook Frye

# restaurant data  -------------------------------------------------------------
rest <- fread("https://data.cityofnewyork.us/resource/43nn-pn8j.csv?$limit=9999999999")
rest_sub <- unique(rest[, .(latitude, longitude, bbl = as.character(bbl))])
rest_sf <- rest_sub[!is.na(latitude), ] %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
rm(rest)

# using vacant storefront data & dca data for businesses --------------------------------------------
vsd <- fread("https://data.cityofnewyork.us/resource/92iy-9c3n.csv?$limit=9999999999")
vsd_sub <- unique(vsd[, .(bbl = as.character(borough_block_lot),
                          latitude, longitude)])
vsd_sf <- vsd_sub[!is.na(latitude), ] %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
rm(vsd)

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
rm(biz)

# concat
big_biz <- rbind(vsd_sf, biz_sf)
big_biz2 <- rbind(big_biz, rest_sf)
bigbiz_sf <- st_as_sf(big_biz2, crs = 4326) %>% 
  st_transform(2263) 

rm(big_biz)  

# make hex polys 
 bizhex <-  boro_sf %>% 
  st_make_grid(cellsize = c(2000, 2000), square = FALSE) %>% # 2000 ft 
  st_sf() %>% # from sfc to sf 
  st_intersection(boro_sf)  %>% 
   mutate(id = row_number()) %>% 
   st_join(bigbiz_sf) 

# biz w complaints
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

# oath violations businesses ----------------------------------------------
# oath <- fread("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999")
oath <- fread("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=issuing_agency%20like%20%27%25SANITATION%25%27%20and%20violation_date%20%3E=%20%272018-01-01%27")

descs <- unique(oath$charge_1_code_description)
biz_descs <- descs[grep("commercial|food|restaurant|business|retail|drink|bar|beverage|
                        store|hotel|alcohol|chain|closing|organics", descs, ignore.case = TRUE)]
# remove dismissed?
oath_sub <- oath[!hearing_result %in% "DISMISSED" & charge_1_code_description %in% biz_descs, 
                 .(boro = violation_location_borough, block = violation_location_block_no, 
                   lot = violation_location_lot_no, unique_key = ticket_number)]
# make bbl
oath_sub[boro %in% "QUEENS", boro := 4]
oath_sub[boro %in% "BROOKLYN", boro := 3]
oath_sub[boro %in% "MANHATTAN", boro := 1]
oath_sub[boro %in% "BRONX", boro := 2]
oath_sub[boro %in% "STATEN ISLAND", boro := 5]
oath_sub[, bbl := paste0(boro, str_pad(block, 5, "left", pad = 0), str_pad(lot, 4, "left", pad = 0))]

# rm(oath)
# get lat and lon from pluto
pluto <- fread("https://data.cityofnewyork.us/resource/64uk-42ks.csv?$limit=999999999&$select=bbl,latitude,longitude")
pluto[, bbl := as.character(bbl)]
oathpl <- merge(oath_sub, pluto, by = "bbl") 

oathsf <-oathpl[!is.na(latitude),.(longitude, latitude, unique_key)] %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4226) %>% 
  st_transform(2263)
st_geometry(oathsf) <- "location"

oath311 <- rbind(oathsf, dsny_311_biz)
rm(pluto); rm(oath_sub)

hexdt <-  bizhex %>% 
  st_join(oath311) %>% 
  as.data.table()
wdt <- hexdt[!is.na(unique_key), ] # remove area w no complaints - not interesting

# how many complaints re biz per geom
wdt[, .N, by = c("id", "unique_key")][order(N, decreasing = T)]

# biz per geom/# cmplnts per geom
wdt[, n_bz := length(unique(bbl)), by = "id"]
wdt[, n_cmp := length(unique(unique_key)), by = "id"]
wdt[, cmp_2_biz := 100 * round(n_cmp/n_bz, 2)]

# hist(wdt$cmp_2_biz)

wdt_sf <- wdt[, .(geometry, cmp_2_biz, n_cmp, 
                  n_bz, id)] %>% 
  distinct() %>% 
  st_as_sf() %>% 
  st_transform('+proj=longlat +datum=WGS84') 

pal <- leaflet::colorQuantile(palette = pal_nycc("warm"), n = 6, domain = unique(wdt_sf$cmp_2_biz))

m_biz <- leaflet() %>% 
  addCouncilStyle(add_dists = TRUE) %>%
  leaflet::addPolygons(data = wdt_sf, 
                       # fillColor = ~pal(cmp_2_biz), 
                       color =  ~pal(cmp_2_biz), 
                       label = ~paste0(cmp_2_biz), 
                       # popup = paste0("Number of Businesses: ", wdt_sf$n_bz, "<br/>",
                               # "Number of Complaints/Violations: ", wdt_sf$n_cmp), 
                       opacity = 0, 
                       fillOpacity = .5) %>% 
  addLegend(pal = pal, 
            values = unique(wdt_sf$cmp_2_biz), 
            labFormat = function(type, cuts, p) {
              n = length(cuts)
              p = paste0(round(p * 100), '%')
              cuts = paste0(formatC(cuts[-n], format = "d"), " - ", formatC(cuts[-1], format = "d"))
              # mouse over the legend labels to see the percentile ranges
              paste0(
                '<span title="', p[-n], " - ", p[-1], '">', cuts,
                '</span>'
              )
            },
            position = "topleft", 
            title = "Complaints/Violations <br/> per 100 Businesses")
m_biz
mapview::mapshot(m_biz, "visuals/dsny_biz_cmplts_2022-sept2023.html")

# commercial waste zones --------------------------------------------------
# cwurl <- "https://data.cityofnewyork.us/api/geospatial/8ev8-jjxq?accessType=DOWNLOAD&method=export&format=Shapefile"
# cwzf <- unzip_sf(cwurl)
# cwz_sf <- st_read(cwzf) %>% 
#   st_simplify(preserveTopology = TRUE,
#               dTolerance = 100) %>%
#   st_cast("MULTIPOLYGON") 

