source("code/00_load_dependencies.R")

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
  st_transform(2263) %>%
  select("geometry")

# restaurant data  -------------------------------------------------------------
restaurants = fread("https://data.cityofnewyork.us/resource/43nn-pn8j.csv?$limit=9999999999") %>%
  select(latitude, longitude, bbl) %>%
  mutate(bbl = as.character(bbl)) %>%
  unique() %>%
  filter(!is.na(latitude)) %>%
  select(bbl, latitude, longitude)

# storefront data --------------------------------------------------------------
vsd = fread("https://data.cityofnewyork.us/resource/92iy-9c3n.csv?$limit=9999999999") %>%
  select(latitude, longitude, bbl) %>%
  unique() %>%
  filter(!is.na(latitude))  %>%
  mutate(bbl = as.character(bbl)) %>%
  select(bbl, latitude, longitude)


# 311 complaint data -----------------------------------------------------------
dsny_311 = fread("https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?agency='DSNY'&$where=created_date>='2022-08-01'&$limit=999999999999") %>%
  mutate(bbl = as.character(bbl)) %>%
  filter(!resolution_description %in% 
           c("The Department of Sanitation investigated this complaint and found no violation at the location.",
             "The Department of Sanitation investigated this complaint and found no condition at the location."), 
         !is.na(latitude), 
         complaint_type %in% c("Commercial Disposal Complaint", "Retailer Complaint")) 

# OATH data --------------------------------------------------------------------
oath = fread("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=issuing_agency%20like%20%27%25SANITATION%25%27%20and%20violation_date%20%3E=%20%272021-08-01%27")
oath = oath %>%
  filter(grepl("commercial|food|restaurant|business|retail|drink|bar|beverage|store|hotel|alcohol|chain|closing|organics", 
               charge_1_code_description, ignore.case=T), 
         hearing_result != "DISMISSED") %>%
  rename(boro = violation_location_borough,
         block = violation_location_block_no,
         lot = violation_location_lot_no, 
         unique_key = ticket_number) %>%
  select(boro, block, lot, unique_key) %>%
  mutate(boro = ifelse(boro == "QUEENS", 4, boro), 
         boro = ifelse(boro == "BROOKLYN", 3, boro), 
         boro = ifelse(boro == "MANHATTAN", 2, boro), 
         boro = ifelse(boro == "BRONX", 2, boro),  
         boro = ifelse(boro == "STATEN ISLAND", 5, boro), 
         bbl = paste0(boro,
                      str_pad(block, 5, "left", pad = 0),
                      str_pad(lot, 4, "left", pad = 0)))

# legally operating businesses  ------------------------------------------------
biz = fread("https://data.cityofnewyork.us/resource/p2mh-mrfv.csv?$limit=999999") %>%
  filter(license_type == "Business", 
         license_status == "Active", 
         !bbl %in% vsd$bbl, 
         !bbl %in% restaurants$bbl, 
         !is.na(latitude)) %>%
  mutate(bbl = as.character(bbl)) %>%
  select(bbl, latitude, longitude)

# ------------------------------------------------------------------------------
# combine data data 
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# combine all the businesses and count them @ hex poly level -------------------
bigbiz_sf = rbind(biz, vsd) %>%
  rbind(restaurants) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(2263)

# make hex polys
bizhex <- boro_sf %>%
  st_make_grid(cellsize = c(2000, 2000), square = FALSE) %>% # 2000 ft
  st_sf() %>% # from sfc to sf
  st_intersection(boro_sf)  %>%
  mutate(id = row_number()) %>%
  st_join(bigbiz_sf)


# ------------------------------------------------------------------------------
# combine oath data with pluto  ------------------------------------------------

# get lat and lon from pluto
pluto = fread("https://data.cityofnewyork.us/resource/64uk-42ks.csv?$limit=999999999&$select=bbl,latitude,longitude") %>%
  mutate(bbl = as.character(bbl))

oathpl = merge(oath, pluto, by = "bbl")
oath_sf = oathpl %>%
  filter(!is.na(latitude)) %>%
  select(unique_key, latitude, longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4226) %>%
  st_transform(2263)


oath311 <- rbind(oath_sf, dsny_311_biz)
rm(pluto)
rm(oath_sub)

hexdt <-  bizhex %>%
  st_join(oath311) %>%
  as.data.table()
wdt <-
  hexdt[!is.na(unique_key),] # remove area w no complaints - not interesting

# how many complaints re biz per geom
wdt[, .N, by = c("id", "unique_key")][order(N, decreasing = T)]

# biz per geom/# cmplnts per geom
wdt[, n_bz := length(unique(bbl)), by = "id"]
wdt[, n_cmp := length(unique(unique_key)), by = "id"]
wdt[, cmp_2_biz := 100 *round(n_cmp / n_bz, 2)]
wdt[, cmpt := ifelse(nchar(unique_key)==8, 1, 0), by = "id"]
wdt[, vio := ifelse(nchar(unique_key) > 8, 1, 0), by = "id"]

# grab streets from LIONS; zip: 'https://data.cityofnewyork.us/download/2v4z-66xt/application%2Fx-zip-compressed'
# to understand geography better
dcm <- st_read("data/input/DCM_ArterialsMajorStreets.shp", "DCM_ArterialsMajorStreets") %>% 
  st_as_sf(crs = 4326) %>% 
  st_transform(2263)

wdt_sf <- wdt[, .(geometry, cmp_2_biz, n_cmp,
                  n_bz, id)] %>%
  distinct() %>%
  st_as_sf() %>% 
  st_transform("+proj=longlat +datum=WGS84")

summary(wdt_sf$cmp_2_biz)
hist(wdt_sf$cmp_2_biz)
quantile(wdt_sf$cmp_2_biz, .95)

badbz <- wdt[cmp_2_biz > 33, .(geometry, cmp_2_biz, n_cmp,
                  n_bz, id)] %>%
  distinct() %>%
  st_as_sf() 

bad_sts_bz <- st_intersection(dcm, badbz)
mapview::mapview(bad_sts_bz, z="cmp_2_biz")

# map biz complaints per biz ----------------------------------------------
pal <-
  leaflet::colorQuantile(
    palette = pal_nycc("warm"),
    n = 6,
    domain = unique(wdt_sf$cmp_2_biz)
  )

m_biz <- leaflet() %>%
  addCouncilStyle(add_dists = TRUE) %>%
  leaflet::addPolygons(
    data = wdt_sf,
    # fillColor = ~pal(cmp_2_biz),
    color =  ~ pal(cmp_2_biz),
    label = ~ paste0(cmp_2_biz),
    # popup = paste0("Number of Businesses: ", wdt_sf$n_bz, "<br/>",
    # "Number of Complaints/Violations: ", wdt_sf$n_cmp),
    opacity = 0,
    fillOpacity = .5
  ) %>%
  addLegend(
    pal = pal,
    values = unique(wdt_sf$cmp_2_biz),
    labFormat = function(type, cuts, p) {
      n = length(cuts)
      p = paste0(round(p * 100), '%')
      cuts = paste0(formatC(cuts[-n], format = "d"), " - ", formatC(cuts[-1], format = "d"))
      # mouse over the legend labels to see the percentile ranges
      paste0('<span title="', p[-n], " - ", p[-1], '">', cuts,
             '</span>')
    },
    position = "topleft",
    title = "Complaints/Violations <br/> for every 100 Businesses"
  )
m_biz
mapview::mapshot(m_biz, "visuals/dsny_biz_cmplts_2022-sept2023.html")
