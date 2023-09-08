pluto <- vroom("https://data.cityofnewyork.us/resource/64uk-42ks.csv?$limit=999999999&$select=bbl,latitude,longitude,bctcb2020")

## add population to bbl

pad <- read_csv('data/input/bobaadr.txt', 
                col_select = c(boro,block,lot,stname,addrtype,segid)) %>% 
  distinct() %>% 
  drop_na(segid)

pad$bbl <- as.numeric(paste0(pad$boro,pad$block,pad$lot))

all_vios_bbl <- all_vios %>% 
  filter(!bbl %in% bbl[nchar(bbl)<10]) %>% # remove vios with missing bbls
  mutate(bbl = as.numeric(bbl) ) %>% 
  left_join(pluto, by = c('bbl')) %>% 
  filter(is.na(all_vios_bbl$latitude)==F)  %>% #remove non-matching bbls/ na latitudes for mapping 
  
# year to date total  
pad_vios <- all_vios_bbl %>% filter(month >='08-01-22') %>% 
  filter(category=="dirty sidewalk") %>% 
  group_by(bbl) %>% count(name='total') %>% 
  # ungroup() %>%
  # group_by(bbl) %>% 
  # bind_rows(summarise(category = "All_Violations", 
  #                     pad_vios, total = n())) %>% 
  right_join(pad %>% select(bbl, segid, stname, addrtype), by = c('bbl'),
             relationship ='many-to-many')

# unzipped from 'https://data.cityofnewyork.us/download/2v4z-66xt/application%2Fx-zip-compressed'
lion <- read_sf("lion/lion.gdb", "lion")

# check subset
lion_bit <- lion[, c(1, 25:26, 79:82, 98:105,128)] %>% 
  st_transform("+proj=longlat +datum=WGS84")


all_vios.shp <- all_vios_bbl %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326) %>% 


mapview(all_vios.shp %>% 
          filter(year==2022 & category=="dirty sidewalk"), 
        zcol = "category", col.regions = pal_nycc(), legend = TRUE,
        alpha.regions = 0.01, cex=2, lwd=0.01)

