pluto <- vroom("https://data.cityofnewyork.us/resource/64uk-42ks.csv?$limit=999999999&$select=bbl,latitude,longitude,bctcb2020")

## add population to bbl?

pad <- read_csv('data/input/bobaadr.txt', 
                col_select = c(boro,block,lot,stname,addrtype,segid)) %>% 
  distinct() %>% 
  drop_na(segid)

pad$bbl <- as.numeric(paste0(pad$boro,pad$block,pad$lot))


# JOIN TO PLUTO 
all_vios_bbl <- all_vios %>% 
  filter(!bbl %in% bbl[nchar(bbl)<10]) %>% # remove vios with missing bbls
  mutate(bbl = as.numeric(bbl) ) %>% 
  left_join(pluto, by = c('bbl')) %>% 
  filter(is.na(latitude)==F) #remove non-matching bbls/ na latitudes for mapping 

##### add littering !!!!!!!!!!!!! ---------
# year to date total  filtered to dirty sidewalk only
pad_vios <- all_vios_bbl %>% 
  filter(month >= '2022-08-01' & month < '2023-09-01') %>% 
  mutate(category= case_when(category=='Littering' ~ 'dirty sidewalk',
                             TRUE ~ category)) %>% 
  filter(category=="dirty sidewalk") %>% 
  group_by(bbl) %>% count(name='total') %>% 
  # ungroup() %>%
  # group_by(bbl) %>% 
  # bind_rows(summarise(category = "All_Violations", 
  #                     pad_vios, total = n())) %>% 
  right_join(pad %>% select(bbl, segid, stname, addrtype, boro), by = c('bbl'),
             relationship ='many-to-many')  %>% # JOIN TO PAD
  group_by(segid) %>% 
  reframe(total = sum(total, na.rm = T),
         boro = boro) %>% 
  distinct(segid, .keep_all = T)

# unzipped from 'https://data.cityofnewyork.us/download/2v4z-66xt/application%2Fx-zip-compressed'
lion <- read_sf("lion/lion.gdb", "lion") %>%  st_as_sf() %>% 
  #st_cast("MULTILINESTRING") %>% 
  st_transform("+proj=longlat +datum=WGS84") 

# check & clean subset
lion_bit <- lion[, c(1,3, 25:26, 79:82, 98:105,116:117,128)] 

lion_bit_clean <- lion_bit %>% 
  mutate(id= paste0(Street,SegmentID, SegCount,XFrom, YFrom, SHAPE)) %>% 
  filter(!duplicated(id)) # some issue with distinct

# lion_bit_bx <-lion_bit_clean %>% filter(Street =="CLAY AVENUE" )

# join violations to street segments, remove streets with no violations
lion_vios1 <- lion_bit_clean %>% 
   left_join(pad_vios, by= c('SegmentID'='segid'), keep = T) %>% 
   filter(!is.na(segid) & total!=0) %>% st_drop_geometry() %>% 
  group_by(Street, total) %>% # there are bbls with mutiple street segments but the same number of violations, merging those together
  summarise(total = mean(total),
            SHAPE_Length = sum(SHAPE_Length),
            LLo_Hyphen = min(unique(LLo_Hyphen)),
            LHi_Hyphen = max(unique(LHi_Hyphen)),
            SegmentID = first(SegmentID),
            boro = first(boro)) %>% 
  left_join(segs_4_pluto, by= c('SegmentID'='segid')) %>% 
  mutate(vios_per_length = total/SHAPE_Length,
         vios_per_bbl = total/n,
         #LLo_Hyphen = trimws(LLo_Hyphen, 'both'),
         #LHi_Hyphen = trimws(LHi_Hyphen, 'both'),
         clean_hyphen = case_when(LLo_Hyphen==LHi_Hyphen ~ LLo_Hyphen,
                         TRUE ~ paste(LLo_Hyphen, LHi_Hyphen, sep = " - ")),
         clean_hyphen = case_when(clean_hyphen=="NA - NA" ~ "",
                                  TRUE ~ clean_hyphen),
         full_address = paste(clean_hyphen, Street)) 
  
# get total bbl counts for each segment id for normalizing
segs_4_pluto <- lion_bit_clean %>% 
  left_join(pad_vios, by= c('SegmentID'='segid'), keep = T) %>% 
  filter(!is.na(segid) & total!=0) %>% st_drop_geometry() %>% 
  distinct(segid) %>% select(segid) %>% 
  left_join(pad %>% select(bbl, segid), by = c('segid')) %>% 
  group_by(segid) %>% count()

# for carto?
all_vios.shp <- all_vios_bbl %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326) %>% 


mapview(all_vios.shp %>% 
          filter(year==2022 & category=="dirty sidewalk"), 
        zcol = "category", col.regions = pal_nycc(), legend = TRUE,
        alpha.regions = 0.01, cex=2, lwd=0.01)


lion$types <- as.data.frame(st_geometry_type(lion))

lion_curve <- lion %>% filter(types=="MULTICURVE")
