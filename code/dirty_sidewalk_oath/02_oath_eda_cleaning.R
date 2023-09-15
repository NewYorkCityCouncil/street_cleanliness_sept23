###########################################################
# YOU CAN SKIP THIS FILE
# RUN THIS PART ONLY TO GO OVER EDA !!!
##########################################################

source('code/00_load_dependencies.R')
# quick summary/eda look at the dataset -------------
options(scipen = 999)
skimr::skim(raw_oath_cats)
t <- sort(table(raw_oath_cats$relevant_charge), decreasing = T)
prop.table(t)*100


prep_oath <- raw_oath_cats %>% 
  select(!c(violation_location_floor, violation_description)) %>%  # drop uneeded columns
  filter(!relevant_charge %in% c('remove', 'not related',
                                 'obstruction', 'NONPUTRESCIBLE WASTE',
                                 'RECYCLING PROGRAM', 'SANITATION VIOLATION')) %>% # remove unrelated charges 
  mutate(block = str_pad(violation_location_block_no, 5, pad = "0"), 
         lot = str_pad(violation_location_lot_no, 4, pad = "0"), 
         borough = case_when(
           violation_location_borough == "BROOKLYN" ~ 3, 
           violation_location_borough == "QUEENS" ~ 4, 
           violation_location_borough == "BRONX" ~ 2, 
           violation_location_borough == "MANHATTAN" ~ 1, 
           violation_location_borough == "STATEN IS" ~ 5, 
           TRUE ~ 0
         ), 
         bbl = paste0(borough, block, lot)) # create bbl column

# get lat & lon from pluto matching on bbl

# check if missing bbls have addresses
missing_bbl <- prep_oath %>% filter(bbl %in% bbl[nchar(bbl)<10])
skimr::skim(missing_bbl)


##############################
## EDA TAKEAWAYS !!!!!!!! -------------

# half have house number and 99% have street number, maybe can get street centriod
#  may be worth pulling oath violations by identified charge codes in case issuing agency is mislabeled or other agencies give out sanitation related violations as well

# - dirty sidewalk makes about 60% of sanitation violations, 
# - rats, abandoned vehicle make less than 0.001%
# - illegal dumping makes 0.3%
#
# - drop violation_location_floor, violation_description 100% missing
# - there are no dup oaths, 100% unique
# - 5% of entries missing bbl info
# - 1% missing address info
# - 2% missing zipcode

# check all vios to sanitation subset to see if there are different trends
master_vios <- vroom("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=violation_date%3E=%272018-01-01T00:00:00%27&$select=violation_date,date_extract_y(violation_date)", delim = ",")

master_vios_clean <-master_vios %>% 
  filter(date_extract_y_violation_date<=2023) %>% 
  mutate(month = floor_date(as_date(violation_date), "month"))

table(master_vios_clean$date_extract_y_violation_date)
