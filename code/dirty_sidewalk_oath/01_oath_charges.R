source('code/00_load_dependencies.R')

# RUN ONLY IF ITS THE FIRST TIME ---------------------
# RUN TO OBTAIN SANITATION RELATED CODES


# read in OATH data 
# https://data.cityofnewyork.us/City-Government/OATH-Hearings-Division-Case-Status/jz4z-kudi

# sanitation_url <- c("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=issuing_agency%20like%20%27%25SANITATION%25%27%20and%20violation_date%20%3E=%20%272018-01-01%27")

# read in only relevant columns to keep dataset smaller
# raw_oath_bit <- vroom(sanitation_url, col_select = c(1:15,23:24,34:42))


# charge_summary <- raw_oath_bit %>% 
#   group_by(charge_1_code) %>% 
#   summarise(list = list(unique(charge_1_code_description)),
#             n = lengths(list),
#             charge_1_code_description = paste0(unlist(list(
#               unique(charge_1_code_description))),collapse = " , "))  %>% 
#   as.data.frame()
# 
# write_csv(charge_summary, "data/output/charges.csv")

# manually grouped charge codes into categories in excel -------
# cats <- read_csv("data/output/charges.csv")
# write_csv(cats, "data/output/charges_grouped.csv")




# combine codes into groupings --------------
cats <- read_csv("data/output/charges_grouped.csv")
cats_r <- cats %>% 
  group_by(relevant_charge) %>% 
  summarise(list_code = list(unique(charge_1_code)),
            n_code = lengths(list_code),
            charge_1_code = paste0(unlist(list(unique(charge_1_code))),collapse = " , "),
            list_desc = list(unique(charge_1_code_description)),
            n_desc = sum(n),
            charge_1_code_description = paste0(unlist(list(
              unique(charge_1_code_description))),collapse = " , "))  %>% as.data.frame()


# read in categories to filter data
cats <- read_csv("data/output/charges_grouped_table.csv")

# unnest grouped code for matching later
cats_unnested <- cats %>% 
  mutate(list_code = map(list_code, ~tibble(list_code = .))) %>% 
  unnest(list_code)

# add categorization to main oath dataset
# raw_oath_cats <- raw_oath %>% left_join(cats_unnested, by=c('charge_1_code'='list_code'))

# too big to save, dont
#saveRDS(raw_oath_cats, "data/output/oath_2018-present_categories.csv")

# quick summary/eda look at the dataset
# options(scipen = 999)
# skimr::skim(raw_oath_cats)
# t <- sort(table(raw_oath_cats$relevant_charge), decreasing = T)
# prop.table(t)*100

# may be worth pulling oath violations by identified charge codes in case issuing agency is mislabeled
# - dirty sidewalk makes about 60% of sanitation violations, 
# - rats, abandoned vehicle make less than 0.001%
# - illegal dumping makes 0.3%
#
# - drop violation_location_floor, violation_description 100% missing
# - there are no dup oaths 100% unique
# - 5% of entries missing bbls info
# - 1% missing address info
# - 2% missing zipcode


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

# half have house number and 99% have street number, maybe can get street centriod