############################################################
# Pull from API each category's codes to get all OATH violations, not just issued by Sanitation
#####################################################

source('code/00_load_dependencies.R')
cats <- read_csv("data/output/oath_codes/oath_charges_grouped.csv")

# combine codes into groupings --------------
cats_r <- cats %>% 
  group_by(relevant_charge) %>% 
  summarise(list_code = list(unique(charge_1_code)),
            n_code = lengths(list_code),
            charge_1_code = paste0(unlist(
              list(unique(charge_1_code))),collapse = " , "),
            list_desc = list(unique(charge_1_code_description)),
            n_desc = sum(n),
            charge_1_code_description = paste0(unlist(
              list(unique(charge_1_code_description))),
              collapse = " , "))  %>% 
  as.data.frame()

############################################################
# FINAL CATEGORIES USED FROM SANITATION BRIEFING FEEDBACK 
############################################################


# CAN BE MADE MORE EFFICIENT - WRITE UP A FUNCTION FOR THIS!
# 1: illegal dumping ------

# print codes
cats_r$charge_1_code[which(
  cats_r$relevant_charge %in% c('illegal dumping'))]

illdump_url<- c("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=violation_date%3E=%272022-08-01T00:00:00%27%20AND%20charge_1_code%20in(%27AS13%27,%27AS14%27,%27AS15%27,%27AS16%27,%27AA13%27,%27AS1Q%27)")

illegal_dumping <- vroom(illdump_url, 
                         col_select = c(1:15,23:24,34:42))

illegal_dumping$category <- rep('Illegal Dumping', nrow(illegal_dumping))

# 2: debris + littering + spill ------------

# print codes
litter_code <- cats_r$charge_1_code[which(
  cats_r$relevant_charge %in% c('debris', 'littering','spills'))]
paste0(unlist(strsplit(litter_code, " , ")), collapse = "' , '")

# encoded
litter_url <- c("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=violation_date%3E=%272022-08-01T00:00:00%27%20AND%20charge_1_code%20in(%27AS08%27%20,%20%27AS19%27%20,%20%27AS22%27%20,%20%27AS9C%27%20,%20%27AS9D%27%20,%20%27AS03%27%20,%20%27AS04%27%20,%20%27AS05%27%20,%20%27AS3C%27%20,%20%27AS09%27%20,%20%27AS9A%27%20,%20%27AS9B%27%20,%20%27AFK2%27%20,%20%27AFZ5%27%20,%20%27AH3I%27%20,%20%27AK09%27%20,%20%27AS3F%27%20,%20%27ASF3%27)")

littering <- vroom(litter_url, col_select = c(1:15,23:24,34:42))
littering$category <- rep('Littering', nrow(littering))


# 3: abandoned vehicle ----------------

aband_vech_url <- c("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=violation_date%3E=%272022-08-01T00:00:00%27%20AND%20charge_1_code%20in(%27AV01%27,%27AS24%27)")
# using abandoning & disabled vehicle codes
# disabled vehicle code found by using the query api function commented out below

# search_url <- c("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=violation_date%3E=%272018-01-01T00:00:00%27&$q=vehicle")

abandoned_vec <- vroom(aband_vech_url, col_select = c(1:15,23:24,34:42))
abandoned_vec$category <- rep('abandoning vehicle', nrow(abandoned_vec))

# 4: dirty sidewalk --------------

# print codes
dirty_sidewalk_code <- cats_r$charge_1_code[which(
  cats_r$relevant_charge %in% c('dirty sidewalk'))]
paste0(unlist(strsplit(dirty_sidewalk_code, " , ")), collapse = "' , '")

# encoded
dirty_sidewalk_url <- c("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=violation_date%3E=%272022-08-01T00:00:00%27%20AND%20charge_1_code%20in(%27AS06%27%20,%20%27AS26%27%20,%20%27AS6M%27%20,%20%27AS6V%27%20,%20%27AS8V%27%20,%20%27AS97%27%20,%20%27AT12%27%20,%20%27AT13%27)")

dirty_sidewalk <- vroom(dirty_sidewalk_url, col_select = c(1:15,23:24,34:42))

dirty_sidewalk$category <- rep('dirty sidewalk', nrow(dirty_sidewalk))


# combine all violations ---------

all_vios <- rbind(dirty_sidewalk, illegal_dumping, abandoned_vec, littering) %>% 
  select(!c(violation_location_floor, violation_description)) %>%  # drop uneeded columns
  mutate(block = str_pad(violation_location_block_no, 5, pad = "0"), 
         lot = str_pad(violation_location_lot_no, 4, pad = "0"), 
         borough = case_when(
           violation_location_borough == "BROOKLYN" ~ 3, 
           violation_location_borough == "QUEENS" ~ 4, 
           violation_location_borough == "BRONX" ~ 2, 
           violation_location_borough == "MANHATTAN" ~ 1, 
           violation_location_borough == "STATEN IS" ~ 5, 
           TRUE ~ 0
         ), # recode boros for bbl
         bbl = paste0(borough, block, lot), # create bbl column
         year = year(violation_date), # year month
         month = floor_date(as_date(violation_date), "month"))

# quick eda check
table(all_vios$year)

# ~1.6% missing bbl
missing_bbl <- all_vios %>% filter(bbl %in% bbl[nchar(bbl)<10])
skim(missing_bbl)

# drop missing bbls for now


#############################################################3
# CATEGORIES NO LONGER USED - KEEPING FOR FUTURE REFERENCE #######
# side: improper use of dsny litter basket ------

# basket_url <- c('https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=violation_date%3E=%272018-01-01T00:00:00%27&$q=litter%20basket')
# 
# litter_basket <- vroom(basket_url, col_select = c(1:15,23:24,34:42))


# 3: rats -- search the term -----------

# rats_url <- c("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=violation_date%3E=%272018-01-01T00:00:00%27%20AND%20charge_1_code%20in(%27AH50%27)")

# # rat query returns more results than search for the sanitation charge
# rats_url2 <- c("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=violation_date%3E=%272018-01-01T00:00:00%27&$q=rats")
# 
# rodent_url <- c("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=violation_date%3E=%272018-01-01T00:00:00%27&$q=rodent")
# 
# rats2 <- vroom(rats_url2, col_select = c(1:15,23:24,34:42))
# rats2$violation_location_house <- as.character(rats2$violation_location_house)
# rats2$category <- rep('Rats', nrow(rats2))
# 
# rodent <- vroom(rodent_url, col_select = c(1:15,23:24,34:42)) # this gets indoor rat conditions as well
# 
# # remove indoor rat
# patterns <- c('food','refrigerator', 'kitchen', 'grade pending',
#               'sink','housekeeping','freezer','basement',
#               'toilet','drain','test kit','customer',
#               'in the establishment','employees','door',
#               'floor','HOLES AND GAPS','sanitizing solution',
#               'mice', 'restroom')
# 
# rm_codes <- c("08A",'06C','06F','08B','10B','10G','16-0','18-0',
#               '20-0','28-0','AH3Q','AH4N','AH4P','B102','B106',
#               'B104','B202','B206','B254','B302','19-0','10F','10H','18D')
# 
# rodent_clean <- rodent %>% 
#   filter(grepl(paste(patterns, collapse="* |"), 
#                charge_1_code_description,
#                ignore.case = T)==F) %>% 
#   filter(!charge_1_code %in% rm_codes) %>% 
#   mutate(category = rep('Rats', nrow(.)))
# 4: storage receptacles & improper signage ---------
# storage_codes <- cats_r$charge_1_code[which(
#   cats_r$relevant_charge %in% c('storage receptacles', 'improper signage'))]
# paste0(unlist(strsplit(storage_codes, " , ")), collapse = "' , '")
# 
# storage_url <- c("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=violation_date%3E=%272018-01-01T00:00:00%27%20AND%20charge_1_code%20in(%27AD06%27%20,%20%27AR03%27%20,%20%27AR14%27%20,%20%27AR25%27%20,%20%27AR36%27%20,%20%27AR44%27%20,%20%27AR7E%27%20,%20%27ARE7%27%20,%20%27ARF1%27%20,%20%27ARF4%27%20,%20%27ARF7%27%20,%20%27ARG1%27%20,%20%27ARG4%27%20,%20%27ARH4%27%20,%20%27ARH7%27%20,%20%27ARI1%27%20,%20%27ARI4%27%20,%20%27ASC1%27%20,%20%27ASGP%27%20,%20%27ASU1%27%20,%20%27ASU2%27%20,%20%27ASU3%27%20,%20%27ASV4%27%20,%20%27ASV5%27%20,%20%27ASV6%27%20,%20%27ASW4%27%20,%20%27ASW5%27%20,%20%27ASW6%27%20,%20%27AS18%27%20,%20%27ASP1%27%20,%20%27ASP7%27%20,%20%27ASZ4%27%20,%20%27ASZ7%27%20,%20%27ADC9%27%20,%20%27AR04%27%20,%20%27AR05%27%20,%20%27AR07%27%20,%20%27AR15%27%20,%20%27AR18%27%20,%20%27AR1F%27%20,%20%27AR1G%27%20,%20%27AR1J%27%20,%20%27AR26%27%20,%20%27AR4F%27%20,%20%27AS8C%27%20,%20%27AS8D%27%20,%20%27ASA9%27%20,%20%27ASAC%27%20,%20%27ASAF%27%20,%20%27ASAI%27%20,%20%27ASAL%27%20,%20%27ASAO%27%20,%20%27ASAR%27%20,%20%27ASAU%27%20,%20%27ASAX%27%20,%20%27ASC3%27%20,%20%27ASC4%27%20,%20%27ASC7%27%20,%20%27ASP2%27%20,%20%27ASP3%27%20,%20%27ASZ6%27%20,%20%27ASZ9%27)")
# 
# storage <- vroom(storage_url, col_select = c(1:15,23:24,34:42))
# storage$category <- rep('Storage & Signage', nrow(storage))  
# 5: IMPROPER DISPOSAL ---------------
# improper_codes <- cats_r$charge_1_code[which(
#   cats_r$relevant_charge %in% c('IMPROPER DISPOSAL'))]
# paste0(unlist(strsplit(improper_codes, " , ")), collapse = "' , '")
# 
# improper_url <- c("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=violation_date%3E=%272018-01-01T00:00:00%27%20AND%20charge_1_code%20in(%27ASP4%27%20,%20%27AB01%27%20,%20%27AH30%27%20,%20%27AH3G%27%20,%20%27AR01%27%20,%20%27AR02%27%20,%20%27AR06%27%20,%20%27AR08%27%20,%20%27AR10%27%20,%20%27AR11%27%20,%20%27AR12%27%20,%20%27AR17%27%20,%20%27AR19%27%20,%20%27AR1A%27%20,%20%27AR1E%27%20,%20%27AR1H%27%20,%20%27AR21%27%20,%20%27AR22%27%20,%20%27AR2E%27%20,%20%27AR32%27%20,%20%27AR33%27%20,%20%27AR41%27%20,%20%27AR46%27%20,%20%27AR48%27%20,%20%27AR4E%27%20,%20%27AR4G%27%20,%20%27AR4H%27%20,%20%27AR51%27%20,%20%27AR6B%27%20,%20%27AR6H%27%20,%20%27AR76%27%20,%20%27AR79%27%20,%20%27AR7C%27%20,%20%27AR7F%27%20,%20%27AR7H%27%20,%20%27AR9H%27%20,%20%27ARA1%27%20,%20%27ARA7%27%20,%20%27ARB1%27%20,%20%27ARB4%27%20,%20%27ARC1%27%20,%20%27ARC4%27%20,%20%27ARC7%27%20,%20%27ARD1%27%20,%20%27ARD7%27%20,%20%27ARG7%27%20,%20%27ARH1%27%20,%20%27AS01%27%20,%20%27AS17%27%20,%20%27AS27%27%20,%20%27AS2A%27%20,%20%27AS2P%27%20,%20%27AS43%27%20,%20%27AS6B%27%20,%20%27ASCF%27%20,%20%27ASF4%27%20,%20%27ASF5%27%20,%20%27ASGD%27%20,%20%27ASGJ%27%20,%20%27ASP5%27%20,%20%27ASP6%27%20,%20%27ASP8%27%20,%20%27ASP9%27%20,%20%27ASU7%27%20,%20%27ASV1%27%20,%20%27ASY0%27%20,%20%27ASY4%27%20,%20%27ASY7%27%20,%20%27ASZ1%27)")
# 
# improper <- vroom(improper_url, col_select = c(1:15,23:24,34:42))
# improper$category <- rep('Improper Disposal', nrow(improper))

# 6: FAILURE TO RECYCLE -------------
# fail_recy_code <- cats_r$charge_1_code[which(
#   cats_r$relevant_charge %in% c('FAILURE TO RECYCLE'))]
# paste0(unlist(strsplit(fail_recy_code, " , ")), collapse = "' , '")
# 
# fail_recy_url <- c("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=violation_date%3E=%272018-01-01T00:00:00%27%20AND%20charge_1_code%20in(%27AR7G%27%20,%20%27ASR7%27%20,%20%27ASU4%27%20,%20%27ASX1%27%20,%20%27ASX4%27%20,%20%27ASX7%27%20,%20%27AR09%27%20,%20%27AR20%27%20,%20%27AR31%27%20,%20%27AR8G%27%20,%20%27ASR4%27%20,%20%27ASR8%27%20,%20%27ASR9%27%20,%20%27ASS1%27%20,%20%27ASS2%27%20,%20%27ASS3%27%20,%20%27ASS4%27%20,%20%27ASS5%27%20,%20%27ASS6%27)")
# 
# failed_to_rec <- vroom(fail_recy_url, col_select = c(1:15,23:24,34:42))
# failed_to_rec$category <- rep('Failure to Recycle', nrow(failed_to_rec))