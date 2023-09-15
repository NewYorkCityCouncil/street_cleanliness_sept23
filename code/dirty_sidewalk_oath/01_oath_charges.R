###########################################################
# YOUR CAN SKIP THIS FILE
# RUN THIS PART ONLY IF ITS THE FIRST TIME !!!
# RUN TO OBTAIN SANITATION RELATED CODES
##########################################################

source('code/00_load_dependencies.R')

# read in OATH data : https://data.cityofnewyork.us/City-Government/OATH-Hearings-Division-Case-Status/jz4z-kudi

# OATH Violations where Sanitation is the issuing agency from 2018 to Present
sanitation_url <- c("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=issuing_agency%20like%20%27%25SANITATION%25%27%20and%20violation_date%20%3E=%20%272018-01-01%27")

# read in only relevant columns to keep the dataset small
raw_oath_bit <- vroom(sanitation_url, 
                      col_select = c(1:15,23:24,34:42))


charge_summary <- raw_oath_bit %>%
  group_by(charge_1_code) %>%
  summarise(list = list(unique(charge_1_code_description)),
            n = lengths(list),
            charge_1_code_description = paste0(unlist(list(
              unique(charge_1_code_description))),collapse = " , "))  %>%
  as.data.frame()

#write_csv(charge_summary, "data/output/oath_codes/oath_charges.csv")

# I manually grouped charge codes into categories in excel -------
# I read in the edited oath_charges csv & saved as oath_charges_grouped
cats <- read_csv("data/output/oath_codes/oath_charges.csv")
#write_csv(cats, "data/output/oath_codes/oath_charges_grouped.csv")




# code for eda - 02_oath_eda_cleaning ---------
#  read in categories to filter data
cats <- read_csv("data/output/oath_codes/charges_grouped_table.csv")

# unnest grouped code for matching later
cats_unnested <- cats %>%
  mutate(list_code = map(list_code, ~tibble(list_code = .))) %>%
  unnest(list_code)

# add categorization to main oath dataset
raw_oath_cats <- raw_oath_bit %>% left_join(cats_unnested, by=c('charge_1_code'='list_code'))
