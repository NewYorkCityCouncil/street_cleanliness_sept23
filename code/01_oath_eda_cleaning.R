source('code/00_load_dependencies.R')

# read in OATH data 
# https://data.cityofnewyork.us/City-Government/OATH-Hearings-Division-Case-Status/jz4z-kudi
url <- c("https://data.cityofnewyork.us/resource/jz4z-kudi.csv?$limit=999999999999&$where=issuing_agency%20like%20%27%25SANITATION%25%27%20and%20violation_date%20%3E=%20%272018-01-01%27")
  
raw_oath <- vroom(url, col_select = c(1:15,23:24,34:42))

#raw_oath_bit <- raw_oath[,c(1:15,23:24,34:42)]

charge_summary <- raw_oath_bit %>% 
  group_by(charge_1_code) %>% 
  summarise(list = list(unique(charge_1_code_description)),
            n = lengths(list),
    charge_1_code_description = paste0(unlist(list(
    unique(charge_1_code_description))),collapse = " , "))  %>% as.data.frame()
# dont run again
# readr::write_csv(charge_summary, "data/output/charges.csv")

# made common grouping categories, used excel
cats <- read_csv("data/output/charges.csv")

cats_r <- cats %>% 
  group_by(relevant_charge) %>% 
  summarise(list_code = list(unique(charge_1_code)),
            n_code = lengths(list_code),
            charge_1_code = paste0(unlist(list(unique(charge_1_code))),collapse = " , "),
            list_desc = list(unique(charge_1_code_description)),
            n_desc = lengths(list_desc),
            charge_1_code_description = paste0(unlist(list(
              unique(charge_1_code_description))),collapse = " , "))  %>% as.data.frame()
