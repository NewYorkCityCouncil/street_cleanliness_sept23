# for carto

# SODA API SQL Query for Reference ---------------
https://data.cityofnewyork.us/resource/erm2-nwe9.csv?$query=SELECT
`created_date`,
`agency`,
`complaint_type`,
`descriptor`,
`location_type`,
`incident_address`,
`resolution_description`,
`latitude`,
`longitude`,
`location`,
`bbl`,
`:@computed_region_92fq_4b7q` WHERE `created_date` >= "2022-09-01T00:00:00" :: floating_timestamp AND
(caseless_one_of(`agency`, "DSNY", "DEP")
  AND caseless_one_of(
    `complaint_type`,
    "Dirty Condition",
    "Dirty Conditions",
    "Missed Collection (All Materials)",
    "Missed Collection",
    "Sweeping/Inadequate",
    "Sweeping/Missed",
    "Sweeping/Missed-Inadequate",
    "Street Sweeping Complaint",
    "Street Condition",
    "Illegal Dumping",
    "Litter Basket Complaint",
    "Overflowing Litter Baskets",
    "DEP Sidewalk Condition",
    "Derelict Vehicle",
    "Derelict Vehicles",
    "Abandoned Vehicle",
  )) OR (caseless_one_of(
  `descriptor`,
  "Catch Basin Clogged/Flooding (Use Comments) (SC)",
  "Grease In Sewer/Catch Basin (IDG)"
)) LIMIT 999999999999

# url endcoded ----------
carto_url <- c('https://data.cityofnewyork.us/resource/erm2-nwe9.csv?$query=SELECT`created_date`,`agency`,`complaint_type`,`descriptor`,`location_type`,`incident_address`,`resolution_description`,`latitude`,`longitude`,`bbl`,`location`,`:@computed_region_92fq_4b7q`%20WHERE%20`created_date`%20%3E=%20%222022-09-01T00:00:00%22%20::%20floating_timestamp%20AND(caseless_one_of(`agency`,%20%22DSNY%22,%20%22DEP%22)%20%20AND%20caseless_one_of(%20%20%20%20`complaint_type`,%20%20%20%20%22Dirty%20Condition%22,%20%20%20%20%22Dirty%20Conditions%22,%20%20%20%20%22Missed%20Collection%20(All%20Materials)%22,%20%20%20%20%22Missed%20Collection%22,%20%20%20%20%22Sweeping/Inadequate%22,%20%20%20%20%22Sweeping/Missed%22,%20%20%20%20%22Sweeping/Missed-Inadequate%22,%20%20%20%20%22Street%20Sweeping%20Complaint%22,%20%20%20%20%22Street%20Condition%22,%20%20%20%20%22Illegal%20Dumping%22,%20%20%20%20%22Litter%20Basket%20Complaint%22,%20%20%20%20%22Overflowing%20Litter%20Baskets%22,%20%20%20%20%22DEP%20Sidewalk%20Condition%22,%20%20%20%20%22Derelict%20Vehicle%22,%20%20%20%20%22Derelict%20Vehicles%22,%20%20%20%20%22Derelict%20Bicycle%22,%20%20%20%20%22Abandoned%20Vehicle%22%20%20))%20OR%20(caseless_one_of(%20%20`descriptor`,%20%20%22Catch%20Basin%20Clogged/Flooding%20(Use%20Comments)%20(SC)%22,%20%20%22Grease%20In%20Sewer/Catch%20Basin%20(IDG)%22))%20LIMIT%20999999999999')

sanitation_311_carto <- vroom(carto_url)

carto_311<- sanitation_311_carto %>% 
  filter(created_date >='2022-09-01' & created_date <='2023-09-01') %>% 
  drop_na(latitude,bbl) %>% 
  mutate(complaint_type = case_when(
    descriptor == "Catch Basin Clogged/Flooding (Use Comments) (SC)" ~ 
      "Catch Basin Clogged/Flooding",
    descriptor == "Grease In Sewer/Catch Basin (IDG)" ~ 
      "Grease In Sewer/Catch Basin",
    TRUE ~ complaint_type)) %>% 
  filter(complaint_type != "Street Sweeping Complaint")
  

#write_csv(carto_311, 'data/output/for_carto_311_sanitation.csv')
#not pushed to github due to large size
