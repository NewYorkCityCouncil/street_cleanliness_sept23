# top issuing agencies -------
top_agencies <- all_vios %>% 
  group_by(year, issuing_agency) %>% 
  count() %>% arrange(desc(n)) %>% 
  group_by(year) %>% 
  slice_max(n, n=10) %>% 
  pivot_wider(names_from = year, values_from=n)

# Total Related Sanitation Violations Vs All Oath Violations Over Time -----

t <- 
  #table(all_vios$year) %>% 
  table(all_vios$month) %>% 
  data.frame() %>% 
  #setNames(nm = c('year','violations')) %>% 
  setNames(nm = c('month','violations')) %>% 
  mutate(type = rep('related_sanitation', nrow(.)))
t1<-
  #table(master_vios_clean$date_extract_y_violation_date) %>% 
  table(master_vios_clean$month) %>% 
  data.frame() %>% 
  #setNames(nm = c('year','violations')) %>% 
  setNames(nm = c('month','violations')) %>% 
  mutate(type = rep('all_oath', nrow(.)))

to_all_vios_yr <- rbind(t,t1)
to_all_vios_mon <- rbind(t,t1)

ggplot(to_all_vios_yr, aes(x=year, y=violations, group = type, color=type)) +
  geom_point() +
  geom_smooth(se=F) +
  #geom_line() +
  scale_color_nycc() +
  theme_nycc() +
  ggtitle("Related Sanitation to All Oath Violations Trend")

ggplot(to_all_vios_mon, aes(x=ymd(month), y=violations, 
                            group = type, color=type)) +
  geom_point() +
  geom_smooth(se=F, span=0.25) +
  #geom_line() +
  scale_x_date(date_labels = "%m-%Y", breaks = "6 months") +
  #scale_color_nycc() +
  theme_nycc() +
  ggtitle("Related Sanitation Vs All Oath Violations", "Monthly Trend")


# monthly trend all categories -----

monthly_trend <- all_vios %>% 
  group_by(month) %>% count(name = 'total') %>% ungroup() %>% 
  mutate(category = rep("All Violations", nrow(.)))

cat_monthly_trend <- all_vios %>% 
  mutate(category= case_when(category=='Littering' ~ 'dirty sidewalk',
                             TRUE ~ category)) %>% 
  group_by(month, category) %>% count(name = 'total') %>% 
  arrange(desc(total)) %>% 
  mutate(category = factor(category, 
                           levels=names(sort(table(all_vios$category), decreasing = T))))

together <-rbind(monthly_trend, cat_monthly_trend)

# reshape to do facet
monthly_trend <- monthly_trend %>% 
  left_join(cat_monthly_trend, by=c('month')) %>% 
  select(-category.x) %>% 
  mutate(percent = round((total.y / total.x) *100))

ggplot(cat_monthly_trend, aes(x=month, y=total, 
                            group = category, color=category)) +
  geom_point() +
  geom_smooth(se=F, span=0.25) +
  facet_wrap(. ~ category) +
  #geom_line() +
  scale_x_date(date_labels = "%m-%Y", breaks = "6 months") +
  theme_nycc() +
  #scale_color_nycc() +
  ggtitle("Related Sanitation Vs All Oath Violations", "Monthly Trend")


## dirty sidewalk alone -------

ds <- cat_monthly_trend %>% filter(category=="dirty sidewalk")
plot <- ggplot(ds, aes(x=month, y=total)) +
  geom_point_interactive(color= pal_nycc("cool")[7],
                         tooltip = paste0(ds$month, ": ", ds$total)) +
  geom_smooth(se=F, span=0.25, color = pal_nycc("cool")[7]) +
  #geom_line() +
  scale_x_date(date_labels = "%b-%Y", breaks = "6 months") +
  theme_nycc() +
  #scale_color_nycc() +
  ggtitle("Oath Violations: Dirty Side Walk", "Monthly Trend")

tooltip_css <- "background-color:#CACACA;"
plot_interactive <- girafe(ggobj = plot, # formatting for all
               width_svg = 10, height_svg = 6,
               options = list(
                 opts_tooltip(
                   opacity = 0.8, #opacity of the background box 
                   css = "background-color:#4c6061; color:white; padding:10px; border-radius:5px;"),
                 opts_hover_inv(css = "stroke-width: 1;opacity:0.6;"),
                 opts_hover(css = "stroke-width: 4; opacity: 1;")
               ))

htmltools::save_html(plot_interactive, "visuals/oath_dirty_sidewalk.html")

## illegal dumping alone----
df <- cat_monthly_trend %>% filter(category=="Illegal Dumping")
plot <- ggplot(df, aes(x=month, y=total)) +
  geom_point_interactive(color= pal_nycc("cool")[7],
                         tooltip = paste0(df$month, ": ", df$total)) +
  geom_smooth(se=F, span=0.25, color = pal_nycc("cool")[7]) +
  #geom_line() +
  scale_x_date(date_labels = "%b-%Y", breaks = "6 months") +
  theme_nycc() +
  #scale_color_nycc() +
  ggtitle("Oath Violations: Illegal Dumping", "Monthly Trend")


tooltip_css <- "background-color:#CACACA;"
plot_interactive <- girafe(ggobj = plot, # formatting for all
                           width_svg = 10, height_svg = 6,
                           options = list(
                             opts_tooltip(
                               opacity = 0.8, #opacity of the background box 
                               css = "background-color:#4c6061; color:white; padding:10px; border-radius:5px;"),
                             opts_hover_inv(css = "stroke-width: 1;opacity:0.6;"),
                             opts_hover(css = "stroke-width: 4; opacity: 1;")
                           ))

save_html(plot_interactive, "visuals/oath_illegal_dumping.html")

## derelict vehicle alone ----
df <- cat_monthly_trend %>% filter(category=="abandoning vehicle")
plot <- ggplot(df, aes(x=month, y=total)) +
  geom_point_interactive(color= pal_nycc("cool")[7],
                         tooltip = paste0(df$month, ": ", df$total)) +
  geom_smooth(se=F, span=0.25, color = pal_nycc("cool")[7]) +
  #geom_line() +
  scale_x_date(date_labels = "%b-%Y", breaks = "6 months") +
  theme_nycc() +
  #scale_color_nycc() +
  ggtitle("Oath Violations: Abandoning Vehicle", "Monthly Trend")


tooltip_css <- "background-color:#CACACA;"
plot_interactive <- girafe(ggobj = plot, # formatting for all
                           width_svg = 10, height_svg = 6,
                           options = list(
                             opts_tooltip(
                               opacity = 0.8, #opacity of the background box 
                               css = "background-color:#4c6061; color:white; padding:10px; border-radius:5px;"),
                             opts_hover_inv(css = "stroke-width: 1;opacity:0.6;"),
                             opts_hover(css = "stroke-width: 4; opacity: 1;")
                           ))

save_html(plot_interactive, "visuals/oath_abandoning_vehicle.html")
## dirtiest sidewalk table -----
# normalized
plot <- lion_vios %>% 
  slice_max(vios_per_length, n=10) %>% 
  select(full_address, vios_per_length, total) %>% 
  gt() %>%
  tab_header(
    title = "Streets with the Highest Number of Dirty Sidewalk Violations Per Foot",
    subtitle = "Year to Date (August 2022-Present)"
  ) %>%
  #  tab_source_note(source_note = "") %>%
  gt_theme_nytimes() %>% 
  tab_options(column_labels.font.weight = "160px")

plot %>% gtsave("visuals/dirtiest_streets.html")

#raw count
plot1 <- lion_vios %>% slice_max(total, n=10) %>% 
  select(full_address, total) %>% 
  gt() %>%
  tab_header(
    title = "Streets with the Highest Number of Dirty Sidewalk Violations Per Foot",
    subtitle = "Year to Date (August 2022-Present)"
  ) %>%
  #  tab_source_note(source_note = "") %>%
  gt_theme_nytimes() %>% 
  tab_options(column_labels.font.weight = "160px")

## dirtiest sidewalk map ---------

#plot(density(lion_vios$vios_per_length))
#extremely skewed - 

ints <- classIntervals(lion_vios$vios_per_length, n = 3, 
                      style = 'maximum', cutlabels=F)

pal_street <-  leaflet::colorBin(
  palette = c('#FFFFFF','#EEB6B1','#C67466','#800000'),
  bins = ints$brks,
  domain = lion_vios$vios_per_length,
  na.color = "#FFFFFF"
)

# fix multicurve issue 
# reference: https://github.com/r-spatial/sf/issues/2203#issuecomment-1634794519
fix_geom<-lion_vios[grepl("list\\(list",lion_vios$id)==T,]
fixed=c()
for(i in 1:dim(fix_geom)[1]){
  fix_geom$SHAPE[i]=st_cast(fix_geom$SHAPE[i], "MULTILINESTRING")
}

lion_vios.shp <- lion_vios %>% 
  filter(grepl("list\\(list",id)==F) %>% #drop the multicurve rows
  bind_rows(fix_geom) %>%  #add the multicurve rows fixed to multistring
  mutate(bin = case_when(vios_per_length<=ints$brks[2] ~ "#FFD8D8",
                         vios_per_length>ints$brks[2] &
                           vios_per_length<=ints$brks[3] ~"#EEB6B1",
                         vios_per_length>ints$brks[3] &
                           vios_per_length<=ints$brks[4] ~ "#C67466",
                         vios_per_length>ints$brks[4]~ "#800000"),
         size = case_when(bin == '#FFD8D8' ~ 2,
                          bin == '#EEB6B1' ~ 3,
                          bin == '#C67466' ~ 4,
                          bin == '#800000' ~ 5))

# separate each bin level to separate layer?

# make points for the top ten worst
top99975 <- lion_vios %>% 
  filter(vios_per_length>=3.59512) %>% 
  st_as_sf() %>% st_centroid()

map <- leaflet() %>% 
  #addCouncilStyle(add_dists = T) %>% 
  # addCircleMarkers(data= top10, stroke = F,
  #                  fillColor = pal_nycc("cool")[7],
  #                  radius = 5,
  #                  weight = 3,
  #                  fillOpacity = 0.7) %>% 
  addPolylines(data= lion_vios.shp,
               opacity = 1,
               weight = lion_vios.shp$size,
               color = ~lion_vios.shp$bin,
               popup = paste("<h4>",lion_vios.shp$full_address,"<hr>",
                             "<small>Violations Per Foot: </small>",
                             lion_vios.shp$vios_per_length,
                             "<br>",
                             "<small>Street Length: </small>", 
                             lion_vios.shp$SHAPE_Length,
                             "<br>",
                             "<small>Total Violations: </small>",
                             lion_vios.shp$total)) 
