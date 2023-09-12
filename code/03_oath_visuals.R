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


## YTD total all categories bar chart ------
# plot
cat_monthly_trend <- all_vios %>% 
  mutate(category= case_when(category=='Littering' ~ 'dirty sidewalk',
                             TRUE ~ category)) %>% 
  filter(month >= '2022-08-01' & month < '2023-09-01') %>% 
  group_by(month, category) %>% count(name = 'total') %>% 
  arrange(desc(total)) %>% 
  mutate(category = factor(category, 
                           levels=names(sort(table(category), decreasing = T))))

bar_cat_vios <- cat_monthly_trend %>% 
  group_by(category) %>% 
  summarize(total=sum(total)) %>% 
  mutate(category = toupper(category))

write_csv(bar_cat_vios, "data/output/oath_ytd_categories.csv")

plot <- 
  bar_cat_vios %>% 
  ggplot(aes(x = reorder(category,total), 
             y=total, fill = category)) +
  geom_col_interactive(width = 0.6,
                       tooltip = 
                         paste(bar_cat_vios$category, 
                               "<br>Violations:", 
                               round(bar_cat_vios$total))) +
  
  coord_flip() +
  geom_text(show.legend = F, size = 3,
            label= paste0(round(bar_cat_vios$total, 0), " violations"), 
            nudge_x = 0, hjust=-0.15) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  ylab("Violations") + xlab("") +
  labs(title="Citywide Total Sanitation OATH Violations",
       subtitle = "(Year-to-Date)", 
       x="",  y="Violations") +
  theme_nycc()+
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11),
        legend.position = "none",
        axis.text.y = element_text(size = 11))

tooltip_css <- "background-color:#CACACA;"

plot_interactive <- girafe(ggobj = plot,   
                           width_svg = 6,
                           height_svg = 5, 
                           options = list(
                             opts_tooltip(css = tooltip_css)
                           )
)


htmltools::save_html(plot_interactive, "visuals/vios_ytd_total_citywide_bar.html")

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
df <- lion_vios1 %>% 
  arrange(desc(vios_per_bbl)) %>%  ungroup() %>% as.data.frame() %>% 
  select(full_address, boro,vios_per_bbl, total, n)  %>% 
  mutate(vios_per_bbl = round(vios_per_bbl,1),
  boro = case_when(
    boro == 3 ~ "BROOKLYN" , 
    boro ==  4 ~ "QUEENS", 
    boro ==  2 ~ "BRONX", 
    boro ==  1 ~ "MANHATTAN", 
    boro ==  5 ~ "STATEN ISLAND"
  ))
df <- df[1:20,] #slice was not working??
names(df) <- c("Address", "Borough", "Violations Per Property", "Total # of Violations", "Total # of Properties")

plot <- df %>% 
  gt() %>%
  tab_header(
    title = "Streets with the Highest Number of Dirty Sidewalk Violations",
    subtitle = "Year to Date (August 2022 - August 2023)"
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
# making custom bins   

# fix multicurve issue 
# reference: https://github.com/r-spatial/sf/issues/2203#issuecomment-1634794519
fix_geom<-lion_vios[grepl("list\\(list",lion_vios$id)==T,]
fixed=c()
for(i in 1:dim(fix_geom)[1]){
  fix_geom$SHAPE[i]=st_cast(fix_geom$SHAPE[i], "MULTILINESTRING")
}

# bin cut offs based on quantiles
cut_996 <- quantile(lion_vios.shp$vios_per_length,.996)
# 0%-99.5% of streets : < 1 violation per 1ft of a street's length

pal_street <-  leaflet::colorBin(
  palette = c('#EEB6B1','#C67466','#993123','#800000'),
  bins = c(0,1,4,9,max(lion_vios$vios_per_length)),
  domain = lion_vios$vios_per_length,
  na.color = "#FFFFFF"
)

# map

lion_vios.shp <- lion_vios %>% 
  filter(grepl("list\\(list",id)==F) %>% #drop the multicurve rows
  bind_rows(fix_geom) %>%  #add the multicurve rows fixed to multistring
  mutate(bin = case_when(vios_per_length< 1~ '#EEB6B1', 
                         vios_per_length>=1 & 
                           vios_per_length<4 ~ "#C67466",
                         vios_per_length>=4 &
                           vios_per_length<9 ~ '#993123',
                         vios_per_length>=9 ~ "#800000"),
         size = case_when(bin =='#EEB6B1' ~ 0.5,
                          bin =='#C67466' ~ 2.5,
                          bin =='#993123' ~ 6,
                          bin =='#800000' ~ 6.5) )



map <- leaflet() %>% 
  # leaflet(options = leafletOptions(minZoom = 11, maxZoom = 13,
  #                                  zoomControl = FALSE,
  #                                  dragging = T)) %>%
  # addCouncilStyle() %>% not working
  #leaflet.extras::setMapWidgetStyle(list(background= "white")) %>%
  addCouncilStyle(add_dists = TRUE, 
                  highlight_dists = c(9:10,16,18,26,37,3), 
                  highlight_color = "#800000") %>%
  addPolylines(data= lion_vios.shp,
               opacity = 0.6,
               weight = ~lion_vios.shp$size,
               color = ~pal_street(vios_per_length),
               popup = paste("<h4>",lion_vios.shp$full_address,"<hr>",
                             "<small>Violations Per Foot: </small>",
                             round(lion_vios.shp$vios_per_length,1),
                             "<br>",
                             "<small>Street Length: </small>", 
                             round(lion_vios.shp$SHAPE_Length),
                             "<br>",
                             "<small>Total Violations: </small>",
                             round(lion_vios.shp$total))) %>% 
  # addLegend_decreasing(position = "topleft", pal = pal_street, 
  #                      title = paste0("Violations Per Foot"),  
  #                      values = c(0, 1), opacity = 1, decreasing = T, 
  #                      na.label = "NA") %>% 
  leaflegend::addLegendBin(data = lion_vios.shp,
    position = "topleft",
                  pal= pal_street,
                  shape = "rect",
                  orientation = "horizontal",
                  values = lion_vios$vios_per_length,
                  title = paste("Violations Per Foot",'\n'),
  numberFormat = function(x) {format(round(x), trim = TRUE,
           scientific = FALSE)} )

htmlwidgets::saveWidget(map, file="visuals/dirtiest_streets_map.html", selfcontained = T)
