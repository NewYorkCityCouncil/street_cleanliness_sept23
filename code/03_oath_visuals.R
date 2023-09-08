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