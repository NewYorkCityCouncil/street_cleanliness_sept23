library(tidyverse)
library(htmltools)
library(ggiraph)
library(zoo)
library(readr)
library(data.table)
library(councildown)

# 311 interactive plots

# separate but related to 01_311_initial_request.Rmd
# you don't have to run the first file if you just want the interactive plots,
# a lot of it is redundant

dsny_311 <- fread("https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?agency='DSNY'&$where=created_date>='2018-01-01'&$limit=999999999999")
setDT(dsny_311)
dsny_311[, created_date := as.Date(created_date)][, yr := year(created_date)][, mo := month(created_date)]
dsny_311[, date_ym:=as.yearmon(created_date)]

dep_311 <- fread("https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?agency='DEP'&$where=created_date>='2018-01-01'&$limit=999999999999")
setDT(dep_311)
dep_311[, created_date := as.Date(created_date)][, yr := year(created_date)][, mo := month(created_date)]
dep_311[, date_ym:=as.yearmon(created_date)]

# ---- street cleanliness 
od_dl <- dsny_311
od_dl[complaint_type %in% c("Overflowing Litter Baskets", "Dirty Condition", "Dirty"), complaint_type := "Dirty Conditions"]
od_dl[grepl("Litter", complaint_type, ignore.case = TRUE) | grepl("litter", descriptor, ignore.case = TRUE), complaint_type := "Litter"]
od_dl_sub <- od_dl[complaint_type %in% c("Dirty Conditions", "Litter")]
# > sum((od_dl_sub[od_dl_sub$date_ym > "2022-08-01" & od_dl_sub$complaint_type == "Dirty Conditions",])$n_comp)
# [1] 47537 - from old version, littering adds ~3k

od_dl_sub[, n_comp := .N, by = c("complaint_type", "date_ym")] # count by complaint type, date
od_dl_sub <- od_dl_sub[, .(complaint_type, date_ym, n_comp)] # subset to complaint type, date, n
od_dl_sub <- unique(od_dl_sub) # get rid of dupes
od_dl_sub <- od_dl_sub[od_dl_sub$date_ym >= "2022-08-01" & od_dl_sub$date_ym < "2023-09-01", ] # filter for 1 year
sum <- sum(od_dl_sub[od_dl_sub$complaint_type=="Dirty Conditions"]$n_comp) + 
  sum(od_dl_sub[od_dl_sub$complaint_type=="Litter"]$n_comp) # combine dirty conditions + litter n
complaint <- "Dirty Conditions"
type <- "311"
df_1 <- data.frame(sum, complaint, type)


# ---- illegal dumping
dsny_311_id <- dsny_311[dsny_311$complaint_type == "Illegal Dumping",]
id_over_time_df <- dsny_311_id %>% group_by(date_ym) %>% summarise(n_comp=n())
id_over_time_df <- id_over_time_df[id_over_time_df$date_ym >= "2022-08-01" & id_over_time_df$date_ym < "2023-09-01", ]
sum <- sum(id_over_time_df$n_comp)
complaint <- "Illegal Dumping"
type <- "311"
df_2 <- data.frame(sum, complaint, type)

# ---- derelict vehicles
dsny_311_dv <- dsny_311[dsny_311$complaint_type == "Derelict Vehicles",]
dv_over_time_df <- dsny_311_dv %>% group_by(date_ym) %>% summarise(n_comp=n())
dv_over_time_df <- dv_over_time_df[dv_over_time_df$date_ym >= "2022-08-01" & dv_over_time_df$date_ym < "2023-09-01", ]
sum <- sum(dv_over_time_df$n_comp)
complaint <- "Derelict Vehicles"
type <- "311"
df_3 <- data.frame(sum, complaint, type)

# https://data.cityofnewyork.us/City-Government/OATH-Hearings-Division-Case-Status/jz4z-kudi
sum <- c(3040, 208665, 927) # rose pulled from OATH
complaint <- c("Derelict Vehicles", "Dirty Conditions", "Illegal Dumping")
type <- "OATH"
df_4 <- data.frame(sum, complaint, type)

# ---- merge complaints
df_list <- list(df_1, df_2, df_3, df_4)    
complaints <- df_list %>% reduce(full_join, by=c('sum', "complaint", "type"))

# ---- make interactive plot
options(scipen=10000)
plot <- complaints %>% 
  ggplot(aes(x = reorder(complaint,sum), 
             y=sum, fill = type)) + 
  geom_bar_interactive(stat = "identity", position = 'dodge',
    width = 0.6,
    tooltip =
      paste(
        complaints$complaint,
            "\nTotal:",
            complaints$sum,
        "\nType:",
        complaints$type
        )) +
  coord_flip() +
  labs(x="",  y="Totals",
       caption = "311 complaints for dirty conditions include litter.") +
  theme_nycc() +
  guides(fill=guide_legend(title="Type")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        plot.caption = element_text(hjust=0, size=10))

tooltip_css <- "background-color:#CACACA;"

plot_interactive <- girafe(ggobj = plot,   
                           width_svg = 6,
                           height_svg = 5, 
                           options = list(
                             opts_tooltip(css = tooltip_css)
                           ))
# htmltools::save_html(plot_interactive, "../visuals/complaints_311_citywide_bar.html")

# ---- plot additional concerns
# illegal dumping
id_plot <- id_over_time_df %>% 
  ggplot(aes(x = as.Date(date_ym), 
              y= n_comp)) +
  geom_line_interactive(size = 1.2,
                        alpha = 0.4)+
  geom_point_interactive(
    aes(tooltip = paste(date_ym,
      "\nComplaints:",n_comp
    )),
    fill = "white",
    size = 2.5,
    stroke = 1.5,
    shape = 21) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  labs(x="Date",  y="Number of Complaints") +
  theme_nycc()

tooltip_css <- "background-color:#CACACA;"

id_interactive <- girafe(ggobj = id_plot,   
                           width_svg = 8,
                           height_svg = 5, 
                           options = list(
                             opts_tooltip(css = tooltip_css)
                           ))
# htmltools::save_html(id_interactive, "../visuals/id_complaints_citywide_line.html")

dep_311_grease <- dep_311[grepl("grease", dep_311$descriptor, ignore.case = TRUE),]
# yellow/brown grease
grease_over_time_df <- dep_311_grease[dep_311_grease$date_ym >= "2022-08-01" & dep_311_grease$date_ym <= "2023-08-01",] %>% 
  group_by(date_ym) %>% 
  summarise(n_comp = n())

grease_plot <- grease_over_time_df %>% 
  ggplot(aes(x = as.Date(date_ym), 
             y= n_comp)) +
  geom_line_interactive(size = 1.2,
                        alpha = 0.4)+
  geom_point_interactive(
    aes(tooltip = paste(date_ym,
                        "\nComplaints:",n_comp
    )),
    fill = "white",
    size = 2.5,
    stroke = 1.5,
    shape = 21) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  labs(x="Date",  y="Number of Complaints") +
  theme_nycc()

grease_interactive <- girafe(ggobj = grease_plot,   
                         width_svg = 8,
                         height_svg = 5, 
                         options = list(
                           opts_tooltip(css = tooltip_css)
                         ))

# htmltools::save_html(grease_interactive, "../visuals/grease_complaints_citywide_line.html")

# derelict and abandoned vehicles
dv_plot <- dv_over_time_df %>% 
  ggplot(aes(x = as.Date(date_ym), 
             y= n_comp)) +
  geom_line_interactive(size = 1.2,
                        alpha = 0.4)+
  geom_point_interactive(
    aes(tooltip = paste(date_ym,
                        "\nComplaints:",n_comp
    )),
    fill = "white",
    size = 2.5,
    stroke = 1.5,
    shape = 21) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  labs(x="Date",  y="Number of Complaints") +
  theme_nycc()
dv_interactive <- girafe(ggobj = dv_plot,   
                         width_svg = 8,
                         height_svg = 5, 
                         options = list(
                           opts_tooltip(css = tooltip_css)
                         ))

# htmltools::save_html(dv_interactive, "../visuals/dv_complaints_citywide_line.html")






