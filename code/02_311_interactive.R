library(tidyverse)
library(htmltools)
library(ggiraph)

# run 01_311_inital_request.Rmd first

# street cleanliness 
od_dl <- dsny_311
od_dl[complaint_type %in% c("Overflowing Litter Baskets", "Dirty Condition", "Dirty"), complaint_type := "Dirty Conditions"]
od_dl[grepl("Litter", complaint_type, ignore.case = TRUE) | grepl("litter", descriptor, ignore.case = TRUE), complaint_type := "Litter"]
od_dl_sub <- od_dl[complaint_type %in% c("Dirty Conditions", "Litter")]

od_dl_sub[, n_comp := .N, by = c("complaint_type", "date_ym")] # count by complaint type, date
od_dl_sub <- od_dl_sub[, .(complaint_type, date_ym, n_comp)] # subset to complaint type, date, n
od_dl_sub <- unique(od_dl_sub) # get rid of dupes
od_dl_sub <- od_dl_sub[od_dl_sub$date_ym >= "2022-08-01" & id_over_time_df$date_ym < "2023-08-01", ] # filter for 1 year
sum <- sum(od_dl_sub[od_dl_sub$complaint_type=="Dirty Conditions"]$n_comp) + 
  sum(od_dl_sub[od_dl_sub$complaint_type=="Litter"]$n_comp) # combine dirty conditions + litter n
complaint <- "Dirty Conditions"
type <- "311"
df_1 <- data.frame(sum, complaint, type)

# illegal dumping
id_over_time_df <- id_over_time_df[id_over_time_df$date_ym >= "2022-08-01" & id_over_time_df$date_ym < "2023-08-01", ]
sum <- sum(id_over_time_df$n_comp)
complaint <- "Illegal Dumping"
type <- "311"
df_2 <- data.frame(sum, complaint, type)

# derelict vehicles
dv_over_time_df <- dv_over_time_df[dv_over_time_df$date_ym >= "2022-08-01" & dv_over_time_df$date_ym < "2023-08-01", ]
sum <- sum(dv_over_time_df$n_comp)
complaint <- "Derelict Vehicles"
type <- "311"
df_3 <- data.frame(sum, complaint, type)

sum <- c(3040, 208665, 927) # rose pulled from OATH
complaint <- c("Derelict Vehicles", "Dirty Conditions", "Illegal Dumping")
type <- "OATH"
df_4 <- data.frame(sum, complaint, type)

# merge complaints
df_list <- list(df_1, df_2, df_3, df_4)    
complaints <- df_list %>% reduce(full_join, by=c('sum', "complaint", "type"))

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
htmltools::save_html(plot_interactive, "../visuals/complaints_311_citywide_bar.html")

