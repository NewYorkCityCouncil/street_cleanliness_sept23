library(tidyverse)
library(htmltools)
library(ggiraph)

# run 01_311_inital_request.Rmd first

od_dl[complaint_type %in% "Missed Collection (All Materials)", complaint_type := "Missed Collection"]
od_dl[grepl("Litter", complaint_type, ignore.case = TRUE) | grepl("litter", descriptor, ignore.case = TRUE), complaint_type := "Litter"]
od_dl[complaint_type %in% c("Overflowing Litter Baskets", "Dirty Condition", "Dirty"), complaint_type := "Dirty Conditions"]
od_dl_sub <- od_dl[complaint_type %in% c("Dirty Conditions", "Missed Collection", "Litter")]

# ---- 311 ----
# 2018 to present
# street cleanliness 
sum <- sum(od_dl_sub[od_dl_sub$complaint_type=="Dirty Conditions"]$n_comp) + 
  sum(od_dl_sub[od_dl_sub$complaint_type=="Litter"]$n_comp)
complaint <- "Dirty Conditions"
type <- "311"
df_1 <- data.frame(sum, complaint, type)

# illegal dumping
sum <- sum(id_over_time_df$n_comp)
complaint <- "Illegal Dumping"
type <- "311"
df_2 <- data.frame(sum, complaint, type)

# derelict vehicles
sum <- sum(dv_over_time_df$n_comp)
complaint <- "Derelict Vehicles"
type <- "311"
df_3 <- data.frame(sum, complaint, type)

sum <- c(3040, 208665, 927)
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
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11),
        legend.position = "none",
        axis.text.y = element_text(size = 11),
        plot.caption = element_text(hjust=0, size=10))

tooltip_css <- "background-color:#CACACA;"

plot_interactive <- girafe(ggobj = plot,   
                           width_svg = 6,
                           height_svg = 5, 
                           options = list(
                             opts_tooltip(css = tooltip_css)
                           )
)
htmltools::save_html(plot_interactive, "../visuals/complaints_311_citywide_bar.html")

