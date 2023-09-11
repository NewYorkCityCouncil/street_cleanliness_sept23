library(tidyverse)
library(htmltools)
library(ggiraph)

# 2018 to present
# street cleanliness 
sum <- sum(od_dl_sub[od_dl_sub$complaint_type=="Dirty Conditions"]$n_comp)
complaint <- "Dirty Conditions"
df_1 <- data.frame(sum, complaint)

sum <- sum(od_dl_sub[od_dl_sub$complaint_type=="Missed Collection"]$n_comp)
complaint <- "Missed Collection"
df_1_2 <- data.frame(sum, complaint)

# illegal dumping
sum <- sum(id_over_time_df$n_comp)
complaint <- "Ilegal Dumping"
df_2 <- data.frame(sum, complaint)

# derelict vehicles
sum <- sum(dv_over_time_df$n_comp)
complaint <- "Derelict Vehicles"
df_3 <- data.frame(sum, complaint)

# catch basin complaints
sum <- sum(cb_over_time_df$n_comp)
complaint <- "Catch Basin"
df_4 <- data.frame(sum, complaint)

# grease
sum <- nrow(dep_311_grease)
complaint <- "Grease"
df_5 <- data.frame(sum, complaint)

# merge complaints
df_list <- list(df_1, df_1_2, df_2, df_3, df_4, df_5)    
complaints <- df_list %>% reduce(full_join, by=c('sum', "complaint"))

options(scipen=10000)
plot <- complaints %>% 
  ggplot(aes(x = reorder(complaint,sum), 
             y=sum, fill = complaint)) +
  geom_col_interactive(width = 0.6,
                       tooltip = 
                         paste(complaints$complaint, 
                               "\nComplaints:",
                               complaints$sum)) +
  coord_flip() +
  # geom_text(show.legend = F, size = 3,
  #           label= paste0(complaints$sum, " complaints"), 
  #           nudge_x = 0, hjust=-0.15) +
  labs(title="Citywide 311 Complaints",
       subtitle = "2018-Present", 
       x="",  y="Complaints") +
  theme_nycc() +
  theme(legend.position="none") +
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
htmltools::save_html(plot_interactive, "../visuals/complaints_311_citywide_bar.html")

