# load required packages

library(rvest)
library(ggplot2)
library(purrr)
library(tidyverse)
library(stringr)
library(lubridate)
library(gganimate)
library(plotly)

# Extract from webscraping
table <- wikipage %>%
html_nodes("table.wikitable") %>%
html_table(header=T)
table <- table[[2]]
 
#add the table to a dataframe
con_tour_df <- as.data.frame(table)%>% filter(Country !="Total Foreigner")


names(con_tour_df)<- c("Rank","Country",2013,2014,2015,2016,2017)


con_tour_df <- con_tour_df%>% map(str_remove,",") %>% as.tibble()

con_tour_df<- head(con_tour_df,10)

con_tour_df <- pivot_longer(con_tour_df,-c(Country,Rank),names_to = "year")

con_tour_df$year <- as.numeric(con_tour_df$year)
con_tour_df$value <- as.numeric(con_tour_df$value)


con_tour_df <- con_tour_df%>%
  group_by(year) %>%
  mutate(rank = rank(-value),Value_lbl = paste0(" ",value))%>% filter(rank <= 10) 
  
  
  con_tour_df$value <- as.integer(con_tour_df$value)
  
 # Setup colors  code 
  colors <- c(
  "India" = "#FF7F24", "China" = "#E31A1C", "Sri Lanka" = "#FFB90F", "United States" = "#4876FF",
  "Thailand" = "#BF3EFF", "United Kingdom" = "#FF4040", "South Korea" = "#EE8262", "Germany" = "#8B7E66",
  "Australia" = "#2E8B57", "Myanmar" = "gray","Bangladesh" = "#006400","France"="#104E8B", "Japan" ="#CDB5CD"
)

anim <- ggplot(con_tour_df, aes(x = rank, y = value, group = Country)) +
  geom_bar(stat = "identity", aes(fill = Country)) +
  geom_text(aes(y = 0, label = paste(Country, " ")), vjust = 0.2, hjust = 1, size = 4.5) +
  geom_text(aes(y = value, label = Value_lbl, hjust = 0), size = 4.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  coord_flip(clip = "off", expand = FALSE) +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(size = .1, color = "grey"),
    panel.grid.minor.x = element_line(size = .1, color = "grey"),
    plot.title = element_text(size = 25, hjust = 0.5, face = "bold", vjust = 1),
    plot.subtitle = element_text(size = 18, hjust = 0.5, face = "italic"),
    plot.caption = element_text(size = 8, hjust = 0.5, face = "italic", color = "grey"),
    plot.background = element_blank(),
    plot.margin = margin(2, 2, 2, 4, "cm")
  ) +
  transition_states(year, transition_length = 3, state_length = 1) +
  enter_drift(x_mod = -2) +
  exit_drift(x_mod = 2) +
  ease_aes("cubic-in") +
  view_follow(fixed_x = TRUE) +
  labs(
    title = "Tourist Arrival by country : {closest_state}",
    subtitle = "Top 10 Countries"
  )
  
# create animation, declare the width height of the plot  
animate(anim, fps = 8, width = 1000, height = 550)
anim_save("simulations.gif")
