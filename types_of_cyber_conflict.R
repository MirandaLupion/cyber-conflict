library(readr)
library(tidyverse)
library(ggrepel)

data_raw <- read_csv("export-incidents.csv")

data_types <- data_raw %>%
  select(Type) %>%
  group_by(Type) %>%
  count(Type) %>%
  mutate(attack = 
           case_when(Type %in% c("Sabotage", "Data destruction", "Denial of service") ~ "y",
                      Type %in% c("Defacement", "Doxing", "Espionage") ~ "n",
                     Type == "Unclear" ~"UN")) %>%
  drop_na()

data_types <-data_types[order(data_types$n), ] 
data_types$Type <- factor(data_types$Type, levels = data_types$Type)



ggplot(data_types, aes(x = Type, y = n)) +
  geom_segment(aes(x = Type, 
                   xend = Type, 
                   y = 0, 
                   yend = n),
               
               color=ifelse(data_types$attack %in% c("y"), 
                            "darkgreen",  "navy"), 
               
               size=ifelse(data_types$attack %in% c("y"), 
                           4, 4)) +
  
  # geom_point(color=ifelse(data_types$attack %in% c("y"), 
  #                         "darkgreen", "navy"), 
  #            
  #            size=ifelse(data_types$attack %in% c("y"), 
  #                        3, 3)) +
  
  labs(title="Frequency of State Cyber Attacks by Type", 
       subtitle = "2006 - 2017",
       caption="Graphic: Miranda Lupion
       Data: The Council on Foreign Relations Cyber Operations Tracker",
       x = NULL,
       y = NULL) +
  
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()) +
  
  geom_label_repel(aes(x = Type, y = n, label = n), 
            size = 2.5, label.size = .05,
            hjust = "right", vjust = "top") 
  