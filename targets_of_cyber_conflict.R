library(readr)
library(tidyverse)
library(ggrepel)
library(stringr)

data_raw <- read_csv("export-incidents.csv")

data_raw$Category <- as.factor(str_to_title(data_raw$Category))

data_target <- data_raw %>%
  select(Category, Type) %>%
  drop_na() %>%
  group_by(Category, Type) %>%
  mutate(gov = 
           case_when(
             Category %in% c("Government", "Government, Civil Society", 
                            "Government, Civil Society, Private Sector", 
                            "Government, Military", "Government, Private Sector",
                            "Military", "Military, Civil Society", 
                            "Military, Private Sector",
                            "Government, Military, Private Sector") ~ "Includes Government",
             Category %in% c("Civil Society", "Civil Society, Private Sector",
                             "Private Sector") ~ "Non-Government"),
         
         attack_y_n = 
           case_when(Type %in% c("Sabotage", "Data destruction", "Denial of service") ~ 
                       "Cyber Attack (Sabotage, Data Destruction, DoS)",
                     Type %in% c("Defacement", "Doxing", "Espionage") ~ "Other Cyber Operations")) %>%
  drop_na()




# data_taget <-data_target[order(data_target$n), ]
# data_target$Category <- factor(data_target$Category, levels = data_target$Category)
# # 
# # 
# # 
ggplot(data_target, aes(x = gov, fill = gov)) +
  
  geom_bar() +
  
  facet_wrap(~attack_y_n) +
  
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()) +
  
  labs(title="Frequency of State Cyber Operations by Target and Type ", 
       subtitle = "2006 - 2018",
       caption="Graphic: Miranda Lupion
       Data: The Council on Foreign Relations Cyber Operations Tracker",
       x = "Target",
       y = NULL) + 
  scale_fill_discrete(guide=FALSE)

  
# #   # geom_point(color=ifelse(data_target$attack %in% c("y"), 
# #   #                         "darkgreen", "navy"), 
# #   #            
# #   #            size=ifelse(data_target$attack %in% c("y"), 
# # #   #                        3, 3)) +
# # #   
# labs(title="Frequency of State Cyber Attacks by Target, 2006 - 2018", 
#      subtitle = "sub",
#       caption="Graphic: Miranda Lupion
#         Data: The Council on Foreign Relations Cyber Operations Tracker",
#        x = NULL,
#        y = NULL) +
#   
#    theme_light() +
#    coord_flip() +
#    theme(
#      panel.grid.major.y = element_blank(),
#    panel.border = element_blank(),
#     axis.ticks.y = element_blank()) +
#   
#  geom_label_repel(aes(x = Category, y = n, label = n), 
#                 size = 2.5, label.size = .05,
#                   hjust = "right", vjust = "top") + 
#   facet_wrap(~Type)
