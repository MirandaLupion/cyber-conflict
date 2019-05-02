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


ggplot(data_target, aes(x = gov, fill = gov)) +
  
  geom_bar() +
  
  facet_wrap(~attack_y_n) +
  
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()) +
  
  labs(title="Frequency of State Cyber Operations by Target and Type ", 
       subtitle = "2005 - 2018",
       caption="Graphic: Miranda Lupion
       Data: The Council on Foreign Relations Cyber Operations Tracker",
       x = "Target",
       y = NULL) + 
  # scale_fill_discrete(guide=FALSE) +
  scale_fill_manual(guide=FALSE, values = c("Includes Government" = "#D55E00", "Non-Government" = "#0072B2"))

  