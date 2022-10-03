artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-27/artists.csv')

library(tidyverse)
library(ggrepel) #This repels the labels from the points so they aren't overlapping
library(ggtext) #This is used to get text wrapping in the sub title below
library(dplyr)
library(plotly)

artists$pillars <- NA
artists$label <- "" #Adding labels for all points helps the ggrepel not overlap with points.  Without labels
#the function doesn't recognize them as something to avoid.

#Reclassify the types of Art under the 7 pillars of Art - Family Tree of Knowledge
artists <- artists %>% mutate(pillars = case_when(
  type == "Designers" | type == "Fine Artists, Art Directors, And Animators" ~ "Painting",
  type == "Landscape Architects" | type == "Architects" ~ "Architecture",
  type == "Writers And Authors" ~ "Literature",
  type == "Musicians" | type == "Music Directors And Composers" ~ "Music",
  type == "Dancers And Choreographers" ~ "Dance",
  type == "Producers And Directors" | type == "Entertainers" | type == "Actors" ~ "Drama",
  TRUE ~ "Other"
))

#This is the order that I want the pillars to graph.
artists <- artists %>% 
  mutate(pillars = fct_relevel(pillars, "Other", "Drama", "Dance", "Music", "Literature", "Architecture", "Painting"))

#Determine the states with the highest Location Quotient and add the state name as a label to those data points.
temp <- artists %>% 
  group_by(pillars, race, state) %>% 
  summarise(maxValue = max(location_quotient, na.rm = T))

temp <- temp %>% 
  group_by(pillars) %>% 
  summarize(newMax = max(maxValue, na.rm = T))

artists <- left_join(artists, temp, by = "pillars")

for(i in 1:length(artists$state)){
  if(round(artists$location_quotient[i],3) == round(artists$newMax[i],3) & !is.na(artists$location_quotient[i])){
    artists$label[i] = artists$state[i]
  }
} 

#Determine the states with the highest Artist Share and add the state name as a label to those data points.
#There is probably a more efficient way to do this.  This was all I could come up with.
temp1 <- artists %>% 
  group_by(pillars, race, state) %>% 
  summarise(maxValue1 = max(artists_share, na.rm = T))

temp1 <- temp1 %>% 
  group_by(pillars) %>% 
  summarize(newMax1 = max(maxValue1, na.rm = T))

artists <- left_join(artists, temp1, by = "pillars")

for(i in 1:length(artists$state)){
  if(round(artists$artists_share[i],5) == round(artists$newMax1[i],5) & !is.na(artists$artists_share[i])){
    artists$label[i] = artists$state[i]
  }
} 

#Set colors for graphing
fillcolor <- "#191919"
subcolor <- "#cccccc"
textcolor <- "#FFFFFF"

#Create my plot
ggplot(data = artists, aes(x = location_quotient, y = pillars, size = artists_share, color = race)) +
  geom_jitter(alpha = 0.4, position = position_jitter(seed = 1, width = 0, height = 0.2)) + 
  geom_text_repel(aes(label = label),
                  size = 3,
                  box.padding = 1.25,
                  segment.size = 0.75,
                  min.segment.length = 0, 
                  show.legend = F, 
                  position = position_jitter(seed = 1, width = 0, height = 0.2),
                  max.overlaps = 200) +
  #These pu the line segments between the different types of Art
  geom_segment(aes(x = 0, xend = 15.5, y = 6.5, yend = 6.5), color = subcolor, size = 0.1)+
  geom_segment(aes(x = 0, xend = 15.5, y = 5.5, yend = 5.5), color = subcolor, size = 0.1)+
  geom_segment(aes(x = 0, xend = 15.5, y = 4.5, yend = 4.5), color = subcolor, size = 0.1)+
  geom_segment(aes(x = 0, xend = 15.5, y = 3.5, yend = 3.5), color = subcolor, size = 0.1)+
  geom_segment(aes(x = 0, xend = 15.5, y = 2.5, yend = 2.5), color = subcolor, size = 0.1)+
  geom_segment(aes(x = 0, xend = 15.5, y = 1.5, yend = 1.5), color = subcolor, size = 0.1)+
  scale_color_brewer(palette = "Spectral") +
  labs(title = "Where should I go to become an Artist?",
       subtitle = "This graph depicts the best locations in the United States to go if you are looking for a job in the arts.  Both the highest location quotient (a measure of an artist occupation's concentration in the labor force, relative to the U.S. labor force share), as well as the artists share of total jobs within a state, by race (shown in the size of the points) are identified.",
       y = "Pillars of Art", 
       x = "Location Quotient",
       color = "Race")+
  guides(size = "none")+
  theme(plot.title = element_text(size = rel(2), family = "serif", hjust = 0.5, color = textcolor),
        plot.subtitle = element_textbox_simple(family = "serif", color = textcolor),
        #The simple textbox is what allows for the text wrap from ggtext
        axis.title = element_text(family = "serif", color = textcolor),
        axis.text = element_text(family = "serif", color = textcolor),
        legend.title = element_text(family = "serif", color = textcolor),
        legend.text = element_text(family = "serif", color = textcolor),
        legend.background = element_rect(fill = fillcolor),
        legend.key = element_rect(fill = fillcolor),
        panel.background = element_rect(fill = fillcolor),
        panel.grid.major = element_line(color = fillcolor),
        panel.grid.minor = element_line(color = fillcolor),
        plot.background = element_rect(fill = fillcolor))
