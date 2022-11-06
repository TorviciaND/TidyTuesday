#Load in Data and necessary packages
frogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv')
#install.packages("ggchicklet", repos = "https://cinc.rud.is")
#devtools::install_github("teunbrand/elementalist")

library(tidyverse)
library(ggchicklet)
library(RColorBrewer)
library(elementalist)
library(extrafont)

#With the extrafont package, this allows me to use different fonts in my graph
#font_import()
loadfonts(device = "win", quiet = T)

#I want the count of observations within the groups of Habitat Type by Structure
frogs1 <- frogs %>% 
  mutate(n = 1) %>% 
  select(HabType, Structure, n) %>% 
  group_by(HabType, Structure) %>% 
  summarise(count = sum(n))

#Info graphic about frogs and their habitats/structures
ggplot(data = frogs1, aes(x = HabType, fill = Structure, y = count)) +
  geom_chicklet(radius = grid::unit(5, "pt"), color = "#2460C9") +
  coord_flip() +
  scale_fill_brewer(palette = "Greens")+
  theme(panel.background = element_rect(fill = "#2460C9"),
        plot.background = element_rect_round(fill = "#2460C9", radius = unit(2, "cm")),
        legend.background = element_rect(fill = "#2460C9"),
        legend.key = element_rect(fill = "#2460C9"),
        panel.grid.major = element_line(color ="#2460C9"),
        panel.grid.minor = element_line(color ="#2460C9"),
        text = element_text(family = "constantia", color = "#FFFFFF"),
        axis.text = element_text(family = "constantia", color = "#FFFFFF"),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 10)), 
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm")) +
  labs(title = "Frog Habitats and Strucutres in Oregon", y = "", x = "",
       subtitle = "Pearl, C.A., Rowe, J.C., McCreary, B., and Adams, M.J., 2022 \nOregon spotted frog (Rana pretiosa) telemetry and habitat use at Crane Prairie Reservoir in Oregon, USA: \n U.S. Geological Survey data release, https://doi.org/10.5066/P9DACPCV.")
  
