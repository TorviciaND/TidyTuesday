#Loading Data and Libraries
wheels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')

library(tidyverse)
library(ggforce)
library(elementalist)
library(extrafont)
library(remotes)
#remotes::install_version("Rttf2pt1", version = "1.3.8")
#extrafont::font_import()

#loadfonts(device = "win", quiet = F)

#Find the Ferris Wheel from each country with complete data, that is the largest
wheel <- wheels %>% 
  select(name, diameter, country, number_of_cabins, passengers_per_cabin) %>% 
  na.omit %>% 
  group_by(country) %>% 
  filter(diameter == max(diameter)) %>% 
  arrange(diameter)

#Prepping the data for graphing.  This is used to determine the center of each wheel
#and where the text will go for each wheel.
xscale = 400
yscale = 430
wheel$x <- xscale* rep(c(1, 3, 5, 7, 9), 2)
wheel$y <- yscale* rep(c(1, 3), each = 5)
wheel$text <- rep(c(yscale - 220, 3*yscale - 510), each = 5)

#Expanding the data set so that each cabin on each wheel has a data point.
#This code creates the location of the cabins so that they are equally 
#spaced around the wheel. Yay for Trig!
temp <- wheel
wheel <- data.frame(lapply(wheel, rep, wheel$number_of_cabins))
xcabin <- c()
ycabin <- c()

for(i in 1:nrow(temp)){
  degrees <- seq(0,360 - 360/temp$number_of_cabins[i], 360/temp$number_of_cabins[i]) 
  xtemp <- temp$x[i] + temp$diameter[i]/2*cos(degrees*pi/180)
  ytemp <- temp$y[i] + temp$diameter[i]/2*sin(degrees*pi/180)
  xcabin <- c(xcabin, xtemp)
  ycabin <- c(ycabin, ytemp)
}

#Adding arbitrary color to the cabins
wheel$xcabin <- xcabin
wheel$ycabin <- ycabin
wheel$color <- round(as.numeric(rownames(wheel)) %% 3)

#Background color - easy change here.
fillcolor <- "#81997C"

#Graphing
ggplot(data = wheel) +
  ylim(200,1700) + #Makes the plot big enough in the y direction so that the label isn't cut off
  geom_circle(aes(x0 = x, y0 = y, r = diameter/2)) + #Creates each Ferris wheel
  #These add the base segments of the Ferris wheel (purely aesthetic)
  geom_segment(aes(x = x, y = y, xend = x + 0.8*diameter*cos(4*pi/3), yend = y + 0.8*diameter*sin(4*pi/3)), size = 2) +
  geom_segment(aes(x = x, y = y, xend = x + 0.8*diameter*cos(5*pi/3), yend = y + 0.8*diameter*sin(5*pi/3)), size = 2) +
  geom_segment(aes(x = x, y = y, xend = xcabin, yend = ycabin), size = 0.25) + #Adds the spokes of the Ferris wheel
  scale_color_manual(values = c("red", "blue", "yellow")) + 
  geom_point(aes(x = xcabin, y = ycabin, size = passengers_per_cabin, color = as.factor(color))) + #Adds the cabins to the wheel
  geom_text(aes(x = x, y = text, label = paste(name, "\n", country)), size = 5, family = "Trebuchet MS", color = "#ffffff") + #Country info below each wheel
  theme(panel.background = element_rect(fill = fillcolor),
        plot.background = element_rect_round(fill = fillcolor, radius = unit(1, "cm")),
        legend.position = "none",
        panel.grid.major = element_line(color =fillcolor),
        panel.grid.minor = element_line(color =fillcolor),
        text = element_text(family = "Trebuchet MS", color = "#ffffff"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 10)), 
        plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
        plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm")) +
  labs(title = "Largest Ferris Wheels by Country", y = "", x = "",
       subtitle = "The largest ferris wheels by country, where complete data was provided in the Tidy Tuesday Ferris Wheel Data Set") +
  geom_segment(aes(x = 1161.5, y = 1483.2, xend = 1125, yend = 1650)) + #Adds the label
  geom_label(label = "Each point represents one cabin on the ferris wheel.\nThe size of the point is relative to\nthe number of passengers per cabin.", 
             x = 1125, y = 1650, fill = "#416B37", color = "#ffffff")


  