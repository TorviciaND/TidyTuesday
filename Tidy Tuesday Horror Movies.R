#Load the necessary libraries
#I hate horror movies and I'm not going to be able to sleep tonight after looking at some of these posters.

library(tidyverse)
library(jpeg) #allows for images in ggplot
library(ggimage) #allows for images in ggplot
library(ggrepel)
library(extrafont) #Use a different font in graphs
library(scales) #Remove scientific notation from axis labels
library(remotes) #Needed to install new ttf installer for fonts.


#Read in the Data
horror_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-01/horror_movies.csv')

#Format the data.  In particular, get out the year from the release data string and calculate the movie's profit
horror_movies$year = as.numeric(str_sub(horror_movies$release_date, start = 1,4))
horror_movies <- horror_movies %>% mutate(profit = revenue - budget)
horror_movies$path = paste0("https://image.tmdb.org/t/p/w1280", horror_movies$poster_path)

#Find the movie that had the highest profit for each year
horror_grouped <- horror_movies %>% group_by(year) %>% filter(profit == max(profit) & profit > 0 & year > 1969) %>% arrange(year)

#Update ttf installation program, install custom font from ttf file.
remove.packages("Rttf2pt1")   
remotes::install_version("Rttf2pt1", version = "1.3.8")
font_import(paths = "C:/Users/yourUserName/AppData/Local/Microsoft/Windows/Fonts")
loadfonts(device = "win")
fonts()

#Graphing parameters
textcolor = "#880808"
fillcolor = "#000000"

#Graph the highest profiting movies in a year with their poster.
#This will take a while.  R studio may even make you think it's done by removing the stop sign... but its not!  Be patient.
ggplot(data = horror_grouped, aes(x = year, y = log(profit), label = title)) + 
  geom_image(aes(image = path), size = 0.075) +
  scale_y_continuous(labels = comma) +
  labs(title = "Top Profiting Horror Movie by Year (1970 - 2022)", 
       x = "Year", y = "Log of Profit") +
  theme(plot.title = element_text(size = 12, family = "Bloody Terror", hjust = 0.5, color = textcolor),
        panel.background = element_rect(fill = fillcolor),
        panel.grid.major = element_line(color = fillcolor),
        panel.grid.minor = element_line(color = fillcolor),
        axis.title = element_text(family = "Bloody Terror", color = textcolor, size = 6),
        axis.text = element_text(family = "Bloody Terror", color = textcolor, size = 6),
        plot.background = element_rect(fill = fillcolor))

#Labels were added manually in MS Paint to identify hidden images.
#If ggrepel ever starts working for images, someone please email me!
