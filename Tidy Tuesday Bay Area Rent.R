#Importing Data
rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')
new_construction <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/new_construction.csv')

#Loading Libraries
library(tidyverse)
library(usmap)
library(gganimate)
library(transformr)
library(magick)



##### Cleaning the Rent Data
#Filter out observations where the square footage is unreasonable, or where there is no county or price listed.
#Create a new variable price per square foot
rent_cleaned <- filter(rent, sqft < 250000, price != "NA", county != "NA") %>% mutate(ppsf = price/sqft)

#Determine average rent by county.  Group first, then summarize
rent_cleaned_grouped <- group_by(rent_cleaned, county, year) %>% summarize('mean_price' = mean(price), 'mean_ppsf' = mean(ppsf))
rent_cleaned_grouped$year <- as.numeric(rent_cleaned_grouped$year)



##### Cleaning and joining the New Construction Data
#Remove "County" from the end of all county names in new construction data frame
new_construction$county <- str_to_lower(substr(new_construction$county,1,nchar(new_construction$county)-7))

#Create a new df with fewer variables to be joined to the cleaned and grouped rent data
new_construction_cleaned <- new_construction[c("county", "year", "totalproduction")]

#Join this new data set with the rent cleaned grouped data set
rent_cleaned_grouped <- left_join(rent_cleaned_grouped, new_construction_cleaned, by = c("county", "year"))

#Determine any county in the area without a value from the years 2001 to 2018.  Fill their values with NAs so
#they show up as gray (instead of blank) on the graphic.  Identify the FIPS of the counties given in each observation
rent_cleaned_grouped <- rent_cleaned_grouped %>% 
  group_by(year, county) %>% 
  complete(year = full_seq(2001:2018, 1)) %>% 
  fill() %>%  mutate(fips = fips("California", county = county))



##### Graphing
#identify the fips that should be used for graphing so only the bay area gets graphed.
fips_to_include <- unique(rent_cleaned_grouped$fips)

#Graphing price per square foot, price and new construction per county using plot_usmap function.  These are stored into
#variables so that they can be rendered together frame by frame into one big plot.

p1 <- plot_usmap(data = rent_cleaned_grouped, values = "mean_ppsf", regions = "counties", include = fips_to_include, labels = T)+
  labs(title = "Average Price per Square Foot for Rents in the Bay Area: {as.integer(previous_state)}") +
  scale_fill_continuous(low = "blue", high = "red") +
  transition_states(year)

p2 <- plot_usmap(data = rent_cleaned_grouped, values = "mean_price", regions = "counties", include = fips_to_include, labels = T)+
  labs(title = "Average Price for Rents in the Bay Area: {as.integer(previous_state)}") +
  scale_fill_continuous(low = "blue", high = "red") +
  transition_states(year)

p3 <- plot_usmap(data = rent_cleaned_grouped, values = "totalproduction", regions = "counties", include = fips_to_include, labels = T)+
  labs(title = "Total New Construction in the Bay Area: {as.integer(previous_state)}") +
  scale_fill_continuous(low = "red", high = "blue") +
  transition_states(year)

#Turn each of the plot_usmap objects into a magick object and animate
plot1 <- animate(p1, renderer = magick_renderer(), fps = 2)
plot2 <- animate(p2, renderer = magick_renderer(), fps = 2)
plot3 <- animate(p3, renderer = magick_renderer(), fps = 2)

#Creating a new gif object.  The first frame of each of the three magick items is put here.
new_gif <- image_append(c(plot3[1], plot2[1], plot1[1]))

#Subsequent frames are added to the plot
for(i in 2:100){
  combined <- image_append(c(plot3[i], plot2[i], plot1[i]))
  new_gif <- c(new_gif, combined)
}

#Final Product!
new_gif
