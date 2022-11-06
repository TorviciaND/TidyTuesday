#Load the data, bring in the necessary libraries
bigfoot <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bigfoot.csv')

library(tidyverse)
library(usmap)
library(maps)
library(stringr)
library(cowplot)

#Making space for the fips data by county and state
bigfoot$fips <- NA
bigfoot$statefips <- NA

#The county level fips function needs a specific state and county.  It won't go through naturally and
#take one state and county per row, so this needed to be done in a loop.
for(i in 1:length(bigfoot$state)){
  skip_to_next <- FALSE
  tryCatch(bigfoot$fips[i] <- fips(state = bigfoot$state[i], county = bigfoot$county[i]), error = function(e) {skip_to_next <- TRUE})
  if(skip_to_next){next}
}

#Getting the state level fips data is a lot easier!
bigfoot$statefips <- fips(state = bigfoot$state)

#Getting the count of sightings by county within a state
temp <- bigfoot
temp <- temp %>% 
  mutate(n = 1) %>% 
  select(fips, n, state) %>% 
  group_by(fips, state) %>% 
  summarize(count = sum(n))

#The three largest counts overall come from Washington, California and Ohio.
#Lets look at them further.

#Filter out the states that are Ohio, use the county.fips predefined objects to 
#extract the fips for counties with polynames containing "ohio".
ohio <- temp %>% 
  filter(state == "Ohio")
ohio_include <- county.fips$fips[str_detect(county.fips$polyname, "ohio")]
#uh oh, this does some silly stuff and creates things like "ohio.indiana".  This
#removes the dual state names.
ohio_include <- ohio_include[-c(1,2,91)]

#plot the ohio data.  Save it in p1 to be used later.
p1 <- plot_usmap(data = ohio, values = "count", regions = "counties", include = ohio_include) +
  scale_fill_continuous(low = "white", high = "darkblue", limits = c(0,75)) +
  theme(legend.position = "none")

#Repeat the process for California
california <- temp %>% 
  filter(state == "California")
california_include <- county.fips$fips[str_detect(county.fips$polyname, "california")]
for(i in 1:length(california_include)){
  california_include[i] <- paste0(0, california_include[i])
}

p2 <- plot_usmap(data = california, values = "count", regions = "counties", include = california_include) +
  scale_fill_continuous(low = "white", high = "darkblue", limits = c(0,75))

#Repeat the process for Washington.
washington <- temp %>% 
  filter(state == "Washington")
washington_include <- county.fips$fips[str_detect(county.fips$polyname, "washington")]

#A lot of other states have washington counties.  This was easy to find though because
#the listings are alphabetical by state name.
washington_include <- washington_include[-c(1:31,74)]

p3 <- plot_usmap(data = washington, values = "count", regions = "counties", include = washington_include) + 
  scale_fill_continuous(low = "white", high = "darkblue", limits = c(0,75)) +
  theme(legend.position = "none")

##################################
#Now to work with state level data
temp1 <- bigfoot

#Get the counts of sightings by state (by fips)
temp1 <- temp1 %>% 
  mutate(n = 1) %>% 
  select(statefips, n) %>% 
  group_by(statefips) %>% 
  summarize(count = sum(n))
temp1$fips <- temp1$statefips

#Graph the State data
p4 <- plot_usmap(data = temp1, values = "count") +
  scale_fill_continuous(low = "white", high = "darkred", limits = c(0,625)) +
  theme(legend.position = "right")

#This is in the cowplot library.  You can put different plots on the same area
#This took some guess and check to get them to not overlap.
ggdraw()+
  draw_plot(p4, x = .25, y = .1, width = .75, height = .75) + #usmap
  draw_plot(p1, x = .65, y = 0.75, width = 0.25, height = 0.25) + #ohio
  draw_plot(p2, x = 0, y = 0.1, width = 0.45, height = 0.45) + #california
  draw_plot(p3, x = 0.1, y = .7, width = 0.25, height = 0.25) +
  draw_label("Bigfoot Sightings Across the United States", x = 0.55, y = 0.9)

#For everything else on the graph I cheated and use MS Paint.  
  
  
