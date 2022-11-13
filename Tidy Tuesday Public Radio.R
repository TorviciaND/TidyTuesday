#Load the necessary libraries
library(tidyverse)
library(purrr)
library(tidyr)
library(janitor)
library(rvest)

#######################################################################################################
#Get the Data
tuesdata <- tidytuesdayR::tt_load('2022-11-08')

state_stations <- tuesdata$state_stations
station_info <- tuesdata$station_info

state_stations <- state_stations %>% right_join(station_info, by = c("call_sign"))

#This file needs to be downloaded to your computer and unzipped
raw_contour <- read_delim("FM_service_contour_current.txt",  delim = "|")

#Getting just the stations that include Public in their format
public_stations <- state_stations %>% filter(grepl("Public", format))

#######################################################################################################
#This is formatting the data by code provided on the tidy Tuesday website
all_states <- datasets::state.name
all_states <- gsub(" ", "_", all_states)
get_stations <- function(state){
  root <- paste0("https://en.wikipedia.org/wiki/List_of_radio_stations_in_", state) 
  tables <- read_html(root) %>% html_nodes("table")
  stations <- tables[1] %>% # ideally all pages will have same format
    html_table(header = TRUE) %>% 
    do.call(rbind, .) %>% 
    clean_names() %>% 
    mutate(frequency = as.character(frequency)) %>%                    # handling Ohio special case (frequency / band split)
    rename_if(startsWith(names(.), "city"), ~ ("city")) %>%            # handling naming issues, citation handler
    rename_at(vars(matches("format")), ~ "format") %>%                 # handling naming issues, Oklahoma handler
    rename_if(startsWith(names(.), "licensee"), ~ ("licensee")) %>%    # handling naming issues, citation handler
    rename_if(startsWith(names(.), "owner"), ~ ("licensee")) %>%       # South Dakota handler
    select(call_sign, frequency, city, licensee, format) %>% 
    mutate(state = state)
  return(stations)
}

conv_contour <- raw_contour %>% 
     select(-last_col()) %>% 
     set_names(nm = c("application_id", "service", "lms_application_id", 
                      "dts_site_number", "transmitter_site", glue::glue("deg_{0:360}")))

lng_contour <- conv_contour %>% 
     separate(transmitter_site, into = c("site_lat", "site_long"), sep = " ,") %>% 
     pivot_longer(names_to = "angle", values_to = "values", cols = deg_0:deg_360) %>% 
     mutate(angle = str_remove(angle, "deg_"), angle = as.integer(angle)) %>% 
     separate(values, into = c("deg_lat", "deg_lng"), sep = " ,")

lng_contour$application_id <- as.numeric(lng_contour$application_id)
lng_contour$site_lat <- as.numeric(lng_contour$site_lat)
lng_contour$site_long <- as.numeric(lng_contour$site_long)

######################################################################################################
#Finding the call letters belonging to the different application IDs so that the two datasets
#that were provided on Tidy Tuesday can be linked.  So that you don't have to webscrape, the 
#file application.id.csv has been added to this repo.

application_id <- unique(lng_contour$application_id)
application_id <- data.frame(application_id)
application_id$call_sign <- NA

for(i in 1:length(application_id$application_id)){
  #for(i in 1:length(application_id$application_id)){
  Sys.sleep(sample(10, 1) * 0.025)
  site <- paste("https://licensing.fcc.gov/cgi-bin/ws.exe/prod/cdbs/pubacc/prod/app_det.pl?Application_id=",application_id$application_id[i], sep="")
  radiopage <- read_html(site)
  temp <- radiopage %>%  html_nodes("td")
  temp <- temp[20] %>% html_text()
  temp <- gsub("\n", "", temp)
  temp <- gsub("(^[[:space:]]*)|([[:space:]]*$)", "", temp)
  application_id$call_sign[i] <- temp
}

######################################################################################################
#Join the Public Radio stations with the application id and then with the signals
public_stations <- public_stations %>% left_join(application_id, by = c("call_sign"))
public_stations <- public_stations[complete.cases(public_stations),]
public_stations <- public_stations %>% left_join(lng_contour, by = "application_id")
public_stations <- public_stations[complete.cases(public_stations),]

public_stations$deg_lat <- as.numeric(public_stations$deg_lat)
public_stations$deg_lng <- as.numeric(public_stations$deg_lng)
public_stations$site_lat <- as.numeric(public_stations$site_lat)
public_stations$site_long <- as.numeric(public_stations$site_long)

#####################################################################################################
#Mapping
my_map <- get_stamenmap(bbox=c(left=-125,bottom=25,right=-67,top=49),
                        zoom=6,maptype="toner-lite")
ggmap(my_map)+
  geom_point(data=public_stations,aes(x=as.numeric(deg_lng),y=as.numeric(deg_lat)),
                        size=0.25,color="darkslategray3") +
  labs(title = "Public Radio in the Continental United States", x = "", y = "") +
  theme(plot.title = element_text(size = 13, hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank())
