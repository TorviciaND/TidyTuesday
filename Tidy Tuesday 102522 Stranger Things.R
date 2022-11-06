#Load the necessary libraries
library(tm) #Used for Text Mining
library(syuzhet) #Sentiment Analysis
library(tidyverse) #Graphing
library(ggpubr) # Graph arrangement

################################################################################
#Data Cleaning - keep the dialogue that isn't null, turn it into an appropriate
#format to be able to do sentiment analysis on it.

stranger <- stranger[!is.na(stranger$dialogue),]
corpus <- iconv(stranger$dialogue)
senti <- get_nrc_sentiment(corpus) #This takes a while!
stranger <- cbind(stranger, senti)

################################################################################
#Create a data set that finds the average sentiment for the given emotions
#grouped by season and episode.

stranger_grouped <- stranger %>% 
  group_by(season, episode) %>% 
  summarize(anger_total = mean(anger), 
            antic_total = mean(anticipation), 
            disgust_total = mean(disgust), 
            fear_total = mean(fear),
            joy_total = mean(joy), 
            sadness_total = mean(sadness),
            surprise_total = mean(surprise), 
            trust_total = mean(trust),
            negative_total = mean(negative), 
            positive_total = mean(positive))

################################################################################
#Change column names for nicer graphing later on.

for(i in 1:length(stranger_grouped$season)){
  if(stranger_grouped$season[i] == 1){
    stranger_grouped$season[i] <- "Season 1"
  }
  else if(stranger_grouped$season[i] == 2){
    stranger_grouped$season[i] <- "Season 2"
  }
  else if(stranger_grouped$season[i] == 3){
    stranger_grouped$season[i] <- "Season 3"
  }
  else {
    stranger_grouped$season[i] <- "Season 4"
  }
}

################################################################################
#Create main graphic.  This shows sentiment over time for the different episodes
#over the different seasons.

p0 <- ggplot(stranger_grouped, aes(x = episode)) + 
  geom_line(aes(y = anger_total, color = "Anger")) +
  geom_line(aes(y = antic_total, col = "Anticipation")) +
  geom_line(aes(y = disgust_total, col = "Disgust")) +
  geom_line(aes(y = fear_total, col = "Fear")) +  
  geom_line(aes(y = joy_total, col = "Joy")) +
  geom_line(aes(y = sadness_total, col = "Sadness")) +
  geom_line(aes(y = surprise_total, col = "Surprise")) +
  geom_line(aes(y = trust_total, col = "Trust")) +
  geom_point(aes(y = anger_total, col = "Anger")) +
  geom_point(aes(y = antic_total, col = "Anticipation")) +
  geom_point(aes(y = disgust_total, col = "Disgust")) +
  geom_point(aes(y = fear_total, col = "Fear")) +  
  geom_point(aes(y = joy_total, col = "Joy")) +
  geom_point(aes(y = sadness_total, col = "Sadness")) +
  geom_point(aes(y = surprise_total, col = "Surprise")) +
  geom_point(aes(y = trust_total, col = "Trust")) +
  facet_wrap(~season, nrow = 4) +
  scale_color_manual(name = "Sentiment", values = c("Anger" = "red2", 
                                                    "Anticipation" = "green4", 
                                                    "Disgust" = "tan4",
                                                    "Fear" = "slateblue",
                                                    "Joy" = "goldenrod1",
                                                    "Sadness" = "dodgerblue2",
                                                    "Surprise" = "maroon1",
                                                    "Trust" = "darkorange2")) +
  labs(title = "Stranger Things Emotions by Season", x = "Episode", y = "Average Sentiment Value" ) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(.1, .1, .1, .1), "cm")) +
  scale_x_discrete(limits = 1:9, labels = factor(1:9))

################################################################################
#Create pie charts
group <- c("Anger", "Anticipation", "Disgust", "Fear", "Joy", "Sadness", "Surprise", "Trust")

#Find the mean value of each sentiment by season, rather than by episode.
stranger_pie <- stranger_grouped %>% group_by(season) %>% 
  summarize(anger_total1 = mean(anger_total), 
            antic_total1 = mean(antic_total), 
            disgust_total1 = mean(disgust_total), 
            fear_total1 = mean(fear_total),
            joy_total1 = mean(joy_total), 
            sadness_total1 = mean(sadness_total),
            surprise_total1 = mean(surprise_total), 
            trust_total1 = mean(trust_total),
            negative_total1 = mean(negative_total), 
            positive_total1 = mean(positive_total))

#The data needs to be transposed to get it in the correct format for graphing
#Add the column names, remove unecessary rows and turn this back into a df.
stranger_pie <- t(stranger_pie)
colnames(stranger_pie) <- stranger_pie[1,]
stranger_pie <- stranger_pie[-c(1, 10, 11),]
stranger_pie <- data.frame(stranger_pie)

#Values were given as character since the season name was part of the column.
#Turn them back into numeric values.
for(i in 1:4){
  stranger_pie[,i] <- as.numeric(stranger_pie[,i])
}

stranger_pie <- cbind(stranger_pie, group)

#Create a pie chart for each season, to see if they have changed sentiment
#over the different emotions from season to season.

p1 <- ggplot(stranger_pie, aes(x = "", y = Season.1, fill = group)) +
  geom_bar(stat = "identity", width = 1, color = "white")+
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("red2", "green4", "tan4", "slateblue", "goldenrod1", "dodgerblue2", "maroon1", "darkorange2"))+
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

p2 <- ggplot(stranger_pie, aes(x = "", y = Season.2, fill = group)) +
  geom_bar(stat = "identity", width = 1, color = "white")+
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("red2", "green4", "tan4", "slateblue", "goldenrod1", "dodgerblue2", "maroon1", "darkorange2"))+
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

p3 <- ggplot(stranger_pie, aes(x = "", y = Season.3, fill = group)) +
  geom_bar(stat = "identity", width = 1, color = "white")+
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("red2", "green4", "tan4", "slateblue", "goldenrod1", "dodgerblue2", "maroon1", "darkorange2"))+
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

p4 <- ggplot(stranger_pie, aes(x = "", y = Season.4, fill = group)) +
  geom_bar(stat = "identity", width = 1, color = "white")+
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("red2", "green4", "tan4", "slateblue", "goldenrod1", "dodgerblue2", "maroon1", "darkorange2"))+
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

#Arrange all of the pie charts into one column, then arrange the pie charts
#next to the line graph.
p5 <- ggarrange(p1, p2, p3, p4, ncol = 1)
ggarrange(p5, NULL, p0, ncol = 3, widths = c(1, -0.3, 1))
