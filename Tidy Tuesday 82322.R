#characteristics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/psych_stats.csv')

library(tidyverse)
library(ggwordcloud)
library(extrafont)
library(elementalist)
library(gridExtra)

#Removes the emojis from the data set
characteristics$personality <- iconv(characteristics$personality, "latin1", "ASCII", sub="")

#Find the highest ranking "feminist" characters
feminists <- characteristics %>% filter(personality == "feminist") %>% 
  arrange(rank)

feminist_characteristics <- characteristics %>% 
  filter(char_name %in% feminists$char_name[1:25] & personality != "feminist" & avg_rating >= 90) %>% 
  mutate(n = 1) %>%
  select(personality, n) %>% 
  group_by(personality) %>% 
  summarise(count = sum(n)) %>%
  filter(count > 4) %>%
  arrange(desc(count))

feminist_characteristics <- feminist_characteristics[2:21,]
feminist_characteristics <- feminist_characteristics %>% 
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))

fillcolor = "#49376B"
p1 <- ggplot(feminist_characteristics, aes(label = personality, size = count/4, angle = angle)) +
  geom_text_wordcloud(area_corr = TRUE) +
  scale_size_area(max_size = 16) +
  theme_minimal() +
  labs(title = "Feminist") +
  theme(panel.background = element_rect(fill = fillcolor, color = NA),
        plot.background = element_rect(fill = fillcolor),
        legend.position = "none",
        text = element_text(family = "Trebuchet MS", color = "#ffffff"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20, color = "#ffffff", margin = margin(b = 10)))

#Find the highest ranking "queer" characters
queer <- characteristics %>% filter(personality == "queer") %>% 
  arrange(rank)

queer_characteristics <- characteristics %>% 
  filter(char_name %in% queer$char_name[1:25] & personality != "queer" & avg_rating >= 90) %>% 
  mutate(n = 1) %>%
  select(personality, n) %>% 
  group_by(personality) %>% 
  summarise(count = sum(n)) %>%
  filter(count > 4) %>%
  arrange(desc(count))

queer_characteristics <- queer_characteristics[2:21,]
queer_characteristics <- queer_characteristics %>% 
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))

fillcolor = "#49376B"
p2 <- ggplot(queer_characteristics, aes(label = personality, size = count/4, angle = angle)) +
  geom_text_wordcloud(area_corr = TRUE) +
  scale_size_area(max_size = 16) +
  theme_minimal() +
  labs(title = "Queer") +
  theme(panel.background = element_rect(fill = fillcolor, color = NA),
        plot.background = element_rect(fill = fillcolor),
        legend.position = "none",
        text = element_text(family = "Trebuchet MS", color = "#ffffff"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20, color = "#ffffff", margin = margin(b = 10)))

#Find the highest ranking "autistic" characters
autistic <- characteristics %>% filter(personality == "autistic") %>% 
  arrange(rank)

autistic_characteristics <- characteristics %>% 
  filter(char_name %in% autistic$char_name[1:25] & personality != "autistic" & avg_rating >= 90) %>% 
  mutate(n = 1) %>%
  select(personality, n) %>% 
  group_by(personality) %>% 
  summarise(count = sum(n)) %>%
  filter(count > 4) %>%
  arrange(desc(count))

autistic_characteristics <- autistic_characteristics[2:21,]
autistic_characteristics <- autistic_characteristics %>% 
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))

fillcolor = "#49376B"
p3 <- ggplot(autistic_characteristics, aes(label = personality, size = count/4, angle = angle)) +
  geom_text_wordcloud(area_corr = TRUE) +
  scale_size_area(max_size = 16) +
  theme_minimal() +
  labs(title = "Autistic") +
  theme(panel.background = element_rect(fill = fillcolor, color = NA),
        plot.background = element_rect(fill = fillcolor),
        legend.position = "none",
        text = element_text(family = "Trebuchet MS", color = "#ffffff"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20, color = "#ffffff", margin = margin(b = 10)))

#Find the highest ranking "nerd" characters
nerd <- characteristics %>% filter(personality == "nerd") %>% 
  arrange(rank)

nerd_characteristics <- characteristics %>% 
  filter(char_name %in% nerd$char_name[1:25] & personality != "nerd" & avg_rating >= 90) %>% 
  mutate(n = 1) %>%
  select(personality, n) %>% 
  group_by(personality) %>% 
  summarise(count = sum(n)) %>%
  filter(count > 4) %>%
  arrange(desc(count))

nerd_characteristics <- nerd_characteristics[2:21,]
nerd_characteristics <- nerd_characteristics %>% 
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))

fillcolor = "#49376B"
p4 <- ggplot(nerd_characteristics, aes(label = personality, size = count/4, angle = angle)) +
  geom_text_wordcloud(area_corr = TRUE) +
  scale_size_area(max_size = 16) +
  theme_minimal() +
  labs(title = "Nerdy") +
  theme(panel.background = element_rect(fill = fillcolor, color = NA),
        plot.background = element_rect(fill = fillcolor),
        legend.position = "none",
        text = element_text(family = "Trebuchet MS", color = "#ffffff"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20, color = "#ffffff", margin = margin(b = 10)))

grid.arrange(p1, p2, p3, p4, nrow = 2, top = textGrob("Stereotypes in Television\nThe Most Common Characteristics of Characters that are Considered:", gp = gpar(fontsize = 24)))
