chips <- read_csv("fakefilepath/chips.csv")

library(plotly)
library(tidyverse)
library(stringr)

chips$year <- substr(chips$`Release Date`, nchar(chips$`Release Date`)-3, nchar(chips$`Release Date`))
chips <- chips[chips$year != "NaT",]
chips$logTransistors <- log(chips$`Transistors (million)`)

titletxt <- "This graph shows us that there is a positive relationship between the logarithm of transistors\nand the MegaHertz of a processor. As the number of transistors goes up, the faster the processor\ncompletes a cycle.  CPUs have higher frequencies in general than GPUs.  This doesn't mean that\nCPUs are performing better though.  GPUs have as much as 10 times the processing elements that CPUs\nhave!  We can also see here that as time goes on GPUs are catching to CPUs in the Frequency and\nthat the size of the Processing units is getting smaller." 

p1 <- ggplot(chips, aes(x = `logTransistors`, y = `Freq (MHz)`, color = Type, frame = year))+
  geom_point(aes(size = `Process Size (nm)`))+
  scale_color_manual(values = c("#e5771e", "#75c8ae")) +
  ylim(0,6500) + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#f3b02e")
  )

ggplotly(p1) %>% 
  animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom") %>% 
  layout(title = list(text = paste0("\n\nComparing Specs of GPUs vs. CPUs", '<br><br>', '<sup>', titletxt, "</sup>")))
  
