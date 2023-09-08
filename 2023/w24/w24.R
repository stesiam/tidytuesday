# Import libraries

library(readr)
library(dplyr)
library(ggplot2)
library(ggtext)
library(extrafont)
library(forcats) # Recoding and Reordering of # of months
library(lubridate) # convert interview date from yyyy-mm-dd -> yyyy

# Import dataset

safi_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-13/safi_data.csv')


for (i in 1:nrow(safi_data)){
  if(safi_data$months_lack_food[[i]] == "none"){
    safi_data$months_lack_food[[i]] = "No"
  }else{
    safi_data$months_lack_food[[i]] = "Yes" 
  }
}


data = safi_data %>%
  select(village, months_lack_food) %>%
  group_by(village, months_lack_food) %>%
  summarise(count = n()) %>%
  mutate(countT= sum(count)) %>%
  mutate(pct = round(count/countT * 100, digits = 1))


title = "<b>Lack of Food</b>"
subtitle = "Based on SAFI survey, the three observed villages have a big portion
of <br>theιr population which have reported food insufficiency to feed the <br>household.
The problem is greater in <b>God</b> village, as there is a <br>staggering <span style = 'color: red; font-weight:bold;'>90%</span> of
interviewees who declares food insecurity concerns."
caption = "Tidy Tuesday, week 24<br>stesiam, 2023"

plot = ggplot(data, aes(x="", y=pct, fill=months_lack_food)) +
  geom_bar(stat="identity", width=1) +
  geom_text(aes(x = 1.15, label = paste0(pct, "%")), 
            position = position_stack(vjust = .5),
            color = "white",
            size = 3,
            fontface = "bold",
            family = "Gentium Book Plus") +
  facet_wrap(~village) +
  coord_polar("y", start=0) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    fill = "Responses:",
    x = "",
    y = ""
  ) +
  scale_fill_manual(values  = c("No" = "#00bfc4", 
                                "Yes" = "#F8766D")) +
  theme_bw() +
  theme(
    plot.title = element_markdown(family = "Amiri", size = 25, hjust = 0.5, color = "white"),
    plot.subtitle = element_markdown(family = "EB Garamond", color = "white", hjust = 0.5),
    plot.caption = element_markdown(family = "EB Garamond", size= 12, color = "white"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid  = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold", color = "white"),
    legend.text = element_text(color = "white", size = 12),
    text = element_text(family = "EB Garamond"),
    legend.box.margin = margin(t=1,b=1),
    legend.background = element_rect(fill = "black"),
    strip.background = element_rect(fill = "red3"),
    strip.text = element_text(color = "white",
                              face = "bold",
                              margin = margin(t=5,b=5), 
                              size = 15),
    plot.background = element_rect(fill = "black", linetype = "blank"),
    panel.background = element_rect(fill = "black")
)

ggsave(
  filename = "2023/w24/w24-2023-tt.png",
  plot = plot,
  device = "png", 
  height = 4)

