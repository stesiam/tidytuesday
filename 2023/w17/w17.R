library(readr)
library(dplyr)
library(tidyr)

library(glue)

library(ggplot2)
library(ggtext)
library(extrafont)



winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/winners.csv')
london_marathon <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/london_marathon.csv')

total_participants = london_marathon %>%
  select(Starters) %>%
  drop_na() %>%
  sum(.)
  
total_finishers = london_marathon %>%
  select(Finishers) %>%
  drop_na() %>%
  sum(.)


title = glue("<b>London Marathon Winners' Nationality</b>")
subtitle = glue("London Marathon is an annual marathon held in London.
                The first one held<br> in 1981. In total, London Marathon participants 
                over the years (until 2020) <br> counts to <span style = 'font-weight: bold;'>{round(total_participants/10^6, digits = 2)} </span>millions 
                with <span style = 'color:red; font-family: Gentium Book Plus; font-weight: bold;'> {paste0(round(total_finishers/total_participants*100, digits =1),'%')} </span> of them finishing the race.
                In <b> Elite</b> <br> events Kenyan athletes are dominating. 
                The UK athletes have analogous success <br>
                on <b>Wheelchair (T53/T54)</b> events.")
caption = "Tidy Tuesday, week <b>17</b><br>stesiam, 2023"

plot = winners %>%
  group_by(Nationality, Category) %>%
  summarise(n = n()) %>%
  filter(n >=3) %>%
  arrange(-n) %>%
  ggplot(.) +
  geom_col(aes(x = n, y = reorder(Nationality, n), fill = Category)) +
  geom_text(aes(x = n, y = reorder(Nationality, n), label = n),
            size = 4,
            family = "Ubuntu Condensed",
            color = "white",
            nudge_x = -0.8) +
  facet_wrap(~factor(Category, c("Men", 
                                 "Wheelchair Men", 
                                 "Women", 
                                 "Wheelchair Women")), 
             scales = "free") +
  scale_fill_manual(values  = c("Men" = "dodgerblue", 
                                "Wheelchair Men" = "dodgerblue3", 
                                "Women" = "pink", 
                                "Wheelchair Women" = "pink3")) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = "",
    y = ""
  ) +
  theme_classic() +
  theme(
    plot.title = element_markdown(family = "Amiri", size = 15),
    plot.subtitle = element_markdown(family = "EB Garamond"),
    plot.caption = element_markdown(family = "EB Garamond"),
    legend.position = "none",
    strip.background = element_rect(fill = "#ccccff", linetype = "blank"),
    strip.text = element_markdown(family = "EB Garamond", face = "bold"),
    plot.background = element_rect(color = NA),
    plot.margin = margin(t = 5, b = 5, r = 30, l =3),axis.text = element_text(family = "EB Garamond")
)

ggsave(
  filename = "2023/w17/w17-2023-tt.png",
  plot = plot,
  device = "png",
  height = 6)

