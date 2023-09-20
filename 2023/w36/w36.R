library(readr)
library(dplyr)
library(ggplot2)
library(ggtext)
library(glue)
library(stringr)


library(showtext)
library(sysfonts)

## Load fonts

sysfonts::font_add_google("EB Garamond", "eb")
sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-5.15.4-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')


showtext_auto()
showtext::showtext_opts(dpi = 300)
# Import datasets

demographics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/demographics.csv')
wages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/wages.csv')
states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv')

# Filter data

demographics = demographics %>%
  filter( facet %in% c("private sector: all", "public sector: all")) %>%
  mutate(p_members = round(p_members * 100, digits = 1))

# Title, subtitle and caption
last_year_private = demographics %>%
  select(year, p_members, facet) %>%
  filter(facet == "private sector: all") %>%
  filter(year == 2022) %>%
  .[["p_members"]]


## Find max perc of union members and year

find_max = demographics %>%
  select(year, p_members, facet) %>%
  group_by(facet) %>%
  filter(
    p_members == max(p_members)
  )

title = glue("Employees' participation in Unions")
subtitle = glue("In the early 70's the participation in unions was similar in private and public<br> sector.
                (~ 1 out of 4 empployees). Since then, the union participation in <span style = 'color: dodgerblue'>public <br>sector</span> has significantly increased. 
                On the contrary, <span style = 'color: red'>private sector</span> employees' <br>participation is decreasing year by year,
                reaching <span style = 'color: red'>{paste0(round(last_year_private, digits = 2), '%')} </span> in 2022.")
caption = "Tidy Tuesday, week 36<br><span style='font-family:fb; font-size:10px;'  >&#xf09b;</span> <b>stesiam</b>, 2023"




# Plot #1 - Percentage of workers in unions


plot = ggplot(data = demographics) +
  geom_line(aes(x = year, y = p_members, group = facet, color = facet), linewidth = 1.2) +
  geom_richtext(aes(x = 1990, y = 25, label = "Highest point of <br>Union participation"),
                family = "eb",
                size = 3,
                fill = NA, label.color = NA) +
  geom_curve(data = find_max, aes(x = 1991, xend = year, y = 28, yend = p_members, group = facet),
             arrow = arrow(ends = "last",
                           length = unit(0.1, "inches"),type = "closed")
  ) +
  geom_text(aes(x = year, y = p_members+2, label = ifelse(year %in% c(1980,1994,2000,2020),p_members, ""), group = facet),
            family = "eb", fontface = "bold") +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = "Year",
    y = "Percentage (%)"
  ) +
  theme_classic(
    base_size = 13,
    base_family = "eb"
  ) +
  scale_x_continuous(breaks = c(seq(1975,2025,5))) +
  scale_y_continuous(breaks = c(seq(0, 50, 5)))  +
  scale_color_discrete(labels = c("Private Sector", "Public Sector")) +
  theme(
    text = element_text(family = "eb"),
    plot.title = element_markdown(family = "eb", face = "bold",
                                  margin = margin(t = 2, b = 5)),
    plot.background = element_rect(fill = "cornsilk"),
    panel.background = element_rect(fill = "cornsilk"),
    plot.subtitle = element_markdown(family = "eb", lineheight = 1.2),
    plot.caption = element_markdown(family = "eb", lineheight = 1.2),
    legend.position = c(0.8,0.5),
    legend.background = element_rect(fill = "cornsilk"),
    legend.title = element_blank()
  )



ggsave(
  filename = "2023/w36/w36-2023-tt.png",
  plot = plot,
  device = "png",
  height = 4,
  width = 6
)

