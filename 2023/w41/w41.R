library(readr)
library(dplyr)

library(ggplot2)
library(ggtext)
library(glue)

library(showtext)
library(sysfonts)

library(usmap)

library(fuzzyjoin)
library(stringr) #erase comma separator for thpusands to make calculations

## Load fonts

sysfonts::font_add_google("Creepster", "title")
sysfonts::font_add_google("Pacifico", "pc")
sysfonts::font_add_google("Lilita One", "lo")
sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')


showtext_auto()
showtext::showtext_opts(dpi = 300)

# Import Data

haunted_places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-10/haunted_places.csv')


freq_states = haunted_places %>%
  group_by(state_abbrev) %>%
  summarise(n = n()) %>%
  mutate(pct = round(n/sum(n)*100, digits = 2)) %>%
  arrange(-n)

d = stringdist_join(freq_states, s2, 
                by='state_abbrev', #match based on team
                mode='left', #use left join
                method = "jw", #use jw distance metric
                max_dist=99, 
                distance_col='dist') %>%
  group_by(state_abbrev.x) %>%
  slice_min(order_by=dist, n=1)

d$LandArea = str_replace_all(d$LandArea, ",", "")

d$LandArea = as.numeric(d$LandArea)

d$nper1000km2 = d$n*1000/d$LandArea

d$state = d$state_abbrev.x
d = d %>%
  mutate(cat = case_when(
  nper1000km2 < 2 ~ "Low",
  ((nper1000km2 >= 2) & (nper1000km2 < 5))  ~ "Med",
  ((nper1000km2 >= 5) & (nper1000km2 < 20))  ~ "High",
  (nper1000km2 >= 20)  ~ "Extreme"
  )
)

## Reorder values of categories to be listed in order in legend

d$cat = factor(d$cat, c("Extreme", "High", "Med", "Low"))

## Plot texts

title = glue("<b><span style='font-family:fs; color:  white;'  >&#xf6e2;</span> Haunted States in US <span style='font-family:fs; color:  white;'  >&#xf6e2;</span></b>")
subtitle = paste0("In total ", nrow(haunted_places), " haunted places have been observed in US. <br>
                      The most ones are in <br> <span style = 'color:orange;'>District of Columbia</span>, <span style = 'color:orange;'>Rhode Island</span> 
                      and <span style = 'color:orange;'>Massachussets</span>.")
caption = "<b>NOTE: Based on the density of haunted places per 1000 square kms</b><br>Tidy Tuesday, week 41<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2023"

map = plot_usmap(data = d,
                 values = "cat", labels=FALSE) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption
  ) +
  scale_fill_brewer(palette="YlOrRd",direction = -1) +
  theme_void() +
  theme(
    plot.title = element_markdown(family = "title", size = 23, color = "white", hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "title", color = "white", hjust = 0.5, size = 15),
    plot.caption = element_markdown(family = "title", color = "white",
                                    margin = margin(r = 10, b = 7), lineheight = 1, size = 12),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),text = element_text(size = 2),
    legend.text = element_text(family = "title", colour = "white", size = 18),
    legend.direction = "vertical",
    legend.position = "right"
  )

ggsave(
  filename = "2023/w41/w41-2023-tt.png",
  plot = map,
  device = "png",
  height = 4,
  width = 6)

