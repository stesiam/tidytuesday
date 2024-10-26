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

sysfonts::font_add_google("Alegreya SC", "title")
sysfonts::font_add_google("Comfortaa", "subtitle")
sysfonts::font_add_google("Ubuntu Condensed", "caption")
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
    nper1000km2 < 2 ~ "Χαμηλή",
    ((nper1000km2 >= 2) & (nper1000km2 < 5))  ~ "Μέτρια",
    ((nper1000km2 >= 5) & (nper1000km2 < 20))  ~ "Υψηλή",
    (nper1000km2 >= 20)  ~ "Πολύ Υψηλή"
  )
  )

## Reorder values of categories to be listed in order in legend

d$cat = factor(d$cat, c("Πολύ Υψηλή", "Υψηλή", "Μέτρια", "Χαμηλή"))

## Plot texts

title = glue("<b><span style='font-family:fs; color:  white;'  >&#xf6e2; </span> Στοιχιωμένες Πολιτείες των ΗΠΑ<span style='font-family:fs; color:  white;'  > &#xf6e2;</span></b>")
subtitle = glue("Συνολικά, στις ΗΠΑ έχουν αναφερθεί <b>{nrow(haunted_places)}</b> στοιχειωμένα μέρη <br>
                      Μεγάλυτερη συγκέντρωση αυτών παρατηρείται <br> στη πρωτεύουσα των ΗΠΑ, **<span style = 'color:red;'>Washington DC</span>** και στις πολιτείες <br>**<span style = 'color:orange;'>Rhode Island</span>**
                      και της **<span style = 'color:orange;'>Μασαχουσέτης</span>**.")
caption = "<b>Σημείωση: Με βάση τη πυκνότητα των στοιχειωμένων τοποθεσιών ανά 1000 τετραγωνικά χλμ</b><br>Tidy Tuesday, week 41<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2023"

map = plot_usmap(data = d,
                 values = "cat", labels=FALSE) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    color = "Συγκέντρωση:",
    fill = "Συγκέντρωση:"
  ) +
  scale_fill_brewer(palette="YlOrRd",direction = -1) +
  theme_void() +
  theme(
    plot.title = element_markdown(family = "title", color = "white", hjust = 0.5, face= "bold",
                                  size = 16),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "subtitle", color = "white", hjust = 0.5, size = 9),
    plot.caption = element_markdown(family = "caption", color = "white", size = 7,
                                    margin = margin(r = 10, b = 7), lineheight = 1.2),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    legend.text = element_text(family = "title", colour = "white", size = 10),
    legend.direction = "vertical",
    legend.position = "right", legend.title = element_text(color = "white",
                                                           family = "title", face = "bold")
  )

ggsave(
  filename = "2023/w41/w41-2023-tt-el.png",
  plot = map,
  device = "png",
  height = 4,
  width = 6)

