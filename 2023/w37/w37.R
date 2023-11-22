library(readr)
library(dplyr)
library(stringr)
library(lubridate)

library(ggplot2)
library(ggtext)
library(glue)

library(showtext)
library(sysfonts)


library(rnaturalearth)
library(rnaturalearthdata)

## Load fonts

sysfonts::font_add_google("EB Garamond", "eb")
sysfonts::font_add_google("Pacifico", "pc")
sysfonts::font_add_google("Lilita One", "lo")
sysfonts::font_add_google("Oswald", "caption")
sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')


showtext_auto()
showtext::showtext_opts(dpi = 300)

# Import Data

all_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/all_countries.csv')
country_regions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/country_regions.csv')
global_human_day <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/global_human_day.csv')
global_economic_activity <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/global_economic_activity.csv')


## Flter Greece's data



title = glue("<span style='font-family:fs; color: #c19a68;'>&#xf236;</span> <span style='font-family:lo; color: black;'> Sleep </span> <span style='font-family:lo; color: red;'>Uncertainty </span> </span> <span style='font-family:fs; color: #c19a68;'  >&#xf236;</span>")
subtitle = glue("Sleep is one of the most important parts of daytime as it consists one third <br>
                 of it. The less uncertainty in <b>sleep time</b> is in <span style='font-family:lo; color: #009933;'>North America</span>, <span style='font-family:lo; color: #009933;'>EU</span>,  <span style='font-family:lo; color: #009933;'>India</span>, <br>
<span style='font-family:lo; color: #009933;'>Australia</span> and  <span style='font-family:lo; color: #009933;'>New Zealand</span>. 
                 On the other hand, the less safe are <br><span style='font-family:lo; color: red;'>South East Asia</span> and 
                 <span style='font-family:lo; color: red;'>Oceania</span> states <br> .")
caption = "<b>SOURCE:</b> The Human Chronome Project<br>Tidy Tuesday, week 37<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2023"

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica")
class(world)

x = all_countries %>%
  select(Subcategory, country_iso3, uncertaintyCombined) %>%
  filter(Subcategory == "Sleep & bedrest") %>%
  rename(
    adm0_a3 = "country_iso3"
  )

c = left_join(world, x, by = "adm0_a3")


plot = ggplot(data = c) +
  geom_sf(aes(fill = uncertaintyCombined)) +
  scale_fill_gradient(low = "white", high = "red2") +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption
  ) +
  theme_void(
    base_size = 14,
    base_family = "eb"
  ) + 
  theme(
    legend.position = "none",
    plot.title = element_markdown(family = "lo", hjust = 0.5, margin = margin(t =5, b = 5)),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "eb", margin = margin(r = 5, l = 5)),
    plot.caption = element_markdown(family = "caption", margin = margin(b = 5), lineheight = 1.2, size = 7),
    plot.background = element_rect(fill = "#e6ffff", colour = "#e6ffff"),
    panel.background = element_rect(fill = "#e6ffff", colour = "#e6ffff")
  )
  

ggsave(
  filename = "2023/w37/w37-2023-tt.png",
  plot = plot,
  device = "png",
  height = 4,
  width = 6
)

