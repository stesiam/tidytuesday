library(readr)
library(dplyr)
library(tidyr)


library(ggplot2)
library(ggtext)
library(glue)

library(showtext)
library(sysfonts)


library(rnaturalearth)
library(rnaturalearthdata)

## Load fonts

sysfonts::font_add_google("Oswald", "title")
sysfonts::font_add_google("Josefin Slab","js")
sysfonts::font_add_google("Cabin Condensed","cc")
sysfonts::font_add_google("Ubuntu Condensed", "uc")


#sysfonts::font_add_google("Gentium Plus", "gp")
sysfonts::font_add('gp',"/home/stelios/Downloads/Gentium_Plus/GentiumPlus-Regular.ttf")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()
showtext::showtext_opts(dpi = 300)

# Import data

life_expectancy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-05/life_expectancy.csv')
#life_expectancy_different_ages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-05/life_expectancy_different_ages.csv')
#life_expectancy_female_male <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-05/life_expectancy_female_male.csv')

life_expectancy = life_expectancy %>%
  filter(Year == 1950 | Year == 1985 | Year == 2020) %>%
  filter(Code %in% c("SWE","NOR","FIN", "DNK", "ISL", "OWID_WRL"))


x = pivot_wider(data = life_expectancy, names_from = Year, values_from = LifeExpectancy)


## Plot texts

title = glue("<span style='font-family:title; color: black;'> Life Expectancy in Scandinavia States</span>")
subtitle = glue("Last 70 years, life expectancy has been increased in an unrepresented way.<br>
                Scandinavia states raised their life expectancy by 10 years. At the same <br>
                period, World's population life expectancy at birth raised by 26 years.
                ")
caption = "<b>NOTE:</b> This plot is referred to expected years of life at birth<br><b>SOURCE:</b> Our World in Data | Tidy Tuesday, week 49<br><b>VISUALIZATION:</b> <span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2023"

plot = ggplot(x, aes(y = reorder(Entity, -`1950`))) +
  geom_point(aes(x = `1950`),
                 color = "grey60",
                 size = 3, 
                 shape = 16) +
  geom_point(aes(x = `1985`),
             color = "orange",
             size = 3, shape = 16) +
  geom_point(aes(x = `2020`),
                 color = "black",
                 size = 3, 
                 shape = 16) +
  geom_segment(aes(x = `1950`,
                   y = Entity,
                   xend = `2020`,
                   yend = Entity),
               size = 1) +
  geom_text(aes(x = `1950`, y = Entity, label = round(`1950`, digits = 1)), nudge_y = 0.37, color = "grey60",
            fontface = "bold", family = "cc") +
  geom_text(aes(x = `2020`, y = Entity, label = round(`2020`, digits = 1)), nudge_y = 0.37, color = "black",
            fontface = "bold", family = "cc") +
  geom_text(aes(x = `1985`, y = Entity, label = round(`1985`, digits = 1)), nudge_y = 0.37, color = "orange",
            fontface = "bold", family = "cc") +
  ## Add custom points and text to make a unique legend
  geom_point(x = 52, y = "Denmark", colour = "grey80") +
  geom_text(x = 54, y = "Denmark", label = "1950",nudge_x = 1) +
  geom_point(x = 52, y = "Sweden", colour = "orange") +
  geom_text(x = 54, y = "Sweden", label = "1985", nudge_x = 1) +
  geom_point(x = 52, y = "Iceland", colour = "black") +
  geom_text(x = 54, y = "Iceland", label = "2020", nudge_x = 1) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = "Years",
    y = "") +
  theme_classic(base_size = 13, 
              base_family = "uc") +
  theme(
    panel.grid = element_blank(),
    plot.title = element_markdown(family = "js",
                                  margin = margin(t = 10, b = 5), 
                                  hjust = 0.5, 
                                  color = "black",face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "gp",
                                     margin = margin(t = 5, l = 10, r = 10, b = 5),
                                     lineheight = 1.1,
                                     color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(color = "black"),
    plot.background = element_rect(fill = "#eed9c4", color = "#eed9c4"),
    panel.background = element_rect(fill = "#eed9c4", color = "#eed9c4"),
    plot.caption = element_markdown(family = "title", margin = margin(t = 2, r = 5, b = 4), 
                                    lineheight = 1.4,
                                    color = "black",
                                    size = 7),
    plot.margin = margin(l=8, r=8),
    panel.border = element_blank()
  )

# Export viz

ggsave(
  filename = "2023/w49/w49-2023-tt.png",
  plot = plot,
  device = "png",
  height = 4,
  width = 6,
)

