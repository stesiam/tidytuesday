# Import libraries

library(readr)

library(rnaturalearth) # ne_countries
library(rnaturalearthdata) # medium scale option

library(ggplot2)
library(ggtext)
library(glue)


library(osmdata)

#library(extrafont)


library(showtext)
library(sysfonts)

## Load fonts

sysfonts::font_add_google("Amiri", "Amiri")
sysfonts::font_add_google("EB Garamond","eb")
sysfonts::font_add_google("Oswald","caption")
#sysfonts::font_add_google("Gentium Plus", "gp")
#sysfonts::font_add('gp',"/home/stelios/Downloads/Gentium_Plus/GentiumPlus-Regular.ttf")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')


showtext_auto()
showtext::showtext_opts(dpi = 300)


# Import dataset

numbats <- read_csv("2023/w10/numbats.csv")

# Set Plain Australia Map

aus = ne_countries(country = "australia", 
                   type = "countries", 
                   scale = 'large',
                   returnclass = "sf")


# Texts

title = glue("<b>Numbats' Observations</b>")
subtitle = glue("In total ", {nrow(numbats)}, " numbats have been observed in Australia. <br>
                      The vast majority of those are <span style = 'color:red;'>Myrmecobius fasciatus</span>
                      <br> and just 12 are <span style = 'color:dodgerblue;'>Myrmecobius fasciatus rufus</span>.")
caption = glue("<b>SOURCE:</b> Atlas of Living Australia | Tidy Tuesday, week 10<br><b>VISUALIZATION: </b><span style='font-family:fb;'>&#xf09b;</span><b> stesiam</b>, 2023")
  
# Plot numbats observations

map = ggplot() +
  geom_sf(data = aus) +
  coord_sf(xlim = c(110, 152), ylim = c(-10, -43)) +
  geom_point(data = numbats, aes(x = decimalLongitude, y = decimalLatitude, color = scientificName)) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_markdown(family = "Amiri", color = "black"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "eb", color = "black"),
    plot.caption = element_markdown(family = "caption", color = "black",lineheight = 1.2,
                                    margin = margin(b = 5)),
    legend.position = "none"
  )



ggsave(
  filename = "2023/w10/w10-2023-tt.png",
  plot = map,
  device = "png",
  bg = "white",
  height = 4,
  width = 4.5)

