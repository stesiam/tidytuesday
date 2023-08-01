# Import libraries

library(rnaturalearth) # ne_countries
library(rnaturalearthdata) # medium scale option
library(ggplot2)
library(ggtext)

library(osmdata)

library(extrafont)
library(sysfonts)

library(readr)

# Import dataset

numbats <- read_csv("2023/w10/numbats.csv")

# Set Plain Australia Map

aus = ne_countries(country = "australia", 
                   type = "countries", 
                   scale = 'large',
                   returnclass = "sf")

# Plot numbats observations

map = ggplot() +
  geom_sf(data = aus) +
  coord_sf(xlim = c(113, 155), ylim = c(-10, -46)) +
  geom_point(data = numbats, aes(x = decimalLongitude, y = decimalLatitude, color = scientificName)) +
  labs(
    title = "<b>Numbats Observations</b>",
    subtitle = paste0("In total ", nrow(numbats), " numbats have been observed in Australia. <br>
                      The vast majority of those are <span style = 'color:red;'>Myrmecobius fasciatus</span>
                      <br> and just 12 are <span style = 'color:dodgerblue;'>Myrmecobius fasciatus rufus</span>."),
    caption = "stesiam, 2023"
  ) +
  theme_void() +
  theme(
    plot.title = element_markdown(family = "Amiri", size = 15),
    plot.subtitle = element_markdown(family = "EB Garamond"),
    plot.caption = element_markdown(family = "EB Garamond"),
    legend.position = "none"
  )

ggsave(
  filename = "2023/w10/w10-2023-tt.png",
  plot = map,
  device = "png",
  bg = "white",
  height = 4,
  width = 4.5)
