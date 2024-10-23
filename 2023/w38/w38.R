library(readr)
library(dplyr)
library(stringr)
library(lubridate)

library(ggplot2)
library(ggtext)
library(glue)

library(showtext)
library(sysfonts)

## Load fonts

sysfonts::font_add_google("EB Garamond", "eb")
sysfonts::font_add_google("Pacifico", "pc")
sysfonts::font_add_google("Lilita One", "lo")
sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')


showtext_auto()
showtext::showtext_opts(dpi = 300)

# Import Data

cran_20230905 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-19/cran_20230905.csv')

# Extract first part of character (exclude everything after first space)

cran_20230905$`Date/Publication` = word(cran_20230905$`Date/Publication`, 1)

# Convert char to Date type

cran_20230905$`Date/Publication` = ymd(cran_20230905$`Date/Publication`)


# Extract only Year of Publication for each R package

cran_20230905$`Date/Publication` = year(cran_20230905$`Date/Publication`)

freq = cran_20230905$`Date/Publication` %>%
  table() %>%
  as.data.frame() %>%
  setNames(c("Year", "Packages"))


title = glue("Development of &nbsp;&nbsp; <span style='font-family:fb; color:  #1a8cff;'  >&#xf4f7;</span> &nbsp;&nbsp; Packages")
subtitle = glue("<b>CRAN</b>, the Comprehensive R Archive Network, is the leading repository
                  for enhancing<br> R code with a total of <span style= 'color: red; font-weight:bold; font-family:lo; font-size: 10px'>{length(cran_20230905$Package)}</span> packages. 
                  Established in 2008, CRAN has seen substantial <br>contributions from the R community. 
                  In the last three years, package publications on <br>CRAN have surged by an impressive 200%. 
                  Additionally, alternative repositories for R,<br>such as Bioconductor and R-Forge, are available.")
caption = "<span style= 'color: grey; text-align: left;'><b>NOTE: The number of developed R packages in 2023 refers to data up until September 2023.</span><br>Tidy Tuesday, week 38<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2023"


label = glue("<span  style='font-family:lo; color: red; font-size: 40px; font-weight:bold;'>200%</span> 
             <span style='font-family:fs;font-size: 40px; color: #1a8cff;'>&#xe098;</span><br>
             <span  style='font-family:lo; color: black; font-size: 40px; font-weight:bold;'>last 3 years</span>")

plot = ggplot(data = freq) +
  geom_line(aes(x = Year, y = Packages, group = "none"), linewidth = 1.1) +
  geom_text(aes(x = Year, y = Packages + 480, label = ifelse(Year %in% seq(2008,2023,3), Packages, "")),
            family = "lo", fontface = "bold", size = 4, color = "dodgerblue2") +
  geom_area(aes(x = Year, y = Packages, group = "none"),
            fill = "green", alpha = 0.2) +
  geom_point(aes(x = Year, y = Packages)) +
  geom_richtext(aes(x = "2016", y = 5000, label = label), 
                fill = NA, label.color = NA) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7000))+
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = ""
  ) +
  theme_classic(
    base_size = 12,
    base_family = "eb"
  ) +
  theme(
    text = element_text(family = "eb"),
    plot.title = element_markdown(family = "lo", face = "bold",
                                  margin = margin(t = 2, b = 5),
                                  hjust = 0.5),
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.subtitle = element_markdown(family = "eb", lineheight = 1.2,
                                     margin = margin(r = 10, l = 10, t = 5, b = 5)),
    plot.caption = element_markdown(family = "eb", lineheight = 1.2)
  )



ggsave(
  filename = "2023/w38/w38-2023-tt.png",
  plot = plot,
  device = "png",
  height = 4,
  width = 6
)
