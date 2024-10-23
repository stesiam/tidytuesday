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


title = glue("Αριθμός νέων πακέτων της &nbsp;&nbsp; <span style='font-family:fb; color:  #1a8cff;'  >&#xf4f7;</span> ανά έτος")
subtitle = glue("Το <b>CRAN</b>, the Comprehensive R Archive Network, είναι από τα πιο σημαντικά 
                  αποθετήρια για πακέτα της R. Συνολικά αριθμεί <span style= 'color: red; font-weight:bold; font-family:lo; font-size: 10px'>{length(cran_20230905$Package)}</span> πακέτα. 
                Από το 2008 που δημιοργήθηκε, το CRAN έχει σμηνατικές συνεισφορές στην R. Τα τελευταία τρία χρόνια, οι αναρτήσεις πακέτων στο CRAN έχουν αυξηθεί κατά εντυπωσιακό ποσοστό (200%). Επιπροσθέτως, υπάρχουν και άλλα αποθετήρια για την R, όπως το Bioconductor και το R-Forge.")
caption = "<span style= 'color: grey; text-align: left;'><b>ΣΗΜΕΙΩΣΗ: Ο αριθμός των πακέτων το 2023, αναφέρονται μέχρι τον Σεπτέμβριο του 2023</span><br>Tidy Tuesday, week 38<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2023"


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
    plot.title = element_markdown(family = "serif", face = "bold",
                                  margin = margin(t = 2, b = 5),
                                  hjust = 0.5),
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.subtitle = element_textbox_simple(family = "eb", lineheight = 1.2,
                                     margin = margin(r = 10, l = 10, t = 5, b = 5), size = 10),
    plot.caption = element_markdown(family = "eb", lineheight = 1.2)
  )



ggsave(
  filename = "2023/w38/w38-2023-tt-el.png",
  plot = plot,
  device = "png",
  height = 4,
  width = 6
)

