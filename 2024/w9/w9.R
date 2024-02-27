library(readr)
library(dplyr)


library(ggplot2)
library(ggforce)
library(ggtext)
library(glue)

library(rvest)
library(stringr)

library(showtext)
library(sysfonts)

## Load fonts

sysfonts::font_add_google("Oswald", "title")
sysfonts::font_add_google("Josefin Slab","js")
sysfonts::font_add_google("Cabin Condensed","cc")
sysfonts::font_add_google("Ubuntu Condensed", "uc")
sysfonts::font_add_google("Atomic Age","ac")

#sysfonts::font_add_google("Gentium Plus", "gp")
sysfonts::font_add('gp',"/home/stelios/Downloads/Gentium_Plus/GentiumPlus-Regular.ttf")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')


showtext_auto()
showtext::showtext_opts(dpi = 300)


## Import datasets

events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/events.csv')
births <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/births.csv')
deaths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/deaths.csv')



# Scraping nationalities

url = "https://www.englishclub.com/vocabulary/world-countries-nationality.php"

nationality_table = read_html(url) %>%
  html_element(".ec-table") %>%
  html_table()

nationality_table_clean = nationality_table %>%
  select(c(1,2)) %>%
  setNames(c("country", "nationality")) %>%
  mutate(
    country = str_remove_all(country, "\\(*"),
    nationality = str_remove_all(nationality, "\\(.*"),
    nationality = str_trim(nationality),
    nationality = ifelse(nationality == "US", "American",nationality)
  )



title = glue("<b>Footballers born on 29th February</b>")

subtitle = glue("On this week's dataset I decided to study a small subdataset concerning 
                the births<br> of infuential people. More specifically, I subtracked each footballers'
                nationality <br> and year of birth. There are not many born in a leap day but still a cool graph.")

caption = glue("<b>SOURCE:</b> Wikipedia | Tidy Tuesday, week 9 (2024)<br><b>VISUALIZATION: </b><span style='font-family:fb;'>&#xf09b;</span><b> stesiam</b>, 2024")
  


births$log = str_detect(births$description, "footballer") == TRUE

bbb = births %>%
  dplyr::filter(log == TRUE) %>%
  mutate(
    nationality = word(description,1)
  )

vvv = left_join(bbb, nationality_table_clean, by = "nationality",keep = T) %>%
  mutate(
    iso2c = countrycode::countryname(country, destination = "iso2c"),
    iso2c = tolower(iso2c)
  ) %>%
  dplyr::filter(country != "Australia")

vvv$cat = c(rep(7,1))

description_text = glue("Born in {year_birth} <br> {country}")

b = ggplot(vvv, aes(x = year_birth, y = cat, country= iso2c, group = person)) +
  geom_hline(aes(yintercept = cat)) +
  ggflags::geom_flag(size = 10) +
  xlim(c(1945, 2010)) +
  ggforce::geom_mark_rect(aes(label = person, 
                              description = paste0("Born in ", year_birth)),
                          label.fontsize = 8,color = "transparent",
                          label.family ="serif",
                          label.hjust = 0.5,
                          label.buffer = unit(1, "cm"),
                          con.size = unit(1,"cm"),con.type = "straight",con.border = "one") +
  #geom_richtext(aes(label = Obs), color = "white", family = "gpb", size = 2, 
  #              fill = NA, label.color = NA, fontface = "bold" ) +
  # geom_vline(xintercept = 1600, color = "grey50", linetype = "longdash") +
  # geom_vline(xintercept = 1700, color = "grey50", linetype = "longdash") +
  # geom_vline(xintercept = 1500, color = "grey50", linetype = "longdash") +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    y = ""
  ) +
 # ggrepel::geom_label_repel(aes(label = person), size = 1,segment.curvature = 0.2) +
 # geom_text(aes(label = person), size = 2) +
  theme_minimal(base_size = 11.5,
                base_family = "uc") +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "ac"),
        panel.grid = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "#ecd495", color = "#ecd495"),
        plot.background = element_rect(fill = "#ecd495", color = "#ecd495"),
        plot.title = element_markdown(hjust = 0.5, margin = margin(t = 10),
                                      family = "title"),
        plot.caption = element_markdown(family = "title",
                                        lineheight = 2, size = 6, hjust = 0.5,
                                        margin = margin(t = 5, b = 3)),
        plot.caption.position = "plot",
        plot.subtitle = element_markdown(family = "gp",
                                         margin = margin(t = 5, l = 7, r = 7),
                                         lineheight = 1.2),
        plot.title.position = "plot",
        plot.margin = margin(l=10, r=10)
        
  )


ggsave(
  filename = "2024/w9/w9-2024-tt.png",
  plot = b,
  device = "png",
  height = 4,
  width = 6)
