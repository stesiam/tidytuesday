library(readr)
library(dplyr)
library(tidyr)
library(forcats)

library(ggplot2)
library(ggtext)
library(glue)

library(showtext)
library(sysfonts)


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

## Load Data


drwho_episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-28/drwho_episodes.csv')
drwho_directors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-28/drwho_directors.csv')
drwho_writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-28/drwho_writers.csv')

## Combine datasets to one

drwho_episodes = drwho_episodes %>%
  drop_na(season_number)

drwho_episodes$season_number %>% table()

d = drwho_episodes %>%
  group_by(season_number) %>%
  summarise(total_ukviewers = sum(uk_viewers),
            avg_rating = mean(rating),
            episodes_per_season = n(),
            viewers_per_episode = total_ukviewers/episodes_per_season)

## Plot texts

title = glue("<b>Doctor Who Episodes</b>")
subtitle = glue("<b>Doctor Who</b> is a famous TV series whose first season was originally broadcast <br>
                on BBC TV between 1963 and 1964. This visualization is based on revival<br> era (2005 - )
                which constists of 13 seasons. The season with the highest <br> rating is season 4 and the last season is the lowest rated one.")

caption = glue("<b>SOURCE:</b> datardis (R package) | Tidy Tuesday, week 48<br><b>VISUALIZATION: </b><span style='font-family:fb;'>&#xf09b;</span><b> stesiam</b>, 2023")



p1 = ggplot(data = d) +
  geom_point(aes(x = avg_rating, y= viewers_per_episode), color = "grey80") +
  geom_point(data = d %>% filter(avg_rating == max(avg_rating)), aes(x = avg_rating, y= viewers_per_episode), color = "green4") +
  geom_point(data = d %>% filter(avg_rating == min(avg_rating)), aes(x = avg_rating, y= viewers_per_episode), color = "red2") +
  geom_curve(data = d %>% filter(avg_rating == max(avg_rating)), aes(x = avg_rating, y= viewers_per_episode, xend=95, yend=6), arrow = arrow(length=unit(0.2, 'cm')), color = "black") +
  geom_curve(data = d %>% filter(avg_rating == min(avg_rating)), aes(x = avg_rating, y= viewers_per_episode, xend=72, yend=2), arrow = arrow(length=unit(0.2, 'cm')), color = "black") +
  geom_text(aes(x = 95, 7), label = "Season 4", family = "js", size = 5, color = "black") +
  geom_text(aes(x = 72, 1.5), label = "Season 13", family = "js", size = 5, color = "black") +
  scale_y_continuous(limits = c(0, 10), breaks = c(0,2,4,6,8,10)) +
  scale_x_continuous(limits = c(70,100)) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = "Rating",
    y = "Average viewers (millions)")+
  theme_classic(base_size = 12,
                          base_family = "gp") +
  theme(
    plot.title = element_markdown(family = "js",
                                  margin = margin(t = 10, b = 5), 
                                  hjust = 0.5, 
                                  color = "black",face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "gp",
                                     margin = margin(t = 5, l = 10, r = 10, b = 5),
                                     lineheight = 1.1,
                                     color = "black"),
    axis.title.x = element_text(family = "gp", size = 10, margin = margin(b = 5), color ="black"),
    axis.title.y = element_text(family = "gp", size = 10, color = "black"),
    axis.text = element_text(color = "black"),
    plot.background = element_rect(fill = "#7f9fa1", color = "#7f9fa1"),
    panel.background = element_rect(fill = "#7f9fa1"),
    plot.caption = element_markdown(family = "title", margin = margin(t = 5, r = 5, b = 4), 
                                    lineheight = 1.4,
                                    color = "black"),
    plot.margin = margin(l=8, r=8)
  )

ggsave(
  filename = "2023/w48/w48-2023-tt.png",
  plot = p1,
  device = "png",
  height = 4,
  width = 6)

get_png <- function(filename) {
  grid::rasterGrob(webp::read_webp(filename), interpolate = TRUE)
}
l <- get_png("2023/w48/DoctorWhoLogo.webp")
t <- grid::roundrectGrob()
p = p1 +
  annotation_custom(l, xmin = 67.5, xmax = 70, ymin = -5, ymax = -2) +
  coord_cartesian(clip = "off")


ggsave(
  filename = "2023/w48/w48-2023-tt.png",
  plot = p,
  device = "png",
  height = 4,
  width = 6)


# <!--
#  NOTES
# 
# library(rvest)
# 
# t = seq(1, 13, 1) %>% as.character()
# 
# get_wiki_images = function(season){
#   url = "https://en.wikipedia.org/wiki/Doctor_Who_("
#   paste0(url,"series_", season, ")")
# }
# 
# get_wiki_drwho_pages = function(season){
#   url = "https://en.wikipedia.org/wiki/Doctor_Who_(series_"
#   paste0(url, URLencode(as.character(season)), ")")
# }
# 
# 
# get_wiki_images(t)
# 
# get_images_urls = function(season){
#   t3 = character(length(season))  # Initialize t3 to store image URLs
#   
#   for (i in 1:length(season)){
#     url = get_wiki_drwho_pages(season[i])  # Use season[i] instead of just season
#     page = read_html(url)
#     my.table = html_node(page, ".infobox-image")
#     t1 = html_element(my.table, "img")
#     t2 = html_attr(t1, "src")
#     t3[i] = substring(t2, 3)
#   }
#   
#   print(t3)
# }
# 
# i = get_images_urls(1)
# 
# # Read the JPG image
# img <- readJPEG(i)
# 
# # Display the image (optional)
# plot(1:2, type = "n", axes = FALSE, xlab = "", ylab = "")
# rasterImage(img, 1, 1, 2, 2)
# 
# 
# page = read_html(get_wiki_images())
# my.table = html_node(page, ".infobox-image")
# t1 = html_element(my.table, "img")
# t2 = html_attr(t1, "src")
# t3 = substring(t2, 3)
# }
# readJPEG(t3)
# 
# -->