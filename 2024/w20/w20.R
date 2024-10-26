# Import libraries

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(glue)
library(ggtext)
library(showtext)
library(sysfonts)
library(ggimage)
library(here)

library(fontawesome)
library(emojifont)

library(camcorder)

# gg_record(
#   dir = file.path(here::here("2024/w20"), "recording100"), # where to save the recording
#   device = "png", # device to use to save images
#   width = 6,      # width of saved image
#   height = 4,     # height of saved image
#   units = "in",   # units for width and height
#   dpi = 300       # dpi to use when saving image
# )

# Import fonts
sysfonts::font_add_google("Leckerli One", "title")
sysfonts::font_add_google("Outfit", "subtitle")
sysfonts::font_add_google("Ubuntu Condensed", "uc")
sysfonts::font_add_google("Jost", "jost")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')


showtext_auto()
showtext::showtext_opts(dpi = 300)

here::here("2024/w20")

# Import dataset

coffee_survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv')


dataSeparated = coffee_survey |>
  select(submission_id, why_drink) |>
  drop_na() |>
  separate_rows(why_drink, sep = ",\\s*")

total_respondents <- n_distinct(dataSeparated$submission_id)

pctWhyDrinkCoffee = dataSeparated %>%
  count(why_drink) |>
  mutate(percentage = n / total_respondents * 100) |>
  dplyr::filter(why_drink != "Other")

pctWhyDrinkCoffee[1,1] = "Need caffeine"
pctWhyDrinkCoffee[2,1] = "As A Ritual"
pctWhyDrinkCoffee[3,1] = "Go to Bathroom"
pctWhyDrinkCoffee[4,1] = "Tastes Good"

pctWhyDrinkCoffee$icons = c(fa("coffee"), fa("calendar"), fa("warning"), fa("thumbs-up"))
pctWhyDrinkCoffee$font  = fontawesome(c("fa-coffee", "fa-calendar", "fa-warning", "fa-thumbs-up"))
## Viz texts
pctWhyDrinkCoffee$col = c("brown", "grey90", "orange2", "yellow1")

title_text = glue("Why do you drink <span style='color:brown;' >coffee</span> ?")
subtitle_text = glue("As someone who does not enjoy coffee, I have always been wondering why <br> people like it
                     I tasted coffee once, and it was awful. However, I should <br> reconsider as almost
                      everyone refers to its taste.")
caption_text = "<b>Data:</b> Great American Coffee Taste Test Survey<br>Tidy Tuesday, Week 20 (2024)<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2024"

pctWhyDrinkCoffee$icontest = c(
                               "https://www.svgrepo.com/show/211920/coffee-grain-seed.svg",
                               "https://www.svgrepo.com/show/411277/schedule.svg",
                               "https://www.svgrepo.com/show/503650/toilet-paper.svg",
                               "https://www.svgrepo.com/show/401471/face-savoring-food.svg")

plot = ggplot(data = pctWhyDrinkCoffee) +
  geom_richtext(aes(x = 0, y = 0.08, label = glue("<b><span style ='color: red; family=title; font-size:31pt;'>{round(percentage, digits = 0)}</span></b>%")), 
                family = "jost", size = 5, fill = NA, label.color = NA,
                color = "white") +
  geom_image(aes(x = 0, y = 0.22, image = icontest), size = 0.3) +
  scale_y_continuous(limits = c(0, 0.3)) +
  facet_wrap(~factor(why_drink, levels = c('Tastes Good', 'Need caffeine',
                                           'As A Ritual', 'Go to Bathroom')),nrow = 1) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  theme_minimal(base_size = 15) +
  theme(
        plot.title = element_markdown(family = "title", 
                                      hjust = 0.5, 
                                      face = "bold",
                                      margin = margin(t = 5, b = 10),
                                      color = "white"),
        plot.subtitle = element_markdown(family = "subtitle", 
                                         size = 12.5,
                                      margin = margin(t = 5, b = 20),
                                      color = "white",
                                      lineheight = 1.2),
        plot.caption = element_markdown(family = "jost", lineheight = 1.2, size = 10, color = "white"),
        axis.text = element_blank(),
        axis.title = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "black"),
        strip.text = element_markdown(family = "jost", size = 13,
                                      face = "bold",
                                      color = "white"))


ggsave(
  filename = "2024/w20/w20-2024-tt.png",
  plot = plot,
  device = "png",
  height = 4,
  width = 6)


# 
# gg_playback(name = list.files("2024/w20","recording100", pattern = "*.png"),
#   first_image_duration = 5,
#   last_image_duration = 15,
#   frame_duration = .4,width = 1920, height = 1020, loop = F
# )
# 
# gifski::gifski(png_files = list.files(here::here("2024/w20/recording100/", ""), 
#                                       full.names = T), 
#                gif_file = here::here("2024/w20/tt2024w20v3.gif"), 
#                delay = 0.55,loop = FALSE,width = 1920, height = 1080)
# av::av_encode_video(list.files('2024/w20/recording100/', '*.png'),
#                     output = '2024/w20/test.mp4',framerate = 0.55,verbose = T
#                       )
# 
# library(av)
