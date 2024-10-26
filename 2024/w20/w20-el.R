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

#library(camcorder)

# gg_record(
#   dir = file.path(here::here("2024/w20"), "recording100"), # where to save the recording
#   device = "png", # device to use to save images
#   width = 6,      # width of saved image
#   height = 4,     # height of saved image
#   units = "in",   # units for width and height
#   dpi = 300       # dpi to use when saving image
# )



# Import fonts
sysfonts::font_add_google("Outfit", "subtitle")
sysfonts::font_add_google("Ubuntu Condensed", "uc")
sysfonts::font_add_google("Jost", "jost")
sysfonts::font_add(family = "title", regular = "../../Downloads/Mynerve-Regular.ttf")
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

pctWhyDrinkCoffee[1,1] = "Θέλω καφεϊνη"
pctWhyDrinkCoffee[2,1] = "Συνήθεια"
pctWhyDrinkCoffee[3,1] = "Πάω στη τουαλέτα"
pctWhyDrinkCoffee[4,1] = "Έχει ωραία γεύση"

pctWhyDrinkCoffee$icons = c(fa("coffee"), fa("calendar"), fa("warning"), fa("thumbs-up"))
pctWhyDrinkCoffee$font  = fontawesome(c("fa-coffee", "fa-calendar", "fa-warning", "fa-thumbs-up"))
## Viz texts
pctWhyDrinkCoffee$col = c("brown", "grey90", "orange2", "yellow1")

title_text = glue("Πίνω **<span style='color:brown;' >καφέ</span>** γιατί / για να...")
subtitle_text = glue("Το 2023 διεξήχθη μία έρευνα μεταξύ 4000 ατόμων σχετικά με τα είδη των καφέδων και αξιολόγηση αυτών.
                      Όντας κάποιος που δεν πίνει καφέ, πάντα αναρωτιόμουν για ποιο λόγο πίνουν οι υπόλοιποι καφέ.
                      Έχω δοκιμάσει καφέ, από περιέργεια, μία φορά και το βρήκα απαίσιο. Ωστόσο, με βάση την έρευνα, 
                      το 94% των συμμετεχόντων ανέφεραν την γεύση ως λόγο κατανάλωσης. Μάλλον πρέπει να ξαναδοκιμάσω κάποια στιγμή.")
caption_text = "Tidy Tuesday, Week 20 (2024)<br><b>Δεδομένα: </b>Great American Coffee Taste Test Survey<br><b>Γράφημα: </b><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2024"

pctWhyDrinkCoffee$icontest = c(
  "https://www.svgrepo.com/show/211920/coffee-grain-seed.svg",
  "https://www.svgrepo.com/show/411277/schedule.svg",
  "https://www.svgrepo.com/show/503650/toilet-paper.svg",
  "https://www.svgrepo.com/show/401471/face-savoring-food.svg")

plot = ggplot(data = pctWhyDrinkCoffee) +
  geom_richtext(aes(x = 0, y = 0.08, label = glue("<b><span style ='color: red; family=title; font-size:31pt;'>{round(percentage, digits = 0)}</span></b>%")), 
                family = "serif", size = 5, fill = NA, label.color = NA,
                color = "white") +
  geom_image(aes(x = 0, y = 0.22, image = icontest), size = 0.3) +
  scale_y_continuous(limits = c(0, 0.3)) +
  facet_wrap(~factor(why_drink, levels = c('Έχει ωραία γεύση', 'Θέλω καφεϊνη',
                                           'Συνήθεια', 'Πάω στη τουαλέτα')),nrow = 1) +
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
                                  margin = margin(t = 5, b = 5),
                                  color = "white"),
    plot.subtitle = element_textbox_simple(family = "serif", 
                                     size = 11.5,
                                     margin = margin(t = 5, b = 20),
                                     color = "white",
                                     lineheight = 1.2),
    plot.caption = element_markdown(family = "serif", lineheight = 1.2, size = 10, color = "white"),
    axis.text = element_blank(),
    axis.title = element_blank(),panel.grid = element_blank(),
    plot.background = element_rect(fill = "black"),
    strip.text = element_markdown(family = "serif", size = 12,
                                  face = "bold",
                                  color = "white"))


ggsave(
  filename = "2024/w20/w20-2024-tt-el.png",
  plot = plot,
  device = "png",
  height = 4,
  width = 6)

