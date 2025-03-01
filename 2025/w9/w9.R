# Import libraries

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce)
library(ggtext)
library(glue)
library(forcats)
library(stringr)

library(showtext)
library(sysfonts)

## Load fonts

sysfonts::font_add_google("Oswald", "title")
sysfonts::font_add_google("Josefin Slab","js")
sysfonts::font_add_google("Cabin Condensed","cc")
sysfonts::font_add_google("Ubuntu Condensed", "uc")


#sysfonts::font_add_google("Gentium Plus", "gp")
sysfonts::font_add('gp',"/home/stelios/Downloads/Gentium_Plus/GentiumPlus-Regular.ttf")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.7.2-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.7.2-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()
showtext::showtext_opts(dpi = 300)




longbeach <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-04/longbeach.csv')


# Some species of birds could be considered domestic ones.
# On this occassion we see that the vast majority comes
# from wildlife or stray so I will classify them as
# non-domestic

data = longbeach |>
  dplyr::filter(intake_condition != "i/i report") |>
  mutate(reproductiveStatus = case_when(
    sex %in% c("Neutered", "Spayed") ~ "Neutered",
    sex %in% c("Female", "Male") ~ "Non-neutered",
    sex %in% c("Unknown") ~ "Unknown"
  )) |>
  mutate(isDomestic = case_when(
    animal_type %in% c("dog", "cat", "rabbit", "guinea pig", "livestock") ~ "Domestic",
    animal_type %in% c("amphibian", "bird", "other", "reptile", "wild") ~ "Non Domestic"
  )) |>
  mutate(severityStatus = case_when(
    intake_condition %in% c("behavior  mild", "ill mild","injured  mild",
                            "under age/weight","aged", "intakeexam", "normal") ~ "Low severity",
    intake_condition %in% c("behavior  moderate", "ill moderatete", 
                            "injured  moderate", "welfare seizures","behavior  severe", 
                            "ill severe", "injured  severe", "feral", "fractious") ~ "High severity"
  )) |>
  mutate(intakeType = case_when(
    intake_type %in% c("stray") ~ "stray",
    intake_type %in% c("wildlife") ~ "wildlife",
    .default = "other"
  )) |>
  mutate(outcome = case_when(
    outcome_is_dead == TRUE ~ "Deceased",
    outcome_is_dead == FALSE ~ "Alive"
  )) |>
  select(animal_id, reproductiveStatus, isDomestic, intakeType, severityStatus, outcome) |>
  mutate(across(c(reproductiveStatus, isDomestic, intakeType, severityStatus, outcome), as.factor)) |>
  mutate(severityStatus = fct_relevel(severityStatus, "High severity", "Low severity"
  ))

data_summary <- data %>%
  group_by(reproductiveStatus, isDomestic, intakeType, severityStatus, outcome) %>%
  summarise(count = n(), .groups = "drop")

data_summary <- data_summary %>%
  mutate(across(c(reproductiveStatus, isDomestic, intakeType, severityStatus, outcome), as.factor))

##
pal <- RColorBrewer::brewer.pal(3, "Set1")
fg = gather_set_data(data_summary, x = 1:5, id = "id")
fg = mutate(fg, x = fct_inorder(as.factor(x)))

fg <- fg %>%
  mutate(y = str_wrap(y, width = 10)) 


title_text = glue("<span style='font-family:js;'>Data on Rescuing Animals</span>")
subtitle_text = marquee::marquee_glue("The animal shelter of City of Long Island has cared approximately, <b>29,500</b> animals,
                      the last 8 years (2017-2024). The percentage of <b><span style='color: #377eb8;'>recovered</span></b> animals is close to <b><span style='color: #377eb8;'>80%</span></b>.
                      Half of the animals that come to the shelter in bad condition have been saved.")
caption_text = " Tidy Tuesday, week 9, 2025<br><b>SOURCE:</b> City of Long Island Animal Rescue<br><b>VISUALIZATION:</b> <span style='font-family:fb;'  >&#xf09b;</span><b> stesiam</b>, 2025"

# bg_gradient <- grid::linearGradient(colours = rev(MetBrewer::met.brewer("")[6:8]))

final_plot = ggplot(fg, aes(x=x, id = id,
               split = y,
               value = count)) +
  geom_parallel_sets(aes(fill = outcome),
                     alpha = 0.5,
                     axis.width = 0.2) +
  geom_parallel_sets_axes(axis.width = 0.3, fill = "gray5") +
  geom_parallel_sets_labels(
    aes(label = y),
    colour = 'grey', 
    size = 3,
    family = "uc") +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  scale_fill_manual(
    values = c("Alive" = pal[2],
               "Deceased" = pal[1])) +
  theme_void() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_markdown(family = "js", 
                                  hjust = 0.5, face = "bold", 
                                  color = "white", margin = margin(t = 10, b = 10), size = 18),
    plot.subtitle = element_textbox_simple(family = "js", color = "white", margin = margin(b = 5, l = 20, r = 20)),
    plot.title.position = "plot",
    plot.caption = element_markdown(family = "js", lineheight = 1.4, colour = "gray", hjust = 0.5),
    plot.background = element_rect(fill = "black"),
    legend.position = "none"
  )


ggsave(
  filename = "2025/w9/w9-2025-tt.png",
  plot = final_plot,
  device = "png",
  height = 4,
  width = 6)



