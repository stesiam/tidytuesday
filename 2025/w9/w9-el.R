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
sysfonts::font_add_google("EB Garamond", "eb")


sysfonts::font_add_google("GFS Neohellenic", "gn")
#sysfonts::font_add('gp',"/home/stelios/Downloads/Gentium_Plus/GentiumPlus-Regular.ttf")

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
    sex %in% c("Neutered", "Spayed") ~ "Στειρωμένο",
    sex %in% c("Female", "Male") ~ "Μη στειρωμένο",
    sex %in% c("Unknown") ~ "Άγνωστο"
  )) |>
  mutate(isDomestic = case_when(
    animal_type %in% c("dog", "cat", "rabbit", "guinea pig", "livestock") ~ "Εξημερωμένα",
    animal_type %in% c("amphibian", "bird", "other", "reptile", "wild") ~ "Μη εξημερωμένα"
  )) |>
  mutate(severityStatus = case_when(
    intake_condition %in% c("behavior  mild", "ill mild","injured  mild",
                            "under age/weight","aged", "intakeexam", "normal") ~ "Καλή κατάσταση",
    intake_condition %in% c("behavior  moderate", "ill moderatete", 
                            "injured  moderate", "welfare seizures","behavior  severe", 
                            "ill severe", "injured  severe", "feral", "fractious") ~ "Κακή"
  )) |>
  mutate(intakeType = case_when(
    intake_type %in% c("stray") ~ "αδέσποτο",
    intake_type %in% c("wildlife") ~ "φύση",
    .default = "άλλο"
  )) |>
  mutate(outcome = case_when(
    outcome_is_dead == TRUE ~ "Απεβίωσε",
    outcome_is_dead == FALSE ~ "Διασώθηκε"
  )) |>
  select(animal_id, reproductiveStatus, isDomestic, intakeType, severityStatus, outcome) |>
  mutate(across(c(reproductiveStatus, isDomestic, intakeType, severityStatus, outcome), as.factor)) |>
  mutate(severityStatus = fct_relevel(severityStatus, "Κακή", "Καλή κατάσταση"
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


title_text = glue("<span style='font-family:gn;'>Δεδομένα διάσωσης ζώων</span>")
subtitle_text = marquee::marquee_glue("Το καταφύγιο ζώων της πόλης Long Beach (στην πολιτεία της Καλιφόρνια) έχει φροντίσει πάνω από <b>29,500</b> ζώα,
                      τα τελευταία οκτώ χρόνια (2017 - 2024). Το ποσοστό αυτών που κατάφεραν να <b><span style='color: #377eb8;'>επανέλθουν</span></b> στο φυσιολογικό προσεγγίζει το <b><span style='color: #377eb8;'>80%</span></b> (ενώ το υπόλοιπο <b><span style='color: #E41A1C;'>20% δεν άντεξε</span></b>).
                      Τα μισά από τα ζώα που έρχονται σε κακή κατάσταση, με τη βοήθεια του καταφυγίου, έχουν καταφέρει να επιζήσουν.")
caption_text = " Tidy Tuesday, εβδομάδα 9, 2025<br><b>Πηγή:</b> City of Long Island Animal Rescue & πακέτο {animalshelter}<br><b>Γράφημα:</b> <span style='font-family:fb;'  >&#xf09b;</span><b> stesiam</b>, 2025"

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
    values = c("Διασώθηκε" = pal[2],
               "Απεβίωσε" = pal[1])) +
  theme_void() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_markdown(family = "gn", 
                                  hjust = 0.5, face = "bold", 
                                  color = "white", margin = margin(t = 10, b = 10), size = 18),
    plot.subtitle = element_textbox_simple(family = "gn", color = "white", margin = margin(b = 5, l = 20, r = 20)),
    plot.title.position = "plot",
    plot.caption = element_markdown(family = "eb", lineheight = 1.4, colour = "gray", hjust = 0.5,
                                    margin = margin(b = 5)),
    plot.background = element_rect(fill = "black"),
    legend.position = "none"
  )


ggsave(
  filename = "2025/w9/w9-2025-tt-el.png",
  plot = final_plot,
  device = "png",
  height = 4,
  width = 6)

