library(readr)
library(dplyr)
library(tidyr)
library(stringr)

library(glue)

library(ggplot2)
library(ggtext)
library(showtext)
library(sysfonts)
library(countrycode)
library(MetBrewer)

## Load fonts

sysfonts::font_add_google("Amiri", "Amiri")
sysfonts::font_add_google("Jost","jost")
sysfonts::font_add_google("Oswald","caption")
sysfonts::font_add_google("Ubuntu Condensed","uc")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')


showtext_auto()
showtext::showtext_opts(dpi = 300)



winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/winners.csv')
london_marathon <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/london_marathon.csv')

total_participants = london_marathon %>%
  select(Starters) %>%
  drop_na() %>%
  sum(.)
  
total_finishers = london_marathon %>%
  select(Finishers) %>%
  drop_na() %>%
  sum(.)


title = glue("<b>London Marathon Winners' Nationality</b>")
subtitle = glue("London Marathon is an annual marathon held in London.
                The first one held in 1981. In total, London Marathon participants 
                over the years (until 2020) counts to <span style = 'font-weight: bold;'>{round(total_participants/10^6, digits = 2)} </span>millions 
                with <span style = 'color:red; font-family: uc; font-weight: bold;'> {paste0(round(total_finishers/total_participants*100, digits =1),'%')} </span> of them finishing the race.
                In <b> Elite</b> events Kenyan athletes are dominating. 
                The UK athletes have analogous success
                on <b>Wheelchair (T53/T54)</b> events.")
caption = "Tidy Tuesday, week <b>17</b><br><b>Source:</b> LondonMarathon (R package)<br><b>Graphic:</b> stesiam, 2023"


wins_by_country_category <- winners %>%
  group_by(Nationality, Category) %>%
  summarise(n = n(), .groups = 'drop') %>%           # Count the number of wins
  arrange(Category, -n) %>%                          # Arrange by category and descending win count
  group_by(Category) %>%
  slice_max(order_by = n, n = 3, with_ties = TRUE) %>% # Select top 3 per category including ties
  ungroup() %>%
  mutate(
    iso2c = tolower(countrycode(Nationality, "country.name.en", "iso2c")) # Convert country names to lowercase ISO codes
  ) %>%
  group_by(Category) %>%
  mutate(
    Rank = dense_rank(-n),                     # Rank within each category
    medal = case_when(                         # Assign medals based on rank
      Rank == 1 ~ "Gold",
      Rank == 2 ~ "Silver",
      Rank == 3 ~ "Bronze",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(medal)) %>%
  mutate(
    rank_as_we_see_it = case_when(                         # Assign medals based on rank
      medal == "Gold" ~ 2,
      medal == "Silver" ~ 1,
      medal == "Bronze" ~ 3,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(!is.na(rank_as_we_see_it)) %>%
  ungroup()
  
bg_gradient <- grid::linearGradient(colours = rev(MetBrewer::met.brewer("Pillement")[5:6]))

plot = ggplot(data = wins_by_country_category) +
  geom_col(aes(y = n, x = reorder(Nationality, rank_as_we_see_it), fill = medal, group = medal)) +
  ggflags::geom_flag(aes(y = -4, x = Nationality,
                                  country = iso2c), 
                     size = 6) + 
  geom_richtext(aes(y = n, x = Nationality, label = n),
            size = 4,
            family = "uc",
            color = "black",
            nudge_y = -0.5) +
  facet_wrap(~factor(Category, c("Men", 
                                 "Wheelchair Men", 
                                 "Women", 
                                 "Wheelchair Women")), 
             scales = "free") +
  scale_fill_manual(values = c("Gold" = "gold", "Silver" = "#C0C0C0", "Bronze" = "#CD7F32")) +
  scale_y_continuous(expand = c(0,0), limits = c(-4, 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = "",
    y = ""
  ) +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_markdown(family = "jost", margin = margin(t = 5), color = "white", hjust = 0.5),
    plot.subtitle = element_textbox_simple(family = "jost", margin = margin(t = 5, b = 10),
                                           color = "white"),
    plot.caption = element_markdown(family = "caption", color = "white"),
    legend.position = "none",
    strip.background = element_rect(fill = "#ccccff", linetype = "blank"),
    strip.text = element_markdown(family = "uc", face = "bold"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(color = NA, fill = bg_gradient),
    axis.text = element_text(family = "uc"),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(margin = margin(t=5), color = "white")
)

ggsave(
  filename = "2023/w17/w17-2023-tt.png",
  plot = plot,
  device = "png",
  height = 4,
  width = 6)

