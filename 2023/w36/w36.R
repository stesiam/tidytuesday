library(readr)
library(dplyr)
library(ggplot2)
library(ggtext)
library(glue)
library(stringr)
library(tidyr)
library(forcats)


library(showtext)
library(sysfonts)

## Load fonts

sysfonts::font_add_google("Arvo", "arvo")
sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-5.15.4-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')


showtext_auto()
showtext::showtext_opts(dpi = 300)
# Import datasets

demographics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/demographics.csv')
wages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/wages.csv')
states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv')

# Filter data

demographics = demographics %>%
  filter( facet %in% c("private sector: all", "public sector: all")) %>%
  mutate(p_members = round(p_members * 100, digits = 1)) |>
  drop_na(p_covered) |>
  mutate(
    decade = case_when(
      year >= 1970 & year <= 1979 ~ "70s",
      year >= 1980 & year <= 1989 ~ "80s",
      year >= 1990 & year <= 1999 ~ "90s",
      year >= 2000 & year <= 2009 ~ "2000 - 2009",
      year >= 2010 & year <= 2019 ~ "2010 - 2019",
      year >= 2020 ~ "2020 - 2022",
      TRUE ~ NA_character_
    )
  ) |>
  drop_na(decade)

# Title, subtitle and caption
last_year_private = demographics %>%
  select(year, p_members, facet) %>%
  filter(facet == "private sector: all") %>%
  filter(year == 2022) %>%
  .[["p_members"]]

demographics$decade <- demographics$decade %>%
  fct_relevel(
    "2020 - 2022", "2010 - 2019", "2000 - 2009", "90s", "80s", "70s"
  )
demographics$facet <- demographics$facet %>%
  fct_relevel(
    "public sector: all", "private sector: all"
  )

demographics$facet <- demographics$facet %>%
  fct_recode(
    "Public Sector" = "public sector: all",
    "Private Sector" = "private sector: all"
  )

## Find max perc of union members and year

find_max = demographics %>%
  select(year, p_members, facet) %>%
  group_by(facet) %>%
  filter(
    p_members == max(p_members)
  )

title = glue("Employees' participation in Unions")

subtitle = glue("In the early 70's the participation in unions was similar in private and public sector.
                (~ 1 out of 4 employees). Since then, the union participation in <span style = 'color: dodgerblue'>public sector</span> has significantly increased. 
                On the contrary, <span style = 'color: red'>private sector</span> employees' participation is decreasing year by year,
                reaching <span style = 'color: red'>{paste0(round(last_year_private, digits = 2), '%')} </span> in 2022.")
caption = "Tidy Tuesday, week 36<br><span style='font-family:fb; font-size:10px;'  >&#xf09b;</span> <b>stesiam</b>, 2023"


f = demographics |>
  dplyr::select(decade, facet, p_members) |>
  group_by(facet, decade) |>
  mutate(
    average = mean(p_members)
  ) |>
  distinct(facet, decade, average) |>
  spread(key = facet, value = average) |>
  mutate(diff = `Public Sector` - `Private Sector`)


plot = ggplot(f, aes(y = decade)) +
  geom_segment(aes(x = `Private Sector`, xend = `Public Sector`)) +
  geom_point(aes(x = `Private Sector`), size = 10, color = "#d35400") + 
  geom_point(aes(x = `Public Sector`), size = 10, color = "#2980b9") +
  geom_richtext(aes(x = `Public Sector`, label = round(`Public Sector`,0)),
                fill = NA, label.color = NA, color = "white", size = 5, fontface = "bold") +
  geom_richtext(aes(x = `Private Sector`, label = round(`Private Sector`,0)),
                fill = NA, label.color = NA, color = "white", size = 5, fontface = "bold") +
  geom_richtext(aes(x = (`Public Sector` +`Private Sector`)/2, label = round(diff,0)),
                fill = NA, label.color = NA, color = "red3", size = 5, fontface = "bold",
                nudge_y = 0.2) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = "Year",
    y = "Decade"
  ) +
  theme_classic(
    base_size = 9,
    base_family = "eb"
  ) +
  theme(
    text = element_text(family = "eb"),
    plot.title = element_markdown(family = "arvo", face = "bold",
                                  margin = margin(t = 5, b = 10),
                                  color = "#4e342e"),
    plot.background = element_rect(fill = "grey90", color = bg_gradient),
    panel.background = element_rect(fill = "transparent"),
    plot.subtitle = element_textbox_simple(family = "arvo", lineheight = 1.2,
                                           margin = margin(b = 10),
                                           color = "#34495e"),
    plot.caption = element_markdown(family = "arvo", lineheight = 1.2),
    legend.position = "none",
    legend.background = element_rect(fill = "transparent"),
    legend.title = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),strip.background = element_rect(fill = "black"), strip.text = element_markdown(color = "white")
  )

wages |> 
  select(year, union_wage, nonunion_wage, facet) |>
  dplyr::filter(facet %in% c("public sector: all", "private sector: all")) |>
  mutate(
    diff = round(union_wage - nonunion_wage, digits = 1)
  ) |>
  dplyr::filter(year %in% c(1980, 2020)) |>
  select(year, facet, diff)  |>
  pivot_wider(names_from = "year", values_from = "diff") |>
  mutate(diff = `2020` - `1980`)
  
  
  
w1 = ggplot(g) +
  geom_point(x = 6, y = 70, color = "#C74B4B", size = 37) +
  geom_point(x = 2, y = 45, color = "#FF6F61", size = 29) +
  scale_size_continuous(range = c(29, 50)) +
  geom_richtext(x = 2, y = 45, label = glue("**<span style='color:black;size:12pt'>GPT-3</span>**<br> {paste0(round(df$pct[1], digits = 0), ' %')}"),
                aes(size = pct), 
                family = "arvo",
                color = "white",size = 6,
                fill = NA, label.color = NA, hjust = 0.5) +
  geom_richtext(x = 6, y = 70, 
                label = glue("**<span style='color:black;'>GPT-4</span>**<br> {paste0(round(df$pct[2], digits = 0), ' %')}"), aes(size = pct), 
                family = "arvo",
                color = "white",size = 8,
                fill = NA, label.color = NA, hjust = 0.5) +
  geom_curve(
    aes(x = 4, y = 83, xend = 1.5, yend = 80),
    color = "#C74B4B",
    curvature = 0.4,
    size = 0.4,
    arrow = arrow(
      angle = 25,
      length = unit(0.1, "inches"),
      type = "closed"
    )
  ) +
  geom_richtext(
    x = 1.6, y = 75, fill = NA, label.color = NA,
    label = glue("Released in<br>March, 2023"),
    size = 3, family = "jost",
    color = "#E0E0E0", hjust = 0.5
  ) +
  geom_curve(
    aes(x = 3, y = 35, xend = 5.5, yend = 40),
    color = "#FF6F61",
    curvature = 0.4,
    size = 0.4,
    arrow = arrow(
      angle = 25,
      length = unit(0.1, "inches"),
      type = "closed"
    )
  ) +
  geom_richtext(
    x = 5.8, y = 45, fill = NA, label.color = NA,
    label = glue("Released in<br>June, 2020"),
    size = 3, family = "jost",
    color = "#E0E0E0", hjust = 0.5
  ) +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(30,90)) +
  labs(
    title = "Deceive AI Detectors by model",
    subtitle = "(%) of texts generated from AI and not spotted",
    x = "Model",
    y = "Percentage"
  ) +
  theme_classic(base_size = 8) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    plot.title = element_markdown(family = "jost",face = "bold", color = "black"),
    plot.subtitle = element_markdown(family = "caption", color = "#E0E0E0"),
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    plot.background = element_rect(fill = "transparent", color = "transparent"),
    axis.ticks = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.text = element_blank()
  )

  
bg_gradient <- grid::linearGradient(colours = rev(MetBrewer::met.brewer("Hokusai1")[6:7]))

title_theme = theme(
  plot.title = element_markdown(
    family = "jost", 
    face = "bold",hjust = 0.5,
    color = "#FFF5E1",
    margin = margin(t = 5, b = 5)),
  plot.subtitle = element_textbox_simple(
    family = "jost", lineheight = 1.3,
    size = 9,
    margin = margin(b = 10, t = 5),
    color = "#E0E0E0"),
  plot.caption = element_markdown(family = "jost", lineheight = 1.3, color = "#E0E0E0", size = 7),
  plot.background = element_rect(fill = bg_gradient, color = bg_gradient))


combined = (plot + w1) &
  plot_annotation(
    title = title,
    subtitle = subtitle,
    caption = caption,
    theme = title_theme
  )


ggsave(
  filename = "2023/w36/w36-2023-tt.png",
  plot = plot,
  device = "png",
  height = 4,
  width = 6
)

