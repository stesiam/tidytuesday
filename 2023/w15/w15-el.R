library(readr)
library(dplyr)
library(tidyr)

library(ggtext)
library(lubridate)

library(glue)
library(ggplot2)
library(patchwork)
library(wesanderson)
library(showtext)
library(sysfonts)

## Load fonts

sysfonts::font_add_google("Amiri", "Amiri")
sysfonts::font_add_google("Jost","jost")
sysfonts::font_add_google("Oswald","caption")
#sysfonts::font_add_google("Gentium Plus", "gp")
#sysfonts::font_add('gp',"/home/stelios/Downloads/Gentium_Plus/GentiumPlus-Regular.ttf")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')


showtext_auto()
showtext::showtext_opts(dpi = 300)


eggproduction  <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/egg-production.csv')
cagefreepercentages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/cage-free-percentages.csv')

## Convert date format from ymd to ym

eggproduction$observed_month = lubridate::ymd(eggproduction$observed_month) %>% as.Date()

eggproduction

min_egg_total = eggproduction %>%
  filter(prod_type == "table eggs" & prod_process == "all") %>%
  summarise(min = round(min(n_eggs)/10^9, 1))


max_egg_total = eggproduction %>%
  filter(prod_type == "table eggs" & prod_process == "all") %>%
  summarise(max = round(max(n_eggs)/10^9, 1))


title = glue("<b>Παραγωγή αυγών για κατανάλωση στις ΗΠΑ</b>")
subtitle = glue("Η παραγωγή αυγών που προορίζονται για κατανάλωση είναι σχετικά σταθερή τα τελευταία χρόνια αφού κυμαίνεται μεταξύ <span style = 'color:red;'><b>{min_egg_total}</b></span> και 
                <span style = 'color:red;'><b>{max_egg_total}</b></span> δις το μήνα. Ένα σημαντικό στατιστικό
                είναι η <b>αυξημένη συμμετοχή των αυγών από κότες ελευθέρας βοσκής</b> ως σύνολο της συνολικής παραγωγής.
                Τα πρόσφατα δεδομένα (2021) υποδεικνύουν ότι τα αυγά που προέρχονται από κότες ελευθέρας βοσκής καταλαμβάνουν το 1/4
                του συνόλου, αντί του 10% το 2016.")

caption = glue("<b>Πηγή:</b> The Humane League | Tidy Tuesday, week 15<br><b>Γράφημα: </b><span style='font-family:fb;'>&#xf09b;</span><b> stesiam</b>, 2023")

min_egg_total = eggproduction %>%
  filter(prod_type == "table eggs" & prod_process == "all") %>%
  summarise(min = min(n_eggs))


max_egg_total = eggproduction %>%
  filter(prod_type == "table eggs" & prod_process == "all") %>%
  summarise(max = max(n_eggs))

extract_proportion_caged_uncaged = eggproduction |> 
  select(observed_month, prod_process, prod_type, n_eggs) |> 
  dplyr::filter(prod_type == "table eggs") |>
  mutate(year = lubridate::year(observed_month)) |>
  pivot_wider(names_from = prod_process, values_from = n_eggs) |>
  tidyr::drop_na() |>
  dplyr::rename(
    CF_NO = `cage-free (non-organic)`,
    CF_O = `cage-free (organic)`
  ) |>
  mutate(caged = all - CF_NO - CF_O,
         cage_free = CF_NO + CF_O) |>
  select(year, caged, cage_free) |>
  pivot_longer(!c(year), names_to = "prod_process",values_to = "obs") |>
  group_by(year, prod_process) |>
  summarise(average = mean(obs)) |>
  ungroup() |>
  group_by(year) |>
  mutate(sum = sum(average),
         pct = round((average/sum)*100, digits = 1) )


image1 = "2023/w15/grass.png"
image2 = "2023/w15/cage.png"

plot1 = ggplot(extract_proportion_caged_uncaged) +
  # color = "white" indicates the color of the lines between the areas
  geom_line(aes(x = year, y = pct, group = prod_process, color = prod_process)) +
  geom_text(x = 2017.3, y = 81, label = "Κλωβοστοιχίας", family = "serif", 
            color = wes_palette(n=2, name="Royal1")[[2]], angle = -6, size = 3) +
  geom_text(x = 2017.4, y = 21, label = "Ελευθέρας βοσκής", family = "serif", 
            color = wes_palette(n=2, name="Royal1")[[1]], angle = 6, size = 3) +
  geom_image(x = 2018.6, y = 25, image = image1, size = 0.1) +
  geom_image(x = 2018.4, y = 78, image = image2, size = 0.08) +
  geom_point(data = extract_proportion_caged_uncaged %>% dplyr::filter(year %in% c(2016,2019,2021)), 
             aes(x = year, y = pct, color = prod_process), size = 7) +
  scale_x_continuous(expand = c(0, 0), limits = c(2015.5, 2021.5), breaks = c(2016, 2019, 2021)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 95), breaks = seq(0,90,10)) +
  geom_richtext(data = extract_proportion_caged_uncaged %>% dplyr::filter(year %in% c(2016,2019,2021)),
                aes(x = year, y = pct, label = pct), color = "white", size = 2.5,
                fill = NA, label.color = NA, fontface = "bold", 
                family = "caption",hjust = 0.5) +
  labs(
    title = "Παραγωγή αυγών ανά συνθήκες εκτροφής",
    subtitle = "(%) συνολικών αυγών"
  ) + 
  scale_color_manual(values=wes_palette(n=2, name="Royal1")) +
  theme_classic(base_size = 8) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    plot.title = element_markdown(family = "serif",face = "bold", color = "white"),
    plot.subtitle = element_markdown(family = "serif", color = "grey60"),
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    plot.background = element_rect(fill = "transparent", color = "transparent"),
    axis.text = element_text(color = "white"),
    axis.ticks = element_line(color = "grey90")
  )



proportion_organic_non_organic = eggproduction |> 
  select(observed_month, prod_process, prod_type, n_eggs) |> 
  dplyr::filter(prod_type == "table eggs") |>
  mutate(year = lubridate::year(observed_month)) |>
  pivot_wider(names_from = prod_process, values_from = n_eggs) |>
  tidyr::drop_na() |>
  dplyr::rename(
    CF_NO = `cage-free (non-organic)`,
    CF_O = `cage-free (organic)`
  ) |>
  mutate(CF_Total = CF_NO + CF_O,
         Pct_non_organic = CF_NO/CF_Total,
         Pct_organic = CF_O/CF_Total) |>
  select(observed_month, Pct_non_organic, Pct_organic) |>
  pivot_longer(!c(observed_month), names_to = "type", values_to = "obs") |>
  mutate(year = lubridate::year(observed_month)) |>
  group_by(year, type) |>
  summarise(average = mean(obs)) |>
  mutate(average = round(average*100, 1))


plot2 = ggplot(proportion_organic_non_organic) +
  # color = "white" indicates the color of the lines between the areas
  geom_line(aes(x = year, y = average, group = type, color = type)) +
  geom_text(x = 2017.3, y = 72, label = "Μη βιολογικά", family = "serif", 
            color = wes_palette(n=2, name="Zissou1")[[1]], angle = 13, size = 3) +
  geom_text(x = 2017.4, y = 36.4, label = "Βιολογικά", family = "serif", 
            color = wes_palette(n=2, name="Zissou1")[[2]], angle = -10, size = 3) +
  geom_point(data = proportion_organic_non_organic %>% dplyr::filter(year %in% c(2016,2019,2021)),
             aes(x = year, y = average, color = type), size = 7) +
  scale_x_continuous(expand = c(0, 0), limits = c(2015.5, 2021.5), breaks = c(2016, 2019, 2021)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 95), breaks = seq(0,90,10)) +
  geom_richtext(data = proportion_organic_non_organic %>% dplyr::filter(year %in% c(2016,2019,2021)),
                aes(x = year, y = average, label = average), color = "white", size = 2.5,
                fill = NA, label.color = NA, fontface = "bold", 
                family = "caption",hjust = 0.5) +
  labs(
    title = "Παραγωγή αυγών ανά τύπο εκτροφής",
    subtitle = "(%) επί του συνόλου των ελευθέρας βοσκής αυγών"
  ) + 
  scale_color_manual(values=wes_palette(n=2, name="Zissou1")) +
  theme_classic(base_size = 8) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    plot.title = element_markdown(family = "serif",face = "bold", color = "white"),
    plot.subtitle = element_markdown(family = "serif", color = "grey60"),
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    plot.background = element_rect(fill = "transparent", color = "transparent"),
    axis.text = element_text(color = "white"),
    axis.ticks = element_line(color = "grey90")
  )  

title_theme = theme(
  plot.title = element_markdown(
    family = "serif", 
    face = "bold",hjust = 0.5,
    color = "white",
    margin = margin(t = 5, b = 5)),
  plot.subtitle = element_textbox_simple(
    family = "serif", lineheight = 1.3,
    size = 8.5,
    margin = margin(b = 10, t = 5),
    color = "white"),
  plot.caption = element_markdown(family = "serif", lineheight = 1.3, color = "white",
                                  size = 6.5),
  plot.background = element_rect(fill = 'black', color = "black")
)


combined = (plot1 + plot2) &
  plot_annotation(
    title = title,
    subtitle = subtitle,
    caption = caption,
    theme = title_theme
  )

ggsave(
  filename = "2023/w15/w15-2023-tt-el.png",
  plot = combined,
  device = "png",
  height = 4,
  width = 6
)

