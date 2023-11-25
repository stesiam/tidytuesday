library(readr)
library(dplyr)

library(ggtext)
# library(extrafont)
library(lubridate)

library(glue)

library(ggplot2)
library(geomtextpath)

library(showtext)
library(sysfonts)

## Load fonts

sysfonts::font_add_google("Amiri", "Amiri")
sysfonts::font_add_google("EB Garamond","eb")
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

min_egg_total = eggproduction %>%
  filter(prod_type == "table eggs" & prod_process == "all") %>%
  summarise(min = round(min(n_eggs)/10^9, 1))


max_egg_total = eggproduction %>%
  filter(prod_type == "table eggs" & prod_process == "all") %>%
  summarise(max = round(max(n_eggs)/10^9, 1))


title = glue("<b>Egg Production in US</b>")
subtitle = glue("Total production of table eggs is relatively stable the last years, 
                ranging from <span style = 'color:red;'><b>{min_egg_total}</b></span> to 
                <span style = 'color:red;'><b>{max_egg_total}</b></span> billion <br> table-eggs per month. A worth-noting
                fact is increase of <b>cage-free</b> table eggs as a portion <br> of total production.
                Most recent data reveal that the <b>cage-free</b> eggs are one quarter of the total <br> production in comparison with the 9.8 at December of 2016")

caption = glue("<b>SOURCE:</b> The Humane League | Tidy Tuesday, week 15<br><b>VISUALIZATION: </b><span style='font-family:fb;'>&#xf09b;</span><b> stesiam</b>, 2023")

min_egg_total = eggproduction %>%
  filter(prod_type == "table eggs" & prod_process == "all") %>%
  summarise(min = min(n_eggs))


max_egg_total = eggproduction %>%
  filter(prod_type == "table eggs" & prod_process == "all") %>%
  summarise(max = max(n_eggs))

plot = eggproduction %>%
  select(-source) %>%
  filter(prod_type == "table eggs") %>%
  ggplot(.) +
  geom_textline(aes(x = observed_month, y = round(n_eggs/10^9, digits = 2), 
                    color = prod_process, label = prod_process), size = 2, 
                    fontface = 2, hjust = 0.23, vjust = -0.3, lwd=1) +
  geom_vline(xintercept= lubridate::ym("2020-01"), linetype="dashed", color = "red") +
  geom_text(aes(x=lubridate::ym("2020-02"), label="First confirmed COVID-19 case in US", y=4), 
            angle=270,
            family = "eb", size = 2.5) +
  geom_text(mapping = aes(x = observed_month, 
                          y = round(n_eggs/10^9, digits = 1), 
                          label = ifelse(max(lubridate::ym(observed_month)) == lubridate::ym(observed_month), round(n_eggs/10^9, digits = 2), ""))) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = "Year",
    y = "Eggs",
    color = "Production"
  ) +
  theme_classic(base_size = 8) +
    theme(
      plot.title = element_markdown(family = "Amiri", size = 20),
      plot.subtitle = element_markdown(family = "eb", lineheight = 1),
      plot.caption = element_markdown(family = "caption", lineheight = 1.4),
      legend.position = "none",
      text = element_text(family = "eb")
    )
  
ggsave(
    filename = "2023/w15/w15-2023-tt.png",
    plot = plot,
    device = "png",
    bg = "white",
    height = 4,
    width = 4.5
  )

  