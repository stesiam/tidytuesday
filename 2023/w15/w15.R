library(readr)
library(dplyr)

library(ggtext)
library(extrafont)
library(lubridate)

library(glue)

library(ggplot2)
library(geomtextpath)

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
                    color = prod_process, label = prod_process), size = 4, 
                    fontface = 2, hjust = 0.23, vjust = -0.3, lwd=1) +
  geom_vline(xintercept= lubridate::ym("2020-01"), linetype="dashed", color = "red") +
  geom_text(aes(x=lubridate::ym("2020-02"), label="First confirmed COVID-19 case in US", y=4), 
            angle=270,
            family = "EB Garamond") +
  geom_text(mapping = aes(x = observed_month, 
                          y = round(n_eggs/10^9, digits = 1), 
                          label = ifelse(max(lubridate::ym(observed_month)) == lubridate::ym(observed_month), round(n_eggs/10^9, digits = 2), ""))) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = "Tidy Tuesday, <b>Week 15</b><br> stesiam, 2023",
    x = "Year",
    y = "Eggs",
    color = "Production"
  ) +
  theme_classic() +
    theme(
      plot.title = element_markdown(family = "Amiri", size = 25),
      plot.subtitle = element_markdown(family = "EB Garamond", size = 12, lineheight = 1),
      plot.caption = element_markdown(family = "EB Garamond", lineheight = 1),
      legend.position = "none",
      text = element_text(family = "EB Garamond", size = 16)
    )
  
ggsave(
    filename = "2023/w15/w15-2023-tt.png",
    plot = plot,
    device = "png",
    bg = "white"
  )

  