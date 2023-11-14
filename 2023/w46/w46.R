library(readr)
library(dplyr)
library(tidyr)

library(ggplot2)
library(ggtext)
library(glue)
library(ggh4x)

library(waffle)
library(wesanderson)

library(showtext)
library(sysfonts)

## Load fonts

sysfonts::font_add_google("Oswald", "title")
sysfonts::font_add_google("EB Garamond","eb")
#sysfonts::font_add_google("Gentium Plus", "gp")
sysfonts::font_add('gp',"/home/stelios/Downloads/Gentium_Plus/GentiumPlus-Regular.ttf")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')


showtext_auto()
showtext::showtext_opts(dpi = 300)

house <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-14/diwali_sales_data.csv')

sum(is.na(house))
l = house %>%
  drop_na() %>%
  group_by(`Age Group`) %>%
  summarise(s = sum(Amount))

l$label = "apple"

title = glue("Diwali Sales Data")
subtitle = glue("<b>Diwali</b>, (also called Deepavali) is the Hindu festival of lights with its variations also celebrated
                in other <br>Indian religions. The celebrations generally last five or six days. On this week's dataset we have sales data.<br>
                We observe that the age group 26-35 age group have spent the most money. On the other hand, the <br>underage people spent the less.
                In total, the sales sums up to {round(sum(l$s)/1000000, 1)} millions rupes.")
caption = glue("Tidy Tuesday, week 46<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2023")



data.frame(
  states = l$`Age Group`,
  vals = l$s) -> xdf

xdf %>%
  count(states, wt = vals) -> waf

design <- c(
"
AABBCC
DDEEFF
##GG##
"
)

waf$la = "money-bill-wave"

p1 = ggplot(waf, aes(fill = states, values = n/1000000, label = la))+
  facet_grid(~states) +
  geom_pictogram( n_rows = 5, size = 2, color = "green4", family = "fs") +
  scale_label_pictogram(
    name = NULL,
    values = c(
      "money-bill-wave" = "money-bill-wave"
    )
  ) +
  ggh4x::facet_manual(~states,design = design) +
  theme_minimal() +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption
  )  +
  theme_classic(base_size = 10,
                base_family = "gp") +
  theme_enhance_waffle() +
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_markdown(family = "title", color ="#f9e099", face = "bold", fill = "black",margin = margin(t = 5, b = 5), hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "eb",
                                     color = "white",
                                     margin = margin(t = 5, b = 10, l = 10, r = 10),
                                     fill = "black"),
    legend.background = element_rect(fill = "white"),
    legend.position = "none",
    legend.text = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black"),
    plot.caption = element_markdown(family = "title", margin = margin(t = 5, r = 5), lineheight = 1.2,
                                    color = "white"),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white",size = 15,face = "bold", family = "title")
  )

ggsave(
  filename = "2023/w46/w46-2023-tt.png",
  plot = p1,
  device = "png",
  height = 4,
  width = 6)


