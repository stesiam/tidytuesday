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
sysfonts::font_add('gpr',"/home/stelios/Downloads/Gentium_Plus/GentiumPlus-Regular.ttf")
sysfonts::font_add('gpb',"/home/stelios/Downloads/Gentium_Plus/GentiumPlus-Bold.ttf")

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


## NOTE: Manually found the rgba equivalent to hex one, as rgba() not working on ggplot

title = glue("<span style='font-family:fs; color:purple;'  >&#xf290;</span> <b>Diwali Sales Data per Age Group</b> <span style='font-family:fs; color:purple;'  >&#xf290;</span>")
subtitle = glue("<b>Diwali</b>, (also called Deepavali) is the Hindu festival of lights with its variations also celebrated
                in <br> other Indian religions. The celebrations generally last five or six days. In this week's dataset <br> we observe that the age group 26-35 
                has spent the most money while underage individuals<br>spent the least.
                In total, the sales sum up to <span style='font-weight: bold; font-family:gpb;'>{round(sum(l$s)/1000000, 1)} millions rupees</span>.")
caption = glue("<span style='font-family:title; color:grey;'><span style='font-weight: bold;'>NOTE:</span> Each </span> <span style='font-family:fs; color:#00f00080;'>&#xf53a;</span> <span style='font-family:title; color:grey;'>corresponds to 1 million (1,000,000) rupees ₹ (≈ 11,000 €)</span><br><b>SOURCE:</b> Kaggle Datasets | Tidy Tuesday, week 46<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2023")


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
  geom_pictogram( n_rows = 5, size = 2, color = rgb(red = 0, green = 250, blue = 0, alpha = 120, maxColorValue = 255), family = "fs") +
  scale_label_pictogram(
    name = NULL,
    values = c(
      "money-bill-wave" = "money-bill-wave"
    )
  ) +
  geom_text(aes(x = 5, y = 3, family = "gpb", group = states),label =paste0(round(l$s/1000000, 1)," mil. ₹"), color = "white", size = 2.5) +
  ggh4x::facet_manual(~states,design = design) +
  theme_minimal() +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption
  ) +
  theme_classic(base_size = 10,
                base_family = "gp") +
  theme_enhance_waffle() +
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_markdown(family = "title", color ="#f9e099", face = "bold", fill = "black",margin = margin(t = 5, b = 5), hjust = 0.5),
    plot.title.position = "plot",
    text = element_text(color = "white", family = "eb"),
    plot.subtitle = element_markdown(family = "gpr",
                                     color = "white",
                                     margin = margin(t = 5, b = 10, l = 10, r = 10),
                                     fill = "black",
                                     lineheight = 1.1),
    legend.background = element_rect(fill = "white"),
    legend.position = "none",
    legend.text = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black"),
    plot.caption = element_markdown(family = "title", margin = margin(t = 5, r = 5), lineheight = 1.2,
                                    color = "white"),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "red3",size = 12,face = "bold", family = "title")
  )

ggsave(
  filename = "2023/w46/w46-2023-tt.png",
  plot = p1,
  device = "png",
  height = 4,
  width = 6)

