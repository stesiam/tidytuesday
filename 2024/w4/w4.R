## Load libraries

library(readr)
library(dplyr)
library(forcats)

library(ggplot2)
library(ggtext)
library(glue)

library(showtext)
library(sysfonts)

## Load fonts

sysfonts::font_add_google("Oswald", "title")
sysfonts::font_add_google("Josefin Slab","js")
sysfonts::font_add_google("Cabin Condensed","cc")
sysfonts::font_add_google("Ubuntu Condensed", "uc")

#sysfonts::font_add_google("Gentium Plus", "gp")
sysfonts::font_add('gp',"/home/stelios/Downloads/Gentium_Plus/GentiumPlus-Regular.ttf")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')


showtext_auto()
showtext::showtext_opts(dpi = 300)

# Import dataset

english_education <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-23/english_education.csv')


## Plot texts

title = glue("<b>Educational attainment of young people in English towns</b>")
subtitle = glue("On this week's TT, I would like to reproduce the results from the ONS survey.<br>
                 UK students from smaller towns seem to have, in average terms, better <br>
                 education level in comparison with bigger ones. Also, we can see the <br>
                 pattern that the results gradually worsen as the size of the town increases.")

caption = glue("<b>SOURCE:</b> UK Office for National Statistics | Tidy Tuesday, week 4 (2024)<br><b>VISUALIZATION: </b><span style='font-family:fb;'>&#xf09b;</span><b> stesiam</b>, 2024")

english_education$size_flag <- english_education$size_flag %>%
  fct_recode(
    "City" = "Inner London BUA",
    "Other" = "Not BUA",
    "Other" = "Other Small BUAs",
    "City"= "Outer london BUA"
  )

p =english_education %>% 
  dplyr::filter(size_flag != "Other") %>%
  group_by(size_flag) %>%
  summarise(n = n(), m = sum(education_score)) %>% mutate(mean = m/n) %>% filter(n >1) %>%
  mutate(color = ifelse(mean>0, "upper0", "lower0")) %>%
  ggplot(.) +
  geom_col(mapping = aes(x = reorder(size_flag, -mean), y = mean, fill = color)) +
  geom_richtext(aes(x = size_flag, y = mean+0.15, label = round(mean, 2)), fontface  = "bold", 
                family = "uc", label.color = NA, size = 4) +
  scale_fill_manual(values = c("lower0" = "#8B0000", "upper0" = "#006400") )+
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = "",
    y = "Average Education Score") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", lwd = 0.5) + 
  theme_classic(base_size = 13,
                base_family = "gp") +
  theme(
    plot.title = element_markdown(family = "js",
                                  margin = margin(t = 10, b = 5), 
                                  hjust = 0.5, 
                                  color = "black",face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "gp",
                                     margin = margin(t = 5, l = 10, r = 10, b = 5),
                                     lineheight = 1.1,
                                     color = "black"),
    axis.title.x = element_text(family = "gp", size = 10, margin = margin(b = 5), color ="black"),
    axis.title.y = element_text(family = "gp", size = 10, color = "black"),
    axis.text = element_text(color = "black"),
    plot.background = element_rect(fill = "#fffacd", color = "#fffacd"),
    panel.background = element_rect(fill = "#fffacd"),
    plot.caption = element_markdown(family = "title", margin = margin(t = 5, r = 5, b = 4), 
                                    lineheight = 1.4,
                                    color = "black", size = 8),
    plot.margin = margin(l=8, r=8),legend.position = "none"
  )

ggsave(
  filename = "2024/w4/w4-2024-tt.png",
  plot = p,
  device = "png",
  height = 4,
  width = 6)

