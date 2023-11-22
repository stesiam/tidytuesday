## Import libraries

library(readr)
library(dplyr)
library(ggplot2)
library(ggtext)
library(glue)
library(patchwork)

#library(extrafont)


library(showtext)
library(sysfonts)

## Load fonts

sysfonts::font_add_google("Amiri", "Amiri")
sysfonts::font_add_google("EB Garamond","eb")
sysfonts::font_add_google("Karantina","title")

#sysfonts::font_add_google("Gentium Plus", "gp")
#sysfonts::font_add('gp',"/home/stelios/Downloads/Gentium_Plus/GentiumPlus-Regular.ttf")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()
showtext::showtext_opts(dpi = 300)


## Import data

detectors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-18/detectors.csv')


df <- detectors %>%
  select(kind, .pred_class, model) %>%
  mutate(s = case_when(kind == .pred_class ~ "Yes",
                           kind != .pred_class ~ "No")) %>%
  filter(model != "Human") %>%
  select(model, s) %>%
  group_by(model, s) %>%
  summarise(count = n()) %>%
  group_by(model) %>%
  mutate(countT= sum(count)) %>%
  mutate(pct = round(count/countT *100, digits = 2)) %>%
  filter(s == "No")

title = glue("<span style='font-family:fs; color:#6d6c6c;'>&#xf544;</span> Comparing Detectability of GPT <span style='font-family:fs; color:#6d6c6c;'>&#xf544;</span>")
subtitle = glue("It is observed a big difference between GPT-3 and GPT-4 in temrs
    of detectability of AI <br>response. The GPT-4 responses were 
    <span style = 'color: red; font-weight:bold;'>{round((df$pct[[2]]- df$pct[[1]])/df$pct[[1]]*100, digits = 1)}</span>
                less possible to be detected by an AI detector<br>scanner. Also,
                <b>OriginalityAI</b> seems to have the higher accuracy among other
                detectors.")
caption = "Tidy Tuesday, week 29<br><span style='font-family:fb;'>&#xf09b;</span> stesiam, 2023"

labels = c(
  OriginalityAI = "<img src='2023/w29/images/originalityai.png' width='10'/> OriginalityAI",
  ZeroGPT = "<img src='2023/w29/images/zerogpt.png'  width='10'/> ZeroGPT",
  HFOpenAI = "<img src='2023/w29/images/hf.png'  width='10'/>HFOpenAI",
  Crossplag = "<img src='2023/w29/images/crossplag.png'  width='10'/> Crossplag",
  Sapling = "<img src='2023/w29/images/sapling.png'  width='10'/> Sapling",
  GPTZero = "<img src='2023/w29/images/gptzero.png'  width='10'/> GPTZero",
  Quil = "Quil"
)

p1 = ggplot(df) +
  geom_col(aes(x = model, y = pct, fill = model)) +
  geom_text(aes(x = model, y = pct+3, label = paste0(pct, " %")), 
            family = "Amiri",
            size = 3) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,80)) +
  labs(
    title = "",
    x = "Model",
    y = "Percentage"
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill=NA),
    plot.background = element_rect(fill = NA),
    plot.title = element_markdown(family = "Amiri", size = 5, face = "bold"),
    legend.position = "none"
  )

p2 = detectors %>%
  select(kind, .pred_class, detector) %>%
  mutate(result = case_when(kind == .pred_class ~ "Yes",
                            kind != .pred_class~ "No")) %>%
  select(detector, result) %>%
  group_by(detector, result) %>%
  summarise(n = n()) %>%
  group_by(detector) %>%
  mutate(countT = sum(n)) %>%
  mutate(pct = round(n/countT*100, digits =1)) %>%
  filter(result == "Yes") %>%
  arrange(-pct) %>%
  ggplot(.) +
  geom_col(aes(x = pct, y = reorder(detector, pct)),
           fill = "dodgerblue3", alpha = 0.5) +
  geom_text(aes(x = pct-6, y = reorder(detector, pct), label = paste0(pct, " %")),
            family = "Amiri", color = "white") +
  scale_y_discrete(
    name = NULL,
    labels = labels
  ) +
  labs(
    x = "Accuracy",
    y = ""
  ) +
  theme_classic(base_family = "eb") +
  theme(
    panel.background = element_rect(fill=NA),
    plot.background = element_rect(fill=NA),
    legend.position = "none",
    axis.text.y = element_markdown(color = "black", size = 11)
  )

plot = (p1 + p2)  +
  plot_layout(ncol = 2, nrow = 1) +
  plot_annotation(
    title = title,
    subtitle = subtitle,
    caption = caption
  )  &
  theme(
    text = element_text(family = "eb"),
    plot.background = element_rect(fill="#ebc296", colour = "#ebc296"),
    plot.title = element_markdown(family = "title", size = 20, 
                                  hjust = 0.5,
                                  margin = margin(t = 2, b = 8)),
    plot.subtitle = element_markdown(family = "eb", size = 12, hjust = 0.5),
    plot.caption = element_markdown(family = "eb", size= 10),
    legend.position = "none"
  )


ggsave(
  filename = "2023/w29/w29-2023-tt.png",
  plot = plot,
  device = "png", height = 3.9, width = 6.68)

#6.68 * 3.91 dimensions
## 2003 × 1171 pixels

