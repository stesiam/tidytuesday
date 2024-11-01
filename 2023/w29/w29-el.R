## Import libraries

library(readr)
library(dplyr)
library(ggplot2)
library(ggtext)
library(glue)
library(patchwork)
library(ggimage)

#library(extrafont)


library(showtext)
library(sysfonts)

## Load fonts

sysfonts::font_add_google("Amiri", "Amiri")
sysfonts::font_add_google("EB Garamond","eb")
sysfonts::font_add_google("Arvo","arvo")
sysfonts::font_add_google("Ubuntu Condensed","caption")
sysfonts::font_add_google("Jost","jost")



#sysfonts::font_add_google("Gentium Plus", "gp")
sysfonts::font_add('gp',"/home/stelios/Downloads/Gentium_Plus/GentiumPlus-Regular.ttf")

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

title = glue("<span style='font-family:fs; color:#ffffff;'>&#xf544;</span> Τεχνητή Νοημοσύνη ή Άνθρωπος <span style='font-family:fs; color:#ffffff;'>&#xf183;</span>")
subtitle = glue("Μία από τις χρήσεις της τεχνητής νοημοσύνης (ΤΝ) είναι η παραγωγή κειμένου. Παρατηρείται ότι τα μοντέλα GPT-3 και GPT-4 της OpenAI παράγουν
                  απαντήσεις που δεν μπορούν να αναγνωριστούν ως ΤΝ. Ειδικά για το GPT-4, οι απαντήσεις του είναι 
    **<span style = 'color: #FFDAB9; font-weight:bold;'>{round((df$pct[[2]]- df$pct[[1]]), digits = 1)}</span>**% 
    λιγόυερο πιθανό να αναγνωριστούν σε σύγκριση με το προηγούμενο μοντέλο, GPT-3. Όσον αφορά τα σκάνερ ταξινόμησης κειμένων,
                η <b>OriginalityAI</b> έχει τη μεγαλύτερη ακρίβεια, ωστόσο είναι σχετικά μικρή. Συνεπώς, δεν έχουμε τα εργαλεία
                που θα μας υποδεικνύουν με σχετική ακρίβεια αν ένα κείμενο το συνέγγραψε ένας άνθρωπος ή ΤΝ.")
caption = "Tidy Tuesday, week 29<br><span style='font-family:fb;'>&#xf09b;</span> stesiam, 2023"

labels = c(
  OriginalityAI = "<img src='2023/w29/images/originalityai.png' width='12'  style='margin-right:50px;'/> &nbsp;&nbsp;  OriginalityAI",
  ZeroGPT = "<img src='2023/w29/images/zerogpt.png'  width='12'/>  &nbsp;&nbsp;&nbsp;&nbsp; ZeroGPT",
  HFOpenAI = "<img src='2023/w29/images/hf.png'  width='10'/>  &nbsp;&nbsp;&nbsp;&nbsp; HFOpenAI",
  Crossplag = "<img src='2023/w29/images/crossplag.png'  width='10'/>  &nbsp;&nbsp;&nbsp;&nbsp; Crossplag",
  Sapling = "<img src='2023/w29/images/sapling.png'  width='10'/> &nbsp;&nbsp;&nbsp;&nbsp;  Sapling",
  GPTZero = "<img src='2023/w29/images/gptzero.png'  width='10'/>  &nbsp;&nbsp;&nbsp;&nbsp; GPTZero",
  Quil = "<img src='https://i0.wp.com/quil.ai/wp-content/uploads/2023/09/transparent-2.png'  width='10'/> &nbsp;&nbsp;&nbsp;&nbsp; Quil"
)


labels = c(
  "OriginalityAI", "ZeroGPT", "HFOpenAI", "Crossplag", "Sapling",
  "GPTZero", "Quill"
)

images = c(
  "2023/w29/images/originalityai.png",
  "2023/w29/images/zerogpt.png",
  "2023/w29/images/hf.png",
  "2023/w29/images/crossplag.png",
  "2023/w29/images/sapling.png",
  "2023/w29/images/gptzero.png",
  "https://i0.wp.com/quil.ai/wp-content/uploads/2023/09/transparent-2.png"
)

tool_and_logo = data.frame(
  detector = labels,
  logo = images
)

p1 = ggplot(df) +
  geom_point(x = 6, y = 70, color = "#C74B4B", size = 37) +
  geom_point(x = 2, y = 45, color = "#FF6F61", size = 29) +
  scale_size_continuous(range = c(29, 50)) +
  geom_richtext(x = 2, y = 45, label = glue("**<span style='color:black;font-family:arvo;'>GPT-3</span>**<br> {paste0(round(df$pct[1], digits = 0), ' %')}"),
                aes(size = pct), 
                family = "arvo",
                color = "white",size = 6,
                fill = NA, label.color = NA, hjust = 0.5) +
  geom_richtext(x = 6, y = 70, 
                label = glue("**<span style='color:black;font-family:arvo;'>GPT-4</span>**<br> {paste0(round(df$pct[2], digits = 0), ' %')}"), aes(size = pct), 
                family = "arvo",
                color = "white",size = 8,
                fill = NA, label.color = NA, hjust = 0.5) +
  geom_curve(
    aes(x = 4.85, y = 84, xend = 1.5, yend = 80),
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
    label = glue("Κυκλοφόρησε τον<br>Μάρτιο του 2023"),
    size = 3, family = "caption",
    color = "#E0E0E0", hjust = 0.5
  ) +
  geom_curve(
    aes(x = 3, y = 35, xend = 5.5, yend = 35),
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
    x = 5.8, y = 40, fill = NA, label.color = NA,
    label = glue("Κυκλοφόρησε τον<br>Ιούνιο του 2020"),
    size = 3, family = "caption",
    color = "#E0E0E0", hjust = 0.5
  ) +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(30,90)) +
  labs(
    title = "Ξεγελώντας τους ανιχνευτές ΤΝ",
    subtitle = "(%) κειμένων που παράχθηκαν από ΤΝ και δεν εντοπίστηκαν",
    x = "Model",
    y = "Percentage"
  ) +
  theme_classic(base_size = 8) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    plot.title = element_markdown(family = "serif",face = "bold", color = "black"),
    plot.subtitle = element_markdown(family = "caption", color = "#E0E0E0"),
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    plot.background = element_rect(fill = "transparent", color = "transparent"),
    axis.ticks = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.text = element_blank()
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
  left_join(., tool_and_logo, by = "detector") %>%
  ggplot(.) +
  geom_col(aes(x = pct, y = reorder(detector, pct), fill = pct)) +
  scale_fill_gradient(low = "#CAB2D6", high = "#6c38aa") +
  geom_text(aes(x = pct-6, y = reorder(detector, pct), label = paste0(pct, " %")),
            family = "arvo", color = "white") +
  geom_text(aes(x = 1, y = reorder(detector, pct), label = detector),
            family = "arvo", color = "black", hjust = 0, size = 3) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 20,40,60),
                     limits = c(0, 62)) +
  labs(
    x = "Accuracy",
    y = "",
    title = "Ακρίβεια ταξινόμησης",
    subtitle = "(%) Αληθώς θετικά (ΤΝ) + Αληθώς Αρνητικά, επί του συνόλου"
  ) +
  theme_classic(base_size = 8) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    plot.title = element_markdown(family = "serif",face = "bold", color = "black"),
    plot.subtitle = element_markdown(family = "caption", color = "#E0E0E0"),
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    plot.background = element_rect(fill = "transparent", color = "transparent"),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  )


bg_gradient <- grid::linearGradient(colours = rev(MetBrewer::met.brewer("Hokusai1")[6:7]))

title_theme = theme(
  plot.title = element_markdown(
    family = "serif", 
    face = "bold",hjust = 0.5,
    color = "#FFF5E1",
    margin = margin(t = 5, b = 5)),
  plot.subtitle = element_textbox_simple(
    family = "serif", lineheight = 1.3,
    size = 8,
    margin = margin(b = 10, t = 5),
    color = "#E0E0E0"),
  plot.caption = element_markdown(family = "jost", lineheight = 1.3, color = "#E0E0E0", size = 7),
  plot.background = element_rect(fill = bg_gradient, color = bg_gradient))


combined = (p1 + p2) &
  plot_annotation(
    title = title,
    subtitle = subtitle,
    caption = caption,
    theme = title_theme
  )


ggsave(
  filename = "2023/w29/w29-2023-tt-el.png",
  plot = combined,
  device = "png",
  height = 4,
  width = 6)

