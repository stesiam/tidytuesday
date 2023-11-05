library(readr)
library(dplyr)

library(ggplot2)
library(ggtext)
library(glue)

library(showtext)
library(sysfonts)

library(usmap)
## Load fonts

sysfonts::font_add_google("Oswald", "title")
sysfonts::font_add_google("Pacifico", "pc")
sysfonts::font_add_google("Lilita One", "lo")
sysfonts::font_add_google("EB Garamond", "eb")
sysfonts::font_add_google("Roboto Condensed", "rbc")


sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')


showtext_auto()
showtext::showtext_opts(dpi = 300)

horror_articles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-31/horror_articles.csv')

ggplot(horror_articles, aes(label = subtitle)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 30) +
  theme_minimal()

d = horror_articles %>%
  select(author,rating) %>%
  filter(rating == "true" | rating == "false") %>%
  group_by(author) %>%
  summarise(n= n()) %>%
  arrange(-n) %>%
  filter(n >5)

e = horror_articles %>% 
  select(author, rating) %>% 
  filter(rating == "true" | rating == "false") %>%
  group_by(author, rating) %>% 
  summarise(m = n()) %>% 
  mutate(pct = m/sum(m))


g = left_join(d, e, by = c("author"))

title = glue("<span style='font-family:fs; color: black;'>&#xf002;</span> Snopes outcomes per author")
subtitle = glue("<b>Snopes</b>, formerly known as the Urban Legends Reference Pages, is a fact-checking website.
                It has been <br>described as a 'well-regarded reference for sorting out myths and rumors' on the Internet.
                The site <br>has also been seen as a source for both validating and debunking urban legends and similar stories<br> in American popular culture.
                The main author ")
caption = glue("Tidy Tuesday, week 44<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2023")


p1 <-
  g %>%
  ggplot(aes(pct, author)) +
  geom_segment(
    data = g %>%
      pivot_wider(
        names_from = rating,
        values_from = pct
      ),
    aes(x = true, xend = false, y = author, yend = author),
    alpha = 0.7, color = "gray90", size = 1.5
  ) +
  geom_point(aes(color = rating), size = 3) +
  scale_x_continuous(limits = c(0,1)) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,x = NULL,
    y = NULL, color = NULL) +
  theme_classic(base_size = 16,
                base_family = "title") +
  theme(
    plot.title = element_markdown(family = "title",margin = margin(t = 5, b = 5), hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "eb",
                                     margin = margin(t = 5, l = 10, r = 10), size = 10),
    legend.background = element_rect(fill = "#ffffb3"),
    legend.position = "top",
    legend.text = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "#ffffb3"),
    panel.background = element_rect(fill = "#ffffb3"),
    plot.caption = element_markdown(family = "title", margin = margin(t = 5, r = 5), lineheight = 1.2)
  )

ggsave(
  filename = "2023/w44/w44-2023-tt.png",
  plot = p1,
  device = "png",
  height = 4,
  width = 6)


  
  