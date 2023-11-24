## Import libraries

library(readr)
library(dplyr)

library(ggplot2)
library(ggtext)


library(glue)

library(showtext)

## Add fonts


font_add_google("Righteous", family = "clim")
font_add_google("Montserrat", family = "mont")
font_add_google("Oswald", family = "caption")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')


showtext_auto()

## Import data

stock_prices = read_csv("2023/w6/data/big_tech_stock_prices.csv")
companies = read_csv("2023/w6/data/big_tech_companies.csv")

## Combine dataset

merged_dataset = left_join(stock_prices, companies, by = "stock_symbol") %>%
  relocate(company, .after=stock_symbol)

## Due to density of data and at the same time the small width of the viz I will
## include closing prices for every month. 


## Plot texts

title = glue("<span style='color:#4285f4;'>Google</span> vs <span style='color:#ff9900;'>Amazon</span> stock prices")
subtitle = glue("Comparison of Adjusted Closing Stock Prices of Gooogle (GOOGL) and Amazon (AMZN)")
caption = glue("<b>SOURCE:</b> Yahoo Finance via Kaggle Datasets | Tidy Tuesday, week 6<br><b>VISUALIZATION: </b><span style='font-family:fb;'>&#xf09b;</span> <b>stesiam</b>, 2023")


## Create visualization

map = stock_prices %>%
  filter(stock_symbol %in% c("GOOGL", "AMZN")) %>%
  ggplot(., mapping = aes(x = date, y = adj_close, color = stock_symbol)) +
  geom_line(size = 0.2, key_glyph = "timeseries") +
  scale_color_manual(values = c("#ff9900", "#4285f4")) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = "Date",
    y = "Adjusted closing price",
    color = "Company"
  ) +
  theme_light(
    base_size = 19,
    base_family = "clim"
  ) +
  theme(
    plot.title = element_markdown(family = "clim",face = "bold", margin = margin(b = 5), size = 50, hjust = 0.5),
    plot.subtitle = element_text(family = "mont"),
    plot.caption = element_markdown(family = "caption", lineheight = 0.5),
    panel.background = element_rect(fill = "grey98"),
    plot.background = element_rect(fill = "grey98"),
    legend.background = element_rect(fill = "grey98"),
    legend.position = c(.3, .7),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    panel.grid = element_blank()
)

## Export it


ggsave(
  filename = "2023/w6/w6-2023-tt.png",
  plot = map,
  device = "png",
  width = 4.5,
  height = 4)

