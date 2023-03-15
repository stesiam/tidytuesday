## Import libraries

library(readr)
library(dplyr)
library(ggplot2)
library(ggtext)
library(showtext)

## Add fonts

font_add_google("Righteous", family = "clim")
font_add_google("Montserrat", family = "mont")

showtext_auto()

## Import data

stock_prices = read_csv("2023/w6/data/big_tech_stock_prices.csv")
companies = read_csv("2023/w6/data/big_tech_companies.csv")

## Create visualization

map = stock_prices %>%
  filter(stock_symbol %in% c("INTC", "NVDA")) %>%
  ggplot(., mapping = aes(x = date, y = adj_close, color = stock_symbol)) +
  geom_line(size = 0.8, key_glyph = "timeseries") +
  labs(
    title = "Intel vs NVIDIA Stock Prices",
    subtitle = "Comparison of Closing Stock Prices of NVIDIA (NVDA) and Intel Corporations (INTC)",
    caption = "stesiam, 2023 <br> Data : Tidy Tuesday, Week 6, 2023",
    x = "Date",
    y = "Adjusted closing price",
    color = "Company"
  ) +
  theme(
    plot.title = element_text(family = "clim",face = "bold",size = 25),
    plot.subtitle = element_text(family = "clim",size = 20),
    plot.caption = element_text(family = "clim",size = 14, hjust = 0.5),
    panel.background = element_rect(fill = "grey98"),
    plot.background = element_rect(fill = "grey98"),
    legend.background = element_rect(fill = "grey98"),
    line = element_line(lineend = "butt", size = 0.1),
    axis.text = element_text(family = "clim",size = 17),
    axis.title = element_text(family = "clim",size = 17),
    legend.position = c(.3, .7),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_text(family = "clim",size = 15),
    legend.text = element_text(family = "clim",size = 12)
)

## Export it


ggsave(
  filename = "2023/w6/w6-2023-tt.png",
  plot = map,
  device = "png",
  width = 4,
  height = 4)
