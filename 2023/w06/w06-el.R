## Import libraries

library(readr)
library(dplyr)
library(lubridate)

library(ggplot2)
library(ggimage)
library(ggtext)
library(glue)

library(showtext)
library(sysfonts)

## Load fonts

sysfonts::font_add_google("Ubuntu Condensed", "uc")
sysfonts::font_add_google("Jost", "jost")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()
showtext::showtext_opts(dpi = 300)

## Import data

stock_prices = read_csv("2023/w06/data/big_tech_stock_prices.csv")
companies = read_csv("2023/w06/data/big_tech_companies.csv")

## Combine dataset

merged_dataset = left_join(stock_prices, companies, by = "stock_symbol") %>%
  relocate(company, .after=stock_symbol)



## Due to density of data and at the same time the small width of the viz I will
## include closing prices for every month. 


clean_data = merged_dataset |>
  mutate(day = lubridate::day(date),
         month = lubridate::month(date),
         year = lubridate::year(date)) |>
  dplyr::filter(stock_symbol %in% c("AMZN", "GOOGL")) |>
  group_by(stock_symbol, year) |>
  summarise(average = mean(adj_close)) |>
  ungroup() |>
  mutate(
    image = case_when(
      stock_symbol == "AMZN" ~ "2023/w06/amazon_logo.png",
      TRUE ~ "2023/w06/google.png"
    )
  )

## Plot texts

title = glue("Τιμές μετοχών των <br> <span style='color:#4285f4;'>Google</span> και <span style='color:#ff9900;'>Amazon</span><br> σε $ δολλάρια ΗΠΑ")
subtitle = glue("Σύγκριση των προσαρμοσμένων τιμών κλεισίματος <br>των μετοχών της Google (GOOGL) και της<br> Amazon (AMZN) μεταξύ των ετών 2010 <br> και 2022.")
caption = glue("<span style = 'font-family:fs; color:#ffffff;'>&#xf1c0; </span> <b>Πηγή:</b> Yahoo Finance | Tidy Tuesday, week 6<br> <span style = 'font-family:fs; color:#ffffff;'>&#xe473; </span> <b>Γράφημα:</b><span style='font-family:fb;'> &#xf09b; </span><b>stesiam</b>, 2023")


## Create visualization

plot = clean_data %>%
  ggplot(., mapping = aes(x = year, y = average, color = stock_symbol, group = stock_symbol)) +
  geom_line(size = 2) +
  geom_point(
    data = clean_data %>% filter(year == 2022), size = 8.5, color = "grey95") +
  scale_color_manual(values = c("#ff9900", "#4285f4")) +
  geom_image( data = clean_data %>% filter(year == 2022), aes(x=year,y=average,image=image, color = NULL), asp=1.2)+
  
  # Plot title 
  
  geom_richtext(aes(x = 2010, y = 135,
                    label = title),
                fill = NA, label.color = NA,
                fontface="bold",family="serif",
                color = "white",size = 8, hjust = 0
  ) +
  
  # Plot subtitle
  
  geom_richtext(aes(x = 2010, y =75,
                    label = subtitle),
                fill = NA, label.color = NA
                ,family="serif",
                lineheight = 1.3,
                color = "white",size = 4, hjust = 0
  ) +
  
  # Plot caption
  
  geom_richtext(aes(x = 2017.2, y = 20,
                    label = caption),
                fill = NA, label.color = NA
                ,family="serif",
                lineheight = 1.6,
                color = "white",size = 2.6, hjust = 0
  ) +
  
  labs(
    x = "Date",
    y = "Adjusted closing price",
    color = "Company"
  ) +
  theme_minimal(
    base_size = 10,
    base_family = "clim"
  ) +
  scale_y_continuous(
    # Primary axis labels
    labels = function(x) paste0("$", round(x, 0)),  # Primary y-axis with dollar sign
    # Define the secondary axis
    sec.axis = sec_axis(~ ., 
                        name = "Secondary Y-Axis ($)", 
                        labels = function(x) paste0("$", round(x, 0)))  # Secondary y-axis with dollar sign
  ) +
  scale_x_continuous(breaks = seq(2010, 2022, 4)) +
  theme(
    plot.caption = element_markdown(family = "uc", lineheight = 0.5,
                                    color = "white", size = 4),
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text = element_text(color = "#ffffff"),
    axis.text.y.right = element_text(color = "#ffffff"),
    axis.text.y.left = element_blank(),
    axis.ticks.y.right = element_line(size = 1, color = "#ffffff"),
    axis.ticks.x.bottom = element_line(size = 1, color = "#ffffff")
  )

## Export it



ggsave(
  filename = "2023/w06/w06-2023-tt-el.png",
  plot = plot,
  device = "png",
  width = 6,
  height = 4)
