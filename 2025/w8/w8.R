article_dat <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-25/article_dat.csv')
model_dat <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-25/model_dat.csv')


article_dat |>
  dplyr::select(study_type) |>
  mutate(study_type = fct_recode(study_type,
                                 "Cross-sectional" = "Cross-Sectional",
                                 "Prospective cohort" = "Prospective Cohort",
                                 "Retrospective cohort" = "Retrospective Cohort"
  )
) |>
  count(study_type) |>
  drop_na(study_type) |>
  mutate(pct = round(n/sum(n) * 100, digits = 1))


article_dat |>
  select(year,) |>
  count()
  
  
