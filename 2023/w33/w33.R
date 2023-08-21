library(readr)
library(dplyr)

library(ggplot2)
library(ggtext)

library(extrafont) ## Better alt / use on next viz -> showtext + sysfonts
library(glue)

library(tidymodels)
library(bonsai)
library(glmnet)

set.seed(234)

spam <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-15/spam.csv')

spam$yesno = as.factor(spam$yesno)

spam$yesno = relevel(spam$yesno, ref = "y")

spam_split <- initial_split(spam,
                                prop = 0.75,
                                strata = yesno)

spam_train <- spam_split %>%
  training()

spam_test <- spam_split %>%
  testing()

val_set <- folds <- vfold_cv(spam_train, v = 10,strata = yesno)

## Build Models

lr_mod <- 
  logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

lgb_mod_default =  boost_tree() %>% 
  set_engine("lightgbm") %>% 
  set_mode("classification") 

lgb_mod_tune <- boost_tree(tree_depth = tune(),
                           min_n = tune(), 
                           trees = tune()) %>% 
  set_engine("lightgbm") %>% 
  set_mode("classification")


## Build Recipes

lr_recipe <- 
  recipe(yesno ~ ., data = spam_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

lr_recipe1 <- 
  recipe(yesno ~bang+make, data = spam_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())


# LightGBM does not require transformation of factors to dummy vars

lgb_recipe <-
  recipe(yesno ~., data = spam_train) %>%
  recipes::step_other(all_nominal(), threshold = 0.01) %>%
  recipes::step_nzv(all_nominal()) %>%
  prep()

## See result after recipe
recipes::bake(lgb_recipe, spam_train)

lr_workflow1 <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(lr_recipe1)

lr_workflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(lr_recipe)

lgb_workflow_default <- 
  workflow() %>% 
  add_model(lgb_mod_default) %>% 
  add_recipe(lgb_recipe)

lgb_workflow_tuned <- 
  workflow() %>% 
  add_model(lgb_mod_tune) %>% 
  add_recipe(lgb_recipe)

## create grid for Logistic Regression

lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

lr_reg_grid %>% top_n(-5) # lowest penalty values

tree_grid <- grid_regular(min_n(),
                          tree_depth(),
                          trees(),
                          levels = 4)


## Grid for LightGBM



start_time_lr <- Sys.time()
lr_res <- 
  lr_workflow %>% 
  tune_grid(resamples = val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))
end_time_lr <- Sys.time()

lr_res1 <- 
  lr_workflow1 %>% 
  tune_grid(resamples = val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))
start_time_lgb_default <- Sys.time()
lgb_res_default <- 
  lgb_workflow_default %>%
  tune_grid(resamples = val_set,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))
end_time_lgb_default <- Sys.time()


start_time_lgb_tuned<- Sys.time()
lgb_res_tune <- 
  lgb_workflow_tuned %>%
  tune_grid(resamples = val_set,
            grid = tree_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))
end_time_lgb_tuned <- Sys.time()

lr_best <- 
  lr_res %>% 
  collect_metrics() %>% 
  arrange(penalty) %>% 
  slice_max(mean)

lr_best1 <- 
  lr_res1 %>% 
  collect_metrics() %>% 
  arrange(penalty) %>% 
  slice_max(mean)

lgb_best_default <- 
  lgb_res_default %>% 
  collect_metrics()

lgb_best_tune <- 
  lgb_res_tune %>% 
  tune::show_best(metric = "roc_auc")

lr_auc <- 
  lr_res %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(yesno, .pred_y) %>% 
  mutate(model = "Logistic Regression (all)")

lr_auc1 <- 
  lr_res1 %>% 
  collect_predictions(parameters = lr_best1) %>% 
  roc_curve(yesno, .pred_y) %>% 
  mutate(model = "Logistic Regression (2 vars)")


lgb_auc_default <- 
  lgb_res_default %>% 
  collect_predictions(parameters = lgb_best_default) %>% 
  roc_curve(yesno, .pred_y) %>% 
  mutate(model = "LightGBM (default)")

lgb_auc_tune <- 
  lgb_res_tune %>% 
  collect_predictions(parameters = lgb_best_tune) %>% 
  roc_curve(yesno, .pred_y) %>% 
  mutate(model = "LightGBM (tuned)")

plot = bind_rows(lr_auc, lr_auc1, lgb_auc_default, lgb_auc_tune) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) + 
  geom_path(lwd = 1.2, alpha = 0.8) +
  geom_abline(lty = 2) + 
  coord_equal() + 
  scale_color_manual(
    values = c(
      "Logistic Regression (2 vars)" = "grey",
      "Logistic Regression (all)" = "purple",
      "LightGBM (default)" = "red",
      "LightGBM (tuned)" = "dodgerblue"
    )
  ) +
  theme_classic(base_size = 10) +
  labs(
    title  = "<b>Model Comparison on Spam Detection</b>",
    subtitle = glue("On this week's TidyTuesday dataset I am analyzing spam messages.
                    Based on <br>parameters (e.g. proportion of exclamation marks, dollar signs etc.)
                    I built <br>several classification models to predict if a certain message is a spam or not.<br>
                    It is apparent that <span style = 'color: dodgerblue; font-weight:bold;'>LightGBM</span>  models (ROC = {round(lgb_best_tune$mean,digits=2)}) outperformed
                    <span style = 'color: purple; font-weight:bold;'>Logistic</span> <br>ones (ROC = {round(lr_best$mean,digits = 3)})."),
    caption = "stesiam, 2023"
  ) +
  theme(
    plot.title = element_markdown(family = "Amiri", size = 12, hjust = 0),
    plot.subtitle = element_markdown(family = "EB Garamond",
                                     size = 8, hjust = 0),
    plot.caption = element_markdown(family = "EB Garamond", size = 7),
    text = element_text(family = "Amiri"),
    legend.position = c(0.8,0.25),
    legend.title = element_blank(),
    legend.background = element_rect(fill = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )

ggsave(
  filename = "2023/w33/w33-2023-tt.png",
  plot = plot,
  device = "png",
  width = 4
  )

# Saving 4 x 3.75 in image