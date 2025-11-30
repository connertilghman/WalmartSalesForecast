library(tidyverse)
library(vroom)
library(tidymodels)
library(DataExplorer)
library(prophet)
library(patchwork)

## Read in the Data
train <- vroom("./train.csv")
test <- vroom("./test.csv")
features <- vroom("./features.csv")

#########
## EDA ##
#########
plot_missing(features)
plot_missing(test)

### Impute Missing Markdowns
features <- features %>%
  mutate(across(starts_with("MarkDown"), ~ replace_na(., 0))) %>%
  mutate(across(starts_with("MarkDown"), ~ pmax(., 0))) %>%
  mutate(
    MarkDown_Total = rowSums(across(starts_with("MarkDown")), na.rm = TRUE),
    MarkDown_Flag = if_else(MarkDown_Total > 0, 1, 0),
    MarkDown_Log   = log1p(MarkDown_Total)
  ) %>%
  select(-MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5)

## Impute Missing CPI and Unemployment
feature_recipe <- recipe(~., data=features) %>%
  step_mutate(DecDate = decimal_date(Date)) %>%
  step_impute_bag(CPI, Unemployment,
                  impute_with = imp_vars(DecDate, Store))
imputed_features <- juice(prep(feature_recipe))

full_data <- train |>
  left_join(imputed_features, by = c("Store", "Date")) |>
  select(-IsHoliday.y) |>
  rename(IsHoliday = IsHoliday.x) |>
  arrange(Store, Dept, Date)

full_test <- test |>
  left_join(imputed_features, by = c("Store", "Date")) |>
  select(-IsHoliday.y) |>
  rename(IsHoliday = IsHoliday.x) |>
  arrange(Store, Dept, Date)

season_recipe <- recipe(Weekly_Sales ~ ., data = full_data) |>
  update_role(Store, Dept, Date, IsHoliday, MarkDown_Flag, new_role = "predictor") |>
  step_rm(IsHoliday) |>
  step_date(Date, features = "doy") |>
  step_rm(Date) |>
  step_range(Date_doy, min = 0, max = pi) |>
  step_mutate(
    sinDOY = sin(Date_doy),
    cosDOY = cos(Date_doy)
  ) |>
  step_zv(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors())

store1 <- 10
dept1 <- 12

sd_train1 <- full_data |>
  filter(Store==store1, Dept==dept1) |>
  rename(y=Weekly_Sales, ds=Date)


sd_train1 <- sd_train1 %>%
  mutate(
    # Day-of-year cyclic features
    doy = yday(ds),
    sinDOY = sin(2 * pi * doy / 365),
    cosDOY = cos(2 * pi * doy / 365),
  
    # Markdown flag or total
    Markdown_Total = if_else(is.na(MarkDown_Total), 0, MarkDown_Total),
    
    # Holiday as numeric
    Holiday_Num = if_else(IsHoliday, 1, 0)
  ) 

sd_test1 <- full_test |>
  filter(Store==store1, Dept==dept1) |>
  rename(ds=Date)

sd_test1 <- sd_test1 %>%
  mutate(
    doy = yday(ds),
    sinDOY = sin(2 * pi * doy / 365),
    cosDOY = cos(2 * pi * doy / 365),
    Markdown_Total = if_else(is.na(MarkDown_Total), 0, MarkDown_Total),
    Holiday_Num = if_else(IsHoliday, 1, 0)
  )


store2 <- 8
dept2 <- 6

sd_train2 <- full_data |>
  filter(Store==store2, Dept==dept2) |>
  rename(y=Weekly_Sales, ds=Date)


sd_train2 <- sd_train2 %>%
  mutate(
    # Day-of-year cyclic features
    doy = yday(ds),
    sinDOY = sin(2 * pi * doy / 365),
    cosDOY = cos(2 * pi * doy / 365),
    
    # Markdown flag or total
    Markdown_Total = if_else(is.na(MarkDown_Total), 0, MarkDown_Total),
    
    # Holiday as numeric
    Holiday_Num = if_else(IsHoliday, 1, 0)
  ) 

sd_test2 <- full_test |>
  filter(Store==store2, Dept==dept2) |>
  rename(ds=Date)

sd_test2 <- sd_test2 %>%
  mutate(
    doy = yday(ds),
    sinDOY = sin(2 * pi * doy / 365),
    cosDOY = cos(2 * pi * doy / 365),
    Markdown_Total = if_else(is.na(MarkDown_Total), 0, MarkDown_Total),
    Holiday_Num = if_else(IsHoliday, 1, 0)
  )

prophet_model1 <- prophet() |>
  add_regressor("Markdown_Total") |>
  add_regressor("sinDOY") |>
  add_regressor("cosDOY") |>
  add_regressor("Holiday_Num") |>
  fit.prophet(df=sd_train1)

fitted_vals1 <- predict(prophet_model1, df=sd_train1) 
test_preds1 <- predict(prophet_model1, df=sd_test1)


prophet_model2 <- prophet() |>
  add_regressor("Markdown_Total") |>
  add_regressor("sinDOY") |>
  add_regressor("cosDOY") |>
  add_regressor("Holiday_Num") |>
  fit.prophet(df=sd_train2)

fitted_vals2 <- predict(prophet_model2, df=sd_train2) 
test_preds2 <- predict(prophet_model2, df=sd_test2)

plot1 <- ggplot() +
geom_line(data = sd_train1, mapping = aes(x = ds, y = y, color = "Data")) +
geom_line(data = fitted_vals1, mapping = aes(x = as.Date(ds), y = yhat, color = "Fitted")) +
geom_line(data = test_preds1, mapping = aes(x = as.Date(ds), y = yhat, color = "Forecast")) +
scale_color_manual(values = c("Data" = "black", "Fitted" = "blue", "Forecast" = "red")) +
labs(color="", title = "Store = 10, Dept = 12")

plot2 <- ggplot() +
  geom_line(data = sd_train2, mapping = aes(x = ds, y = y, color = "Data")) +
  geom_line(data = fitted_vals2, mapping = aes(x = as.Date(ds), y = yhat, color = "Fitted")) +
  geom_line(data = test_preds2, mapping = aes(x = as.Date(ds), y = yhat, color = "Forecast")) +
  scale_color_manual(values = c("Data" = "black", "Fitted" = "blue", "Forecast" = "red")) +
  labs(color="", title = "Store = 8, Dept = 6")

combo_plot <- plot1/plot2

ggsave("two_timeseries.pdf", plot = combo_plot, width = 8, height = 6)
# enet_spec <- linear_reg(
#   penalty = tune(),   # λ
#   mixture = tune()    # α (0=ridge, 1=lasso)
# ) |>
#   set_engine("glmnet")
# 
# rf_spec <- rand_forest(
#   mtry  = tune(),
#   min_n = tune(),
#   trees = 500
# ) |>
#   set_engine("ranger") |>
#   set_mode("regression")
# 
# knn_spec <- nearest_neighbor(
#   neighbors   = tune(),
#   weight_func = tune(),   # uniform, distance
#   dist_power  = tune()    # Minkowski p
# ) |>
#   set_engine("kknn") |>
#   set_mode("regression")
# 
# enet_wf <- workflow() %>%
#   add_model(rf_spec) %>%
#   add_recipe(season_recipe)
# 
# folds <- vfold_cv(full_data, v = 5)
# 
# enet_tuned <- enet_wf %>%
#   tune_grid(
#     resamples = folds,
#     grid = 10,
#     metrics = metric_set(rmse)
#   )
# 
# best_enet <- select_best(enet_tuned, metric = "rmse")
# 
# cv_results <- collect_metrics(enet_tuned)
# cv_results %>% filter(.metric == "rmse")

# df$Date <- ymd(df$Date)
# 
# glimpse(df)
# 
# 
# summary(df)
# 
# # Number of stores and departments
# df %>% summarise(
#   n_stores = n_distinct(Store),
#   n_depts = n_distinct(Dept)
# )
# 
# # Plot weekly sales for Store 1 Dept 1
# store1_dept1 <- df %>%
#   filter(Store == 1, Dept == 1) %>%
#   arrange(Date)
# 
# ggplot(store1_dept1, aes(Date, Weekly_Sales)) +
#   geom_line() +
#   labs(title = "Store 1 Dept 1 Weekly Sales Over Time",
#        x = "Date", y = "Weekly Sales")
# 
# # Compare holiday vs non-holiday sales
# df %>%
#   group_by(IsHoliday) %>%
#   summarise(
#     mean_sales = mean(Weekly_Sales),
#     median_sales = median(Weekly_Sales),
#     n = n()
#   )
# 
# # Example: Compare two stores’ time series
# different_sales <- ggplot(df %>% filter(Dept == 1, Store %in% c(1, 5)),
#        aes(Date, Weekly_Sales, color = factor(Store))) +
#   geom_line() +
#   labs(title = "Different Sales Patterns Between Stores",
#        color = "Store")
# 
# 
# # Check for negative or zero sales
# df %>% filter(Weekly_Sales <= 0)
# 
# # Histogram of weekly sales
# ggplot(df, aes(Weekly_Sales)) +
#   geom_histogram(bins = 50) +
#   labs(title = "Distribution of Weekly Sales")
# 
# df %>% summarise_all(~sum(is.na(.)))
# 
# ggsave("salescomparison.png", plot = different_sales, width = 8, height = 5, dpi = 300)
