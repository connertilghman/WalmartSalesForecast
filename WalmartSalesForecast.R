library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)


features <- read.csv("features.csv")
df <- read.csv("train.csv")
test <- read.csv("test.csv")

features[is.na(features)] <- 0

features$Total <- features$MarkDown1 + features$MarkDown2 + features$MarkDown3 +
  features$MarkDown4 + features$MarkDown5

features$flag <- ifelse(features$Total == 0, 0, 1)

features <- features |>
  select(-MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5)

fulldata <- left_join(df,features, by=c("Store", "Date"))
fulltest <- left_join(test,features, by=c("Store", "Date"))

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
