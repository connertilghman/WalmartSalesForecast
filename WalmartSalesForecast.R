library(tidyverse)
library(lubridate)
library(ggplot2)

#--------------------------------------------------
# 1. Read the data
#--------------------------------------------------

features <- read.csv("features.csv")
df <- read.csv("train.csv")

df$Date <- ymd(df$Date)

glimpse(df)

#--------------------------------------------------
# 2. Basic summaries
#--------------------------------------------------

summary(df)

# Number of stores and departments
df %>% summarise(
  n_stores = n_distinct(Store),
  n_depts = n_distinct(Dept)
)

#--------------------------------------------------
# 3. Issue 1: Seasonality & Holiday Effects
#--------------------------------------------------

# Plot weekly sales for Store 1 Dept 1
store1_dept1 <- df %>%
  filter(Store == 1, Dept == 1) %>%
  arrange(Date)

ggplot(store1_dept1, aes(Date, Weekly_Sales)) +
  geom_line() +
  labs(title = "Store 1 Dept 1 Weekly Sales Over Time",
       x = "Date", y = "Weekly Sales")

# Compare holiday vs non-holiday sales
df %>%
  group_by(IsHoliday) %>%
  summarise(
    mean_sales = mean(Weekly_Sales),
    median_sales = median(Weekly_Sales),
    n = n()
  )

#--------------------------------------------------
# 4. Issue 2: Large Differences Across Stores/Departments
#--------------------------------------------------

# Boxplot of sales by store
ggplot(df, aes(factor(Store), Weekly_Sales)) +
  geom_boxplot() +
  labs(title = "Sales Variability Across Stores", x = "Store", y = "Weekly Sales") +
  theme(axis.text.x = element_text(angle = 90))

# Example: Compare two stores’ time series
different_sales <- ggplot(df %>% filter(Dept == 1, Store %in% c(1, 5)),
       aes(Date, Weekly_Sales, color = factor(Store))) +
  geom_line() +
  labs(title = "Different Sales Patterns Between Stores",
       color = "Store")

#--------------------------------------------------
# 5. Issue 3: Outliers & Unusual Values
#--------------------------------------------------

# Check for negative or zero sales
df %>% filter(Weekly_Sales <= 0)

# Histogram of weekly sales
ggplot(df, aes(Weekly_Sales)) +
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Weekly Sales")

#--------------------------------------------------
# 6. Missing values
#--------------------------------------------------

df %>% summarise_all(~sum(is.na(.)))

ggsave("salescomparison.png", plot = different_sales, width = 8, height = 5, dpi = 300)
