library(tidyverse)
library(vroom)
library(tidymodels)
library(DataExplorer)
library(lubridate)
library(slider)        # for rolling features
set.seed(123)

## Read in the Data
train <- vroom("./train.csv")
test  <- vroom("./test.csv")
features <- vroom("./features.csv")

#########
## EDA ##
#########
plot_missing(features)
plot_missing(test)

### Impute Missing Markdowns (same as before)
features <- features %>%
  mutate(across(starts_with("MarkDown"), ~ replace_na(., 0))) %>%
  mutate(across(starts_with("MarkDown"), ~ pmax(., 0))) %>%
  mutate(
    MarkDown_Total = rowSums(across(starts_with("MarkDown")), na.rm = TRUE),
    MarkDown_Flag  = if_else(MarkDown_Total > 0, 1, 0),
    MarkDown_Log   = log1p(MarkDown_Total)
  ) %>%
  select(-MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5)

## Impute Missing CPI and Unemployment
feature_recipe <- recipe(~., data=features) %>%
  step_mutate(DecDate = decimal_date(Date)) %>%
  step_impute_bag(CPI, Unemployment, impute_with = imp_vars(DecDate, Store))

imputed_features <- juice(prep(feature_recipe))

########################
## Merge the Datasets ##
########################

# NOTE: Do NOT drop MarkDown_Total here (we'll keep it)
fullTrain <- left_join(train, imputed_features, by = c("Store", "Date")) %>%
  select(-IsHoliday.y) %>%
  rename(IsHoliday = IsHoliday.x) %>%
  arrange(Store, Dept, Date)

fullTest <- left_join(test, imputed_features, by = c("Store", "Date")) %>%
  select(-IsHoliday.y) %>%
  rename(IsHoliday = IsHoliday.x) %>%
  arrange(Store, Dept, Date)

plot_missing(fullTrain)
plot_missing(fullTest)

########################################
## Build rolling features for all rows
## (so test rows can inherit historic rolling stats)
########################################

# combine train and test into one timeline per Store/Dept so rolling uses only past Weekly_Sales
# set Weekly_Sales=NA for test rows
train_comb <- fullTrain %>%
  select(Store, Dept, Date, Weekly_Sales, MarkDown_Total, IsHoliday)

test_comb <- fullTest %>%
  select(Store, Dept, Date, MarkDown_Total, IsHoliday) %>%
  mutate(Weekly_Sales = NA_real_)

combined <- bind_rows(train_comb, test_comb) %>%
  arrange(Store, Dept, Date) %>%
  group_by(Store, Dept) %>%
  mutate(
    # rolling features using previous 4 observations (weeks). .before=4 uses current + prior 4, so shift with lag to avoid using current.
    roll_mean_4 = slide_dbl(lag(Weekly_Sales), ~ if(all(is.na(.x)) ) NA_real_ else mean(.x, na.rm = TRUE), .before = 3, .complete = FALSE),
    roll_sd_4   = slide_dbl(lag(Weekly_Sales), ~ if(all(is.na(.x)) ) NA_real_ else sd(.x, na.rm = TRUE), .before = 3, .complete = FALSE)
  ) %>%
  ungroup()

# split back into enriched train and test
combined_train <- combined %>% filter(!is.na(Weekly_Sales))
combined_test  <- combined %>% filter(is.na(Weekly_Sales)) %>% select(-Weekly_Sales)

# join rolling features back to fullTrain/fullTest by Store, Dept, Date
fullTrain <- fullTrain %>%
  left_join(combined_train %>% select(Store, Dept, Date, roll_mean_4, roll_sd_4),
            by = c("Store", "Dept", "Date"))

fullTest <- fullTest %>%
  left_join(combined_test %>% select(Store, Dept, Date, roll_mean_4, roll_sd_4),
            by = c("Store", "Dept", "Date"))

##################################
## Loop Through the Store-depts ##
##################################
all_preds <- tibble(Id = character(), Weekly_Sales = numeric())
n_storeDepts <- fullTest %>% distinct(Store, Dept) %>% nrow()
cntr <- 0

# define model spec outside loop (can tune later)
my_model <- rand_forest(mtry = 3, trees = 500, min_n = 5) %>%
  set_engine("ranger") %>%
  set_mode("regression")

stores <- unique(fullTest$Store)
for (store in stores) {
  store_train <- fullTrain %>% filter(Store == store)
  store_test  <- fullTest  %>% filter(Store == store)
  
  depts <- unique(store_test$Dept)
  for (dept in depts) {
    
    dept_train <- store_train %>% filter(Dept == dept)
    dept_test  <- store_test  %>% filter(Dept == dept)
    
    ## If Statements for data scenarios
    if (nrow(dept_train) == 0) {
      
      ## Predict 0
      preds <- dept_test %>%
        transmute(Id = paste(Store, Dept, Date, sep = "_"),
                  Weekly_Sales = 0)
      
    } else if (nrow(dept_train) < 10 && nrow(dept_train) > 0) {
      
      ## Predict the mean (robust to NA)
      mu <- mean(dept_train$Weekly_Sales, na.rm = TRUE)
      preds <- dept_test %>%
        transmute(Id = paste(Store, Dept, Date, sep = "_"),
                  Weekly_Sales = mu)
      
    } else {
      
      ## Prepare recipe with cyclical features, markdown total, holiday numeric, rolling features, and normalization
      my_recipe <- recipe(Weekly_Sales ~ ., data = dept_train) %>%
        step_mutate(Holiday = as.integer(IsHoliday)) %>%
        step_date(Date, features = c("doy")) %>%
        step_mutate(
          sinDOY = sin(2 * pi * Date_doy / 365),
          cosDOY = cos(2 * pi * Date_doy / 365)
        ) %>%
        # keep MarkDown_Total, MarkDown_Flag and rolling features (roll_mean_4, roll_sd_4)
        step_rm(Date, Store, Dept, IsHoliday) %>%
        step_zv(all_predictors()) %>%
        step_normalize(all_numeric_predictors())
      
      # workflow
      my_wf <- workflow() %>%
        add_recipe(my_recipe) %>%
        add_model(my_model) %>%
        fit(dept_train)
      
      # predict
      preds <- dept_test %>%
        transmute(Id = paste(Store, Dept, Date, sep = "_"),
                  Weekly_Sales = predict(my_wf, new_data = .) %>% pull(.pred))
    }
    
    ## Bind predictions together
    all_preds <- bind_rows(all_preds, preds)
    
    ## Print out Progress
    cntr <- cntr + 1
    cat("Store", store, "Department", dept, "Completed.",
        round(100 * cntr / n_storeDepts, 1), "% overall complete.\n")
    
  } # end dept
} # end store

## Write out
vroom_write(x = all_preds, file = paste0("./Predictions.csv"), delim = ",")



