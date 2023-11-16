##Estimere corruption rate

# Load required libraries
library(tidyverse)
library(tidymodels)
library(dplyr)

corruption_data <- read_csv("c2a74590-6dc3-406c-a3cb-4bec648e36b5_Series - Metadata.csv")
corruption.data <- corruption_data[-c(215:221), ]  ##Removing last rows with text info

corruption.data <- corruption.data%>%
  dplyr::rename(ISO3=`Country Code`, Country=`Country Name`)


##The columns for the years go from 3:26 in the datset
colnames(corruption.data)[c(1, 3:28)] <- gsub("\\[YR.*\\]", "", colnames(corruption.data)[c(1, 3:28)])

corruption.data <- corruption.data%>%
  select(Country,ISO3,c(1,5:28))


##Change layout on dataset
corruption.data <- pivot_longer(corruption.data, cols = -c(Country, ISO3), names_to = "Year", values_to = "corrupt")

corruption.data <- corruption.data %>%
  mutate(corrupt = ifelse(corrupt == "..", NA, corrupt)) %>%
  na.omit()

corruption.data$Year <- as.numeric(corruption.data$Year)
corruption.data$corrupt <- as.numeric(corruption.data$corrupt)

# Machine learning

library(tidyverse)
library(glmnet)

forcast_data <- corruption.data%>%
  select(Country,Year,corrupt)

 # Setting seed

set.seed(123456)


# create a list to store the predicted corruption values for each country
predictions_list <- list()

##########################################
## Tuning the model to find optimal lambda
##########################################

dt_fold <- vfold_cv(dt_train, v = 10)
dt_fold$splits[[1]] %>% analysis()

lasso_mod <- linear_reg(mixture = 1, penalty = .1) %>% 
  set_engine("glmnet")

 #Make recipe with the vector wanting to estimate (corrupt)
 # make a nomial as dummies fit in the estimation calculations 

rec <- recipe(corrupt~.,data=dt_train)%>%
  step_dummy(all_nominal())

 # Create different train-test splits on the data for cross-validation, and fits the Lasso model on each split

lasso_cv <- fit_resamples(lasso_mod, rec, dt_fold)

 # Collect the metrics of the lasso model on the different resamples, by returning the data set with metrics calculated for each resample.

collect_metrics(lasso_cv)

 # Then specifies a linear regression model with penalty I am about to calculate with model training
 # Set mixture= 1, to refer to a lasso model

tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")



 # Set up a workflow
wf_tune <- workflow() %>%
  add_recipe(rec)

# Run the tuning
lasso_grid <- tune_grid(
  wf_tune %>% add_model(tune_spec),
  resamples = dt_fold)


lasso_grid %>%
  collect_metrics()

 # Select best rms

lasso_grid %>% select_best('rmse')

#####################################
##Other method to find optimal lambda
#####################################

# Set up a grid of lambda values to search through
lambda_grid <- grid_regular(penalty(), levels = 50)
lasso_grid <- tune_grid(
  wf_tune %>% add_model(tune_spec),
  resamples = dt_fold,
  grid = lambda_grid)

# Pick the best value
lasso_grid %>% select_best('rmse')

 # Got a very small penalty= 0.0000000001, that can indicate that 
 # Am i over fitting the training data? should not with these steps

optimal_lambda <- 0.0000000001




#################################
## Estimating the corruption rate
#################################

# Split the data into training and testing sets, splitting it by date to fit better

split_date <- 2018
dt_train <- country_data%>%
  filter(Year<split_date)
dt_test <- country_data%>%
  filter(Year>=split_date)

 # Creat a loop to make estimates for each country

unique_countries <- unique(forcast_data$Country)
for (Country in unique_countries) {
  
  # Subset the data for the current country
  country_data <- forcast_data %>% 
    mutate_if(is.character, as.factor)
  
  # Fit ols model
  ols_fit <- linear_reg()%>%
  set_engine("lm")%>%
  fit(corrupt~.,data=dt_train)

 # Fit lass
  lasso_wf <- workflow()%>%
  add_recipe(recipe(corrupt~.,data=dt_train))
  
  lasso_spec <- linear_reg(mixture = 1, penalty = optimal_lambda) %>%
    set_engine("glmnet")
  
  # Fit the model
  lasso_fit <- workflow() %>%
    add_recipe(rec) %>%
    add_model(lasso_spec) %>%
    fit(data = dt_train)

# Make predictions 
  ols_pred <- predict(ols_fit,dt_test) %>% 
  dplyr::rename(ols_pred = .pred)
  
  lasso_pred <- predict(lasso_fit,dt_test) %>% 
  dplyr::rename(lasso_pred = .pred)
  
# Store the predictions for the current country
  predictions <- bind_cols(dt_test, ols_pred, lasso_pred)
  predictions_list[[Country]] <- predictions

}

view(predictions_list)
view(predictions_list[[Country]])

 # Rename the data set

corruption_pred<- predictions_list[[Country]]
view(corruption_pred)






