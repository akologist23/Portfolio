library(tidyverse)
library(tidymodels)
library(vip)
library(rpart.plot)
library(xgboost)
library(tictoc)
library(ranger)


#Read data in
#setwd
getwd()
hist <- read.csv("C:/Users/Alicia/Documents/2021/JobSearch2021/LexisNexis/flight_history.csv",
                 header=TRUE, stringsAsFactors = FALSE, na.strings = c("NA",NA, NULL, "","."))

test <- read.csv("C:/Users/Alicia/Documents/2021/JobSearch2021/LexisNexis/flight_test.csv",
                 header=TRUE, stringsAsFactors = FALSE, na.strings = c("NA",NA, NULL, "","."))


##creating 3 variables in hist
hist$lateflight <- ifelse(hist$arr_delay >0, 1, 0)
hist$latedepart <- ifelse(hist$dep_delay >0, 1, 0)

#Constructing variables for departure delay

#1 average delay by day
dep_delay_avg_day <- hist %>% 
  group_by(day) %>% 
  summarise(dep_delay_avg_day = mean(dep_delay, na.rm= TRUE))

hist <- hist %>% 
  left_join(dep_delay_avg_day, by=c("day"))

#2 percent of flights with departure delayed by carrier

perc_dep_delay_carr <-  hist %>% 
  group_by(carrier) %>% 
  summarise(perc_dep_delay_carr = sum(latedepart, na.rm = TRUE)/length(latedepart))

hist<- hist %>% 
  left_join(perc_dep_delay_carr)

#3 percent of flights with delay departure by origin

perc_dep_delay_orig <-  hist %>% 
  group_by(origin) %>% 
  summarise(perc_dep_delay_orig = sum(latedepart, na.rm = TRUE)/
              length(latedepart))

hist<- hist %>% 
  left_join(perc_dep_delay_orig)

#Constructing variables for arrival delay

#1 Average arrival delay by destination
arr_delay_avg_dest <- hist %>% 
  group_by(dest) %>% 
  summarise(arr_delay_avg_dest = mean(arr_delay, na.rm= TRUE))

hist <- hist %>% 
  left_join(arr_delay_avg_dest, by=c("dest"))

#2: Percent of all flights with delayed arrival by destination
perc_arr_delay_dest <-  hist %>% 
  group_by(dest) %>% 
  summarise(perc_arr_delay_dest = sum(lateflight, na.rm = TRUE)/
              length(lateflight))

hist<- hist %>% 
  left_join(perc_arr_delay_dest)

#3: Percent of flights with delayed arrival by carrier
perc_arr_delay_carr <-  hist %>% 
  group_by(carrier) %>% 
  summarise(perc_arr_delay_carr = sum(lateflight, na.rm = TRUE)/length(lateflight))

hist<- hist %>% 
  left_join(perc_arr_delay_carr)

#Constructing air-time derived variables

#1: Variance of flight air_time by destination
air_time_var_path <- hist %>% 
  group_by(origin, dest) %>% 
  summarise(air_time_var_path = var(air_time, na.rm= TRUE))

hist <- hist %>% 
  left_join(air_time_var_path, by=c("origin","dest"))


##Creating variables in test data

#DEPARTURE DELAY
#Var1: Average departure delay by day
test <- test %>% 
  left_join(dep_delay_avg_day, by=c("day"))

#Var2: Percent of all flights for carrier that have departure delayed
test <- test %>% 
  left_join(perc_dep_delay_carr)

#Var3: Percent departure delay by origin
test <- test %>% 
  left_join(perc_dep_delay_orig)

#ARRIVAL DELAY

#1:Average arrival delay by destination
test <- test %>% 
  left_join(arr_delay_avg_dest, by=c("dest"))

#2: Percent of all flights with delayed arrival by destination
test<- test %>% 
  left_join(perc_arr_delay_dest)

#3: Percent of flights with delayed arrival by carrier
test <- test %>% 
  left_join(perc_arr_delay_carr)

#Air-time
#1: Variance of flight air_time by destination
test <- test %>% 
  left_join(air_time_var_path, by=c("origin","dest"))

#Distance
distance <- hist %>% 
  distinct(origin, dest,distance)

test <- test %>% 
  left_join(distance, by=c("origin","dest"))

##Remove missing values

#test
test <- na.omit(test)

#hist
hist <- na.omit(hist)

#recipe prep - remove nonvariable columns
hist_var <- hist %>%  select(-dep_time,-arr_time,-dep_delay,-arr_delay,
                             -air_time, -hour, -time_hour, -flight, 
                             -tailnum,-year,-latedepart)

test_var <- test %>%  select(-hour, -time_hour, -flight, -tailnum, 
                             -uniqueid, -year)

#Add id variable to hist from test
#uniqueid <- 1:nrow(hist_var)
#hist_var$uniqueid <- uniqueid

#make outcome variable a factor in hist
hist_var$lateflight <- as.factor(hist_var$lateflight)
levels(hist_var$lateflight) <- c("No","Yes")
hist_var$lateflight <- ordered(hist_var$lateflight, c("Yes","No"))
levels(hist_var$lateflight)

#make outcome variable a factor in test
test_var$lateflight <- as.factor(test_var$lateflight)
levels(test_var$lateflight) <- c("No","Yes")
test_var$lateflight <- ordered(test_var$lateflight, c("Yes","No"))
levels(test_var$lateflight)

#check multicollinearity
hist %>% 
  select_if(is.numeric) %>% 
  cor()

#Apply transformations on numeric variables

#check numeric data distributions
#hist(hist$distance)
hist(log10(hist_var$distance)) #log10 transformation
hist(hist_var$dep_delay_avg_day)
hist(hist_var$perc_dep_delay_carr)
hist(hist_var$perc_dep_delay_orig)
hist(hist_var$arr_delay_avg_dest)
#hist(hist_var$perc_arr_delay_dest)
hist(log10(hist_var$perc_arr_delay_dest)) #log10 transf
hist(hist_var$perc_arr_delay_carr)
hist(sqrt(hist_var$air_time_var_path)) #sqrt transformation

#Prep and back on hist_var
hist_recipe<-recipe(lateflight ~ .,
                          data = hist_var) %>% 
  step_log(distance,perc_arr_delay_dest,base = 10) %>%
  step_sqrt(air_time_var_path) %>% 
  step_corr(all_numeric(), threshold = 0.90) %>% 
  step_dummy(all_nominal(), -all_outcomes()) 

hist_processed <- hist_recipe %>%
  prep() %>% 
  bake(new_data = hist_var) #switch to test to apply

test_processed <- hist_recipe %>%
  prep() %>% 
  bake(new_data = test_var) #switch to test to apply

#model create folds to tune hyperparameters

hist_folds <- vfold_cv(hist_processed, v = 3)

#Random Forest
rf_model_tune <- rand_forest(mtry = 20,
                            trees = 500,
                            min_n = tune()) %>% 
  set_engine('ranger') %>% 
  set_mode('classification')

set.seed(86)
rf_grid <- grid_random(parameters(min_n() ),
                       size=5)

#rf_grid <- grid_random(parameters(mtry() %>% range_set(range=c(10,30)),
#                                  min_n() ),
#                                  size=5)

tic(quiet=FALSE)
rf_tuning <- tune_grid(rf_model_tune,
                       lateflight ~ .,
                       resamples = hist_folds,
                       rf_grid,
                       metrics = metric_set(roc_auc))
toc(log=TRUE)

rf_tuning %>% 
  select_best(metric='roc_auc')

#STOP

rf_model <- rand_forest(mtry = 30,
                        trees = 500,
                        min_n = 26) %>% 
  set_engine('ranger', importance = 'impurity') %>% 
  set_mode('classification')
  
tic(quiet=FALSE)

rf_fit<- rf_model %>% 
  fit(lateflight ~ ., data = hist_processed)

toc(log=TRUE)

prob_pred_rf <- predict(rf_fit,
                        new_data = test_processed,
                        type = 'prob')

class_pred_rf <- predict(rf_fit,
                         new_data = test_processed,
                         type = 'class')

rf_results <- test_processed %>% 
  select(lateflight) %>% 
  bind_cols(prob_pred_rf, class_pred_rf)

view(rf_results)

rf_results %>% 
  conf_mat(truth = lateflight,
           estimate = .pred_class) %>% 
           autoplot(type = 'mosaic')

accuracy(rf_results,
         truth = lateflight,
         estimate = .pred_class
         )

sens(rf_results,
     truth = lateflight,
     estimate = .pred_class) 

spec(rf_results,
     truth = lateflight,
     estimate = .pred_class) 

roc_curve(rf_results,
          truth = lateflight,
          estimate = .pred_Yes) %>% 
  autoplot

dev.off()
roc_auc(rf_results,
          truth = lateflight,
          estimate = .pred_Yes)

vip(rf_fit)

test_processed

plot(1-rf_results$.pred_No,rf_results$.pred_Yes)
lines(density(rf_results$.pred_No))


#Sample data
#dat <- data.frame(dens = c(rnorm(100), rnorm(100, 10, 5))
                  , lines = rep(c("a", "b"), each = 100))
#Plot.
#ggplot(dat, aes(x = dens, fill = lines)) + geom_density(alpha = 0.5)


#predictions long

predictions_long <- pivot_longer(rf_results,
                                 cols=c(".pred_Yes",".pred_No"),
                                 names_to = "model",
                                 values_to = "predictions")

predictions_long %>% 
  group_by(model) %>% 
  roc_curve(truth = lateflight,
            estimate = predictions) %>% 
  autoplot

predictions_long %>% 
  group_by(model) %>% 
  roc_auc(truth = lateflight,
            estimate = predictions) 



confusionmatrix(rf_results$lateflight, 
                                rf_results$.pred_Yes, 
                                cutoff = .6 )

rf_results$.pred_yes_.6 <- ifelse(rf_results$.pred_Yes >= .6, "Yes", "No")

rf_results$.pred_yes_.6 <- as.factor(rf_results$.pred_yes_.6)
rf_results$.pred_yes_.6 <- ordered(rf_results$.pred_yes_.6, levels = c("Yes", "No"))

rf_results$.pred_yes_.7 <- ifelse(rf_results$.pred_Yes >= .7, "Yes", "No")

rf_results$.pred_yes_.7 <- as.factor(rf_results$.pred_yes_.7)
rf_results$.pred_yes_.7 <- ordered(rf_results$.pred_yes_.7, levels = c("Yes", "No"))

rf_results$.pred_yes_.8 <- ifelse(rf_results$.pred_Yes >= .8, "Yes", "No")

rf_results$.pred_yes_.8 <- as.factor(rf_results$.pred_yes_.8)
rf_results$.pred_yes_.8 <- ordered(rf_results$.pred_yes_.8, levels = c("Yes", "No"))

rf_results$.pred_yes_.9 <- ifelse(rf_results$.pred_Yes >= .9, "Yes", "No")

rf_results$.pred_yes_.9 <- as.factor(rf_results$.pred_yes_.9)
rf_results$.pred_yes_.9 <- ordered(rf_results$.pred_yes_.9, levels = c("Yes", "No"))

rf_results$.pred_yes_.4 <- ifelse(rf_results$.pred_Yes >= .43, "Yes", "No")

rf_results$.pred_yes_.4 <- as.factor(rf_results$.pred_yes_.4)
rf_results$.pred_yes_.4 <- ordered(rf_results$.pred_yes_.4, levels = c("Yes", "No"))


new<-rf_results[order(rf_results$.pred_Yes),]

#cutoff = .4
rf_results %>% 
  conf_mat(truth = lateflight,
           estimate = .pred_yes_.4)

rf_results %>% 
  sens(truth = lateflight,
       estimate = .pred_yes_.4)

rf_results %>% 
  spec(truth = lateflight,
       estimate = .pred_yes_.4) 

rf_results %>% 
  accuracy(truth = lateflight,
       estimate = .pred_yes_.4) 


#cutoff = .5
rf_results %>% 
  conf_mat(truth = lateflight,
           estimate = .pred_class) 

rf_results %>% 
  sens(truth = lateflight,
           estimate = .pred_class) 

rf_results %>% 
  spec(truth = lateflight,
       estimate = .pred_class) 

rf_results %>% 
  accuracy(truth = lateflight,
       estimate = .pred_class) 


rf_results %>% 
  filter(.pred_Yes <= .9) %>% 
  sens(truth = lateflight,
       estimate = .pred_class) 

rf_results %>% 
  spec(truth = lateflight,
       estimate = .pred_class) 


#cutoff = .6
rf_results %>% 
  conf_mat(truth = lateflight,
           estimate = .pred_yes_.6)

rf_results %>% 
  sens(truth = lateflight,
           estimate = .pred_yes_.6)

rf_results %>% 
  spec(truth = lateflight,
       estimate = .pred_yes_.6) 


#cutoff = .7
rf_results %>% 
  conf_mat(truth = lateflight,
           estimate = .pred_yes_.7)

rf_results %>% 
  sens(truth = lateflight,
       estimate = .pred_yes_.7)

rf_results %>% 
  spec(truth = lateflight,
       estimate = .pred_yes_.7)


#cutoff = .8
rf_results %>% 
  conf_mat(truth = lateflight,
           estimate = .pred_yes_.8)

rf_results %>% 
  sens(truth = lateflight,
       estimate = .pred_yes_.8)

rf_results %>% 
  spec(truth = lateflight,
       estimate = .pred_yes_.8)


#cutoff = .9
rf_results %>% 
  conf_mat(truth = lateflight,
           estimate = .pred_yes_.9)

rf_results %>% 
  sens(truth = lateflight,
       estimate = .pred_yes_.9)

rf_results %>% 
  spec(truth = lateflight,
       estimate = .pred_yes_.9)


 rf_results %>% 
  filter(.pred_Yes <= .5) %>% 
    conf_mat(truth = lateflight,
           estimate = .pred_class)


