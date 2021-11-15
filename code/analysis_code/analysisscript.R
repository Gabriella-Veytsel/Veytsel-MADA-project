#Outcome: county-level cumulative rates of COVID-19
#County-level predictors of interest: 
#Population Size
#Population Density
#Population, 65+
#Population, Black
#Population, Hispanic
#Population, Asian
#Population, Male
#Population, Medical Condition

#Load packages
library(tidyverse)
library(tidymodels)
library(poissonreg)
library(lme4)
library(broom)
library(sjPlot)
library(ggpubr)
library(performance) #overdispersion
library(AER) #overdispersion
library(MASS) #negative binomial
library(vip)

#Load data
save_data_location1 <- here::here("data","processed_data","merge_shp_vac_df_c.rds")
save_data_location2 <- here::here("data","processed_data","cleandata_Trends.rds")
save_data_location3 <- here::here("data","processed_data","cleandata_Cases.and.Fatalities.rds")
save_data_location4 <- here::here("data","processed_data","cleandata_Vaccination.rds")
save_data_location5 <- here::here("data","processed_data","NewCasesTime_long.rds")
save_data_location6 <- here::here("data","processed_data","cleandata_Demographics.rds")

combined <- readRDS(save_data_location1)
cleandata_Trends <- readRDS(save_data_location2)
cleandata_Cases.and.Fatalities <- readRDS(save_data_location3)
cleandata_Vaccination <- readRDS(save_data_location4)
NewCasesTime_long <- readRDS(save_data_location5)
cleandata_Demographics <- readRDS(save_data_location6)

combined <- combined %>%
  dplyr::select(-COUNTY)

#Is cumulative case rate normally distributed? No
hist(combined$CumCaseRatePer100, main = "Histogram of Cumulative Case Rate per 1,000", xlab = "Cumulative Case Rate per 1,000")
ggdensity(combined$CumCaseRatePer100, main = "Density Plot of Cumulative Case Rate per 1,000", xlab = "Cumulative Case Rate per 1,000")
ggqqplot(combined$CumCaseRatePer100) #QQ plot

shapiro.test(combined$CumCaseRatePer100) #p-value = 5.8E-7 
#Data are significantly different from the normal distribution

#*************
#Poisson Model
#*************
mean(combined$`Confirmed Cases`) #mean
var(combined$`Confirmed Cases`) #variance very very different
out <- glm(`Confirmed Cases` ~ Percent65Plus +
             PercentAsian + PercentBlack + PercentHispanic + PercentMale +
             PercentMedCondition + AREA_SQKM + offset(log(Population)), data = combined,
           family = poisson)
summary(out) #residual deviance is 182633 for 246 degrees of freedom
#Rule of thumb: this ratio should be 1, but it is 742, so severe overdispersion

#Check overdispersion
dispersiontest(out) #overdispersion detected (AER package)
check_overdispersion(out) #overdispersion detected (performance package)

par(mfrow = c(2,2))
plot(out) #OOH NOT GOOD

#***********************
#Negative Binomial Model
#***********************

out_nb <- glm.nb(`Confirmed Cases` ~ Percent65Plus + PercentMedCondition +
                   PercentAsian + PercentBlack + PercentHispanic + PercentMale +
                   AREA_SQKM + offset(log(Population)), data = combined)
summary(out_nb) 
#Residual deviance is 260.67 for 247 degrees of freedom--ratio is about 1, so dispersion should be fine now

AIC(out_nb)
rmse(out_nb) #What????????????????

jpeg(file = "results/NegBinomial_pred_vs_obs.jpeg")
#predicted versus observed
plot(out_nb$fitted.values, combined$`Confirmed Cases`)
abline(a=0,b=1, col = 'red')#45 degree line, along which the results should fall
title(main ="Predicted vs. Observed, Negative Binomial")
dev.off()
jpeg(file = "results/NegBinomial_residuals.jpeg")
#residuals
plot(out_nb$fitted.values-combined$`Confirmed Cases`)
abline(a=0,b=0, col = 'red') #straight line, along which the results should fall
title(main ="Residuals, Negative Binomial")
dev.off()
#diagnostic plots
jpeg(file = "results/NegBinomial_diagnosticplots.jpeg")
par(mfrow = c(2,2)) #residuals look randomly distributed, 3 outliers in qqplot and residuals vs fitted graph (93, 51, 180), no pattern in scale-location graph
plot(out_nb)
dev.off()

#******************************************************
#Generalized Linear Mixed Model (Random Effect: County)
#******************************************************

out_random <- glmer(formula = `Confirmed Cases` ~ offset(log(Population)) + #offset = population size
                      scale(Percent65Plus) + 
                      scale(PercentHispanic) +
                      scale(PercentBlack) +
                      scale(PercentAsian) +
                      scale(PercentMedCondition) +
                      scale(AREA_SQKM) +
                      scale(PercentMale) +
                      (1|COUNTY), #random effects = county
                    family=poisson, #count data
                    data = combined) 
summary(out_random)
fixef(out_random)
model_results <- tab_model(out_random)
rmse(out_random)

jpeg(file = "results/MixedModel_pred_vs_obs.jpeg")
#predicted versus observed
plot(fitted(out_random), combined$`Confirmed Cases`)
abline(a=0,b=1, col = 'red')#45 degree line, along which the results should fall
title(main="Predicted vs. Observed, Mixed Effects Model")
dev.off()
jpeg(file = "results/MixedModel_residuals.jpeg")
#residuals
plot(fitted(out_random)-combined$`Confirmed Cases`)
abline(a=0,b=0, col = 'red') #straight line, along which the results should fall
title(main = "Residuals, Mixed Effects Model")
dev.off()

#***********************
#Linear Regression Model
#***********************

#Set a seed for reproducibility
set.seed(4595)

#Split data into train and test, set up cross-validation
data_split <- initial_split(combined, strata = CumCaseRatePer100)
training_data <- training(data_split)
testing_data <- testing(data_split)
cv_data <- vfold_cv(training_data, v= 5, repeats = 5, strata = CumCaseRatePer100)

#Recipe
fit_recipe <- recipe(CumCaseRatePer100 ~ Percent65Plus +
           PercentAsian + PercentBlack + PercentHispanic + 
           PercentMale +
           PercentMedCondition + 
           AREA_SQKM, data = training_data) 

#Null model:
# For a continuous outcome, using RMSE as our performance metric, 
# a null-model that doesn't use any predictor information is one that 
# always just predicts the mean of the data. We'll compute the performance of such a "model" here. 
# It's useful for comparison with the real models. 
# We'll print both numbers here, and then compare with our model results below. 
# Since our performance metric is RMSE, we compute that here with the "model prediction" 
# always just being the mean of the outcomes.

RMSE_null_train <- sqrt(sum( (training_data$CumCaseRatePer100 - mean(training_data$CumCaseRatePer100))^2 )/nrow(training_data))
RMSE_null_test <- sqrt(sum( (testing_data$CumCaseRatePer100 - mean(testing_data$CumCaseRatePer100))^2 )/nrow(testing_data))
print(RMSE_null_train)
print(RMSE_null_test)

#LASSO model
#***********

#LASSO is a penalization method for less relevant preictors
#Model specification:
tune_spec_lasso <- linear_reg(penalty = tune(),
                              mixture = 1) %>% #glmnet model will potentially remove irrelevant predictors and choose a simpler model
  set_engine("glmnet") #fits glm models via penalized maximum likelihood

tune_spec_lasso

#Workflow
lasso_workflow <-
  workflow() %>%
  add_model(tune_spec_lasso) %>%
  add_recipe(fit_recipe)

#Tuning grid
lasso_grid <- tibble(penalty = 10^seq(-4,-1,length.out = 30)) #30 candidate values
lasso_grid %>% top_n(-5) #lowest penalty values
lasso_grid %>% top_n(5) #highest penalty values

lasso_res <-
  lasso_workflow %>%
  tune_grid(cv_data,
            grid = lasso_grid,
            control = 
              control_grid(save_pred = TRUE)
            #, metrics = metric_set(rmse) #not working
            )

top_models <- lasso_res %>%
  show_best("rmse", n = 15) %>%
  arrange(penalty)

top_models

#LASSO evaluation
lasso_res %>% autoplot()

#get the tuned model that performs best
best_lasso <- lasso_res %>%  select_best(metric = "rmse")

#finalize workflow with best model
best_lasso_wf <- lasso_workflow %>% 
  finalize_workflow(best_lasso)

# fitting best performing model
best_lasso_fit <- best_lasso_wf %>% 
  fit(data = training_data)

lasso_pred <- predict(best_lasso_fit, training_data)

#Plotting LASSO variables as function of tuning parameter
x <- best_lasso_fit$fit$fit$fit
plot(x, "lambda")
# the larger the regularization penalty, the fewer predictor variables that remain in the model. (Once a coefficient is at 0, 
# the corresponding variable is not in the model anymore).

#This shows the variables that are part of the best-fit LASSO model, i.e. those that have a non-zero coefficient.
tidy(extract_fit_parsnip(best_lasso_fit)) %>% filter(estimate != 0)
#Percent male was removed

lasso_perfomance <- lasso_res %>% show_best(n = 1)
print(lasso_perfomance) #rmse = 3.82

#predicted versus observed
jpeg(file = "results/LASSO_Predicted_vs_Observed.jpeg")
plot(lasso_pred$.pred,training_data$CumCaseRatePer100)
abline(a=0,b=1, col = 'red') #45 degree line, along which the results should fall
title(main = "Predicted vs. Observed, LASSO")
dev.off()
#residuals
jpeg(file = "results/LASSO_Residuals.jpeg")
plot(lasso_pred$.pred-training_data$CumCaseRatePer100)
abline(a=0,b=0, col = 'red') #straight line, along which the results should fall
title(main = "Residuals, LASSO")
dev.off()

#Random forest
rf_model <- rand_forest() %>%
  set_args(mtry = tune(),     
           trees = tune(),
           min_n = tune()
  ) %>%
  # select the engine/package that underlies the model
  set_engine("ranger",
             num.threads = 18, #for some reason for RF, we need to set this in the engine too
             importance = "permutation") %>%
  # choose either the continuous regression or binary classification mode
  set_mode("regression")     

rf_workflow <-
  workflow() %>%
  add_model(rf_model) %>%
  add_recipe(fit_recipe)

#tuning grid
rf_grid  <- expand.grid(mtry = c(3, 4, 5, 6), min_n = c(40,50,60), trees = c(500,1000)  )

#tune the model, optimizing rmse
rf_tune_res <- rf_workflow %>%
  tune_grid(
    resamples = cv_data, #CV object
    grid = rf_grid # grid of values to try
    #, metrics = metric_set(rmse) #doesn't work
  )

rf_tune_res %>% 
  show_best(metric = "rmse") #best rmse is 3.77

#random forest evaluation
autoplot(rf_tune_res) #plots the results of the tuning process

rf_best <-
  rf_tune_res %>%
  select_best(metric = "rmse")

rf_best 

# finalize workflow with best model
best_rf_wf <- rf_workflow %>% 
  finalize_workflow(rf_best)

# fitting best performing model
best_rf_fit <- best_rf_wf %>% 
  fit(data = training_data)

rf_pred <- predict(best_rf_fit, training_data)

#For random forest models, one can't easily look at the final model. 
#One can however look at the most important predictors for the final model.

#pull out the fit object
x <- best_rf_fit$fit$fit$fit

#plot variable importance
vip(x, num_features = 20) #PercentHispanic > Percent65Plus > PercentMedCondition > PercentAsian > PercentBlack > PercentMale > Area

#Plotting observed/predicted and residuals.
jpeg(file = "results/RF_pred_vs_obs.jpeg")
#predicted versus observed
plot(rf_pred$.pred,training_data$CumCaseRatePer100)
abline(a=0,b=1, col = 'red') #45 degree line, along which the results should fall
title(main = "Predicted vs. Observed, RF")
dev.off()
#residuals
jpeg(file = "results/RF_residuals.jpeg")
plot(rf_pred$.pred-training_data$CumCaseRatePer100)
abline(a=0,b=0, col = 'red') #straight line, along which the results should fall
title(main = "Residuals, RF")
dev.off()

rf_perfomance <- rf_tune_res %>% show_best(n = 1)
print(rf_perfomance)

#Apply the model a single time to the test data
#RANDOM FOREST!!
final_fit <- best_rf_wf %>% last_fit(data_split)
test_performance <- final_fit %>% collect_metrics()
print(test_performance)

test_predictions <- final_fit %>% collect_predictions()
#Plotting observed/predicted and residuals.

#predicted versus observed
plot(test_predictions$.pred,testing_data$CumCaseRatePer100)
abline(a=0,b=1, col = 'red') #45 degree line, along which the results should fall
#residuals
plot(test_predictions$.pred-testing_data$CumCaseRatePer100)
abline(a=0,b=0, col = 'red') #straight line, along which the results should fall












