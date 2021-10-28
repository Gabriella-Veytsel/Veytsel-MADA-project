#Outcome: county-level cumulative rates of COVID-19
#County-level predictors of interest: 
  #Population Size
  #Population Density
  #Population, 65+
  #Population, Vaccinated (1-dose and full)
  #Population, Black
  #Population, Hispanic
  #Population, Asian
  #Population, Male

#I don't have vaccination data by county over time, only vaccination x county & vaccination x time
#I don't have good variant data by county over time, only variant over time in Texas
  #Future analysis: outocme = weekly county-level incidence data, predictor = weekly variant proportion

library(tidymodels)
library(poissonreg)
library(lme4)
library(sjPlot)

#I don't see an offset option in tidymodels
#??????????????????????????????????????????

#Does it make more sense to use counts and offset
#Or convert daily incidence to monthly incidence
#???????????????????????????????????????????????

#Generalized linear mixed model
#Family = poisson (count data)
#Random effects = county: assumption that observations within counties are correlated
#Is adding random effects necessary when each observation is already a county? 
#Still doesn't account for correlation *between* counties: may need spatial component
#Offset = population size
#Scale: continuous covariates were scaled to have mean 0 and SD 1
model <- glmer(formula = `Confirmed Cases` ~ offset(log(Population)) + #offset = population size
              scale(Percent65Plus) + 
              scale(PercentHispanic) +
              scale(PercentBlack) +
              scale(PercentAsian) +
              scale(PercentVaccinated_1dose) +
              scale(PercentVaccinated_full) +
              scale(Population_Density) +
              (1|COUNTY), #random effects = county
              family=poisson, #count data
              data = merge_shp_vac_df_c) 
summary(model)
fixef(model)
model_results <- tab_model(model)

#Save table
save_data_location1 <- here::here("results","model.rds")
saveRDS(model, file = save_data_location1)

#Investigate spatial autocorrelation: calculate Moran's I
#********************************************************

