#load packages
library(readxl) #for loading Excel files
library(here) #to set paths
library(tidyverse)
library(sf)#st_read to read shapefiles
library(tmap) #plot map
library(scales)
library(broom) #tidy
library(ggpubr) #add equation and r-square 
library(tidyr)
library(cowplot)
library(table1) #html table 1

#Read RDS files from processingscript.R
#**************************************

save_data_location1 <- here::here("data","processed_data", "merge_shp_vac.rds")
save_data_location2 <- here::here("data","processed_data","cleandata_Trends.rds")
save_data_location3 <- here::here("data","processed_data","cleandata_Cases.and.Fatalities.rds")
save_data_location4 <- here::here("data","processed_data","cleandata_Vaccination.rds")
save_data_location5 <- here::here("data","processed_data","NewCasesTime_long.rds")
save_data_location6 <- here::here("data","processed_data","cleandata_Demographics.rds")

merge_shp_vac <- readRDS(save_data_location1)
cleandata_Trends <- readRDS(save_data_location2)
cleandata_Cases.and.Fatalities <- readRDS(save_data_location3)
cleandata_Vaccination <- readRDS(save_data_location4)
NewCasesTime_long <- readRDS(save_data_location5)
cleandata_Demographics <- readRDS(save_data_location6)

#Epidemic curve
#**************

epicurve <- ggplot(cleandata_Trends, aes(x=Date, y = `New Confirmed Cases`)) +
  geom_bar(stat = "identity", fill="lightblue3") +
  scale_x_date(date_breaks = "months" , 
               date_labels = "%b-%y", 
               limits = c(min(cleandata_Trends$Date), max = max(cleandata_Trends$Date)))

epicurve

#Save figure
figure_file_epicurve = here("results","epicurve.png")
ggsave(filename = figure_file_epicurve, plot=epicurve, height = 5, width = 8) 

#Epidemic Curve, by County
#*************************

#Too many counties for facet_wrap, narrow down to top 10 counties
NewCasesTime_long %>%
  group_by(County) %>%
  summarize(Sum = sum(NewCases)) %>%
  arrange(desc(Sum))

NewCasesTime_long_subset <- NewCasesTime_long %>%
  filter(County %in% c("Harris", "Dallas", "Tarrant", "Bexar", "El Paso", "Travis", "Collin", "Fort Bend", "Hidalgo", "Denton"))

epicurve_county <- ggplot(NewCasesTime_long_subset, aes(x=Date, y = NewCases)) +
  geom_bar(stat = "identity", fill="lightblue3") +
  scale_x_date(date_breaks = "months" , 
               date_labels = "%b-%y", 
               limits = c(min(NewCasesTime_long$Date), max = max(NewCasesTime_long$Date))) + 
  facet_wrap(~County, nrow = 2) + #2 rows
  theme(axis.text.x = element_text(angle=50, hjust=1)) +
  labs(title = "Epidemic Curve", 
       subtitle = "Counties with the most total cases in Texas",
       y = "New Cases", x = "")

epicurve_county

#Save figure
figure_file_epicurve_county = here("results","epicurve_county.png")
ggsave(filename = figure_file_epicurve_county, plot=epicurve_county, height = 5, width = 15) 

#Confirmed Case, by County
tail(sort(merge_shp_vac$`Confirmed Cases`),10) #top 10 counties with the most confirmed cases
map_cases <- ggplot(merge_shp_vac) +
  geom_sf(aes(fill = `Confirmed Cases`)) +
  ggtitle("Confirmed Cases by County, as of 10/3/2021") +
  geom_sf_text(data = subset(merge_shp_vac, `Confirmed Cases` >= 67896), aes(label = COUNTY), size = 2.5) +
  scale_fill_gradient(low = "light blue", high = "red", name = "Confirmed Cases", labels = comma) 
#labels = comma removes scientific notation (package 'scales')

map_cases
#As expected, these are all the major cities. Instead, look at rates to compare counties with different population sizes

#Save figure
figure_file_map_cases = here("results","map_cases.png")
ggsave(filename = figure_file_map_cases, plot=map_cases, height = 12, width = 15) 

#Cumulative Confirmed Case Rate/1,000 Persons, by County
tail(sort(merge_shp_vac$CumCaseRatePer1000),10) #top 10 counties with the highest cumulative case rate
map_rates <- ggplot(merge_shp_vac) +
  geom_sf(aes(fill = CumCaseRatePer1000)) +
  ggtitle("Cumulative Confirmed Case Rate/1,000 Persons, by County, as of 10/3/2021") +
  geom_sf_text(data = subset(merge_shp_vac, CumCaseRatePer1000 >= 191), aes(label = COUNTY), size = 2.5) +
  scale_fill_gradient(low = "light blue", high = "red", name = "Cumulative Confirmed Case Rate/1,000 Persons", labels = comma)

map_rates
#Very different results, hmm...let's summarize county-level characteristics

#Save figure
figure_file_map_rates = here("results","map_rates.png")
ggsave(filename = figure_file_map_rates, plot=map_rates, height = 12, width = 15) 

merge_shp_vac %>%
  select(COUNTY, CumCaseRatePer1000) %>%
  arrange(desc(CumCaseRatePer1000)) 

merge_shp_vac_subset <- merge_shp_vac %>%
  select(c(COUNTY, "Confirmed Cases", Total, "CumCaseRatePer1000", Population_Density,
           Percent65Plus, PercentVaccinated_1dose, PercentVaccinated_full,
           PercentHispanic, PercentAsian, PercentBlack)) %>%
  rename(Population = Total)
summary(merge_shp_vac_subset)

merge_shp_vac_df_c <- as.data.frame(merge_shp_vac_subset) %>%
  select(-c(geometry)) #has to be a dataframe without geometry for table 1

merge_shp_vac_df <- as.data.frame(merge_shp_vac_subset) %>%
  select(-c(COUNTY, geometry)) 

table1 <- table1(~ . , data = merge_shp_vac_df, topclass="Rtable1-zebra") 
table1

#Save table
save_data_location7 <- here::here("results","summarytable.rds")
saveRDS(table1, file = save_data_location7)

#County characteristics, subset to counties with highest cumulative case rates
table_county <- merge_shp_vac_df_c %>%
  filter(COUNTY %in% c("Dimmit", "Hale", "Uvalde", "Maverick", "Karnes", "Childress", "Lubbock", "Val Verde", "Potter", "Chambers" )) %>%
  group_by(COUNTY) %>%
  arrange(desc(CumCaseRatePer1000))

#Save table
save_data_location6 <- here::here("results","table_county.rds")
saveRDS(table_county, file = save_data_location6)

#Association between county-level characteristics and # confirmed cases 
p1 <- ggplot(merge_shp_vac, aes(x=Total, y=`Confirmed Cases`)) +
  geom_point() +
  geom_smooth(method='lm') + #add linear regression line 
  #stat_regline_equation(label.y = 570000, aes(label = ..eq.label..)) +
  stat_regline_equation(aes(label = ..rr.label..)) +
  scale_x_continuous(labels = comma) + #remove scientific notation
  scale_y_continuous(labels = comma) 

p2 <- ggplot(merge_shp_vac, aes(x=Total_Male, y=`Confirmed Cases`)) +
  geom_point() +
  geom_smooth(method='lm') + #add linear regression line 
  #stat_regline_equation(label.y = 550000, aes(label = ..eq.label..)) +
  stat_regline_equation(aes(label = ..rr.label..)) +
  scale_x_continuous(labels = comma) + #remove scientific notation
  scale_y_continuous(labels = comma, limits = c(0, 600000)) 

p3 <- ggplot(merge_shp_vac, aes(x=NH_Black_Total, y=`Confirmed Cases`)) +
  geom_point() +
  geom_smooth(method='lm') + #add linear regression line 
  #stat_regline_equation(label.y = 550000, aes(label = ..eq.label..)) +
  stat_regline_equation(aes(label = ..rr.label..)) +
  scale_x_continuous(labels = comma) + #remove scientific notation
  scale_y_continuous(labels = comma, limits = c(0, 600000)) 

p4 <- ggplot(merge_shp_vac, aes(x=NH_Asian_Total, y=`Confirmed Cases`)) +
  geom_point() +
  geom_smooth(method='lm') + #add linear regression line 
  #stat_regline_equation(label.y = 550000, aes(label = ..eq.label..)) +
  stat_regline_equation(aes(label = ..rr.label..)) +
  scale_x_continuous(labels = comma) + #remove scientific notation
  scale_y_continuous(labels = comma, limits = c(0, 600000)) 

p5 <- ggplot(merge_shp_vac, aes(x=Hispanic_Total, y=`Confirmed Cases`)) +
  geom_point() +
  geom_smooth(method='lm') + #add linear regression line 
  #stat_regline_equation(label.y = 550000, aes(label = ..eq.label..)) +
  stat_regline_equation(aes(label = ..rr.label..)) +
  scale_x_continuous(labels = comma) + #remove scientific notation
  scale_y_continuous(labels = comma, limits = c(0, 600000))

p6 <- ggplot(merge_shp_vac, aes(x=Population_Density, y=`Confirmed Cases`)) +
  geom_point() +
  geom_smooth(method='lm') + #add linear regression line 
  #stat_regline_equation(label.y = 550000, aes(label = ..eq.label..)) +
  stat_regline_equation(aes(label = ..rr.label..)) +
  scale_x_continuous(labels = comma) + #remove scientific notation
  scale_y_continuous(labels = comma, limits = c(0, 600000)) 

p7 <- ggplot(merge_shp_vac, aes(x=Population65Plus, y=`Confirmed Cases`)) +
  geom_point() +
  geom_smooth(method='lm') + #add linear regression line 
  #stat_regline_equation(label.y = 550000, aes(label = ..eq.label..)) +
  stat_regline_equation(aes(label = ..rr.label..)) +
  scale_x_continuous(labels = comma) + #remove scientific notation
  scale_y_continuous(labels = comma, limits = c(0, 600000)) 

p8 <- ggplot(merge_shp_vac, aes(x=PopulationVaccinated_1dose, y=`Confirmed Cases`)) +
  geom_point() +
  geom_smooth(method='lm') + #add linear regression line 
  #stat_regline_equation(label.y = 550000, aes(label = ..eq.label..)) +
  stat_regline_equation(aes(label = ..rr.label..)) +
  scale_x_continuous(labels = comma) + #remove scientific notation
  scale_y_continuous(labels = comma, limits = c(0, 600000)) 

p9 <- ggplot(merge_shp_vac, aes(x=PopulationVaccinated_full, y=`Confirmed Cases`)) +
  geom_point() +
  geom_smooth(method='lm') + #add linear regression line 
  #stat_regline_equation(label.y = 550000, aes(label = ..eq.label..)) +
  stat_regline_equation(aes(label = ..rr.label..)) +
  scale_x_continuous(labels = comma) + #remove scientific notation
  scale_y_continuous(labels = comma, limits = c(0, 600000)) 

p_combined <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9) 

#Save figure
figure_file_association = here("results","p_combined.png")
ggsave(filename = figure_file_association, plot=p_combined, height = 5, width = 15.5) 
