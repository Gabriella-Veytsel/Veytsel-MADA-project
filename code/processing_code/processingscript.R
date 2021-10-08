# processing script
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#load packages
library(readxl) #for loading Excel files
library(here) #to set paths
library(tidyverse)
library(sf)#st_read to read shapefiles
library(tmap) #plot map
library(scales)
library(broom) #tidy
library(ggpubr) #add equation and r-square 

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","raw_data","CaseCountData.xlsx")

#the first row of data is not column names, skip
rawdata_Cases.and.Fatalities <- read_excel(data_location, sheet = 1, skip = 1) #skip first 2 rows

#the first two rows of data are not column names, skip
rawdata_Trends <- read_excel(data_location, sheet = 2, skip = 3)

#take a look at the data
#2 obs with missing probable cases, 2 obs with missing confirmed cases
summary(rawdata_Cases.and.Fatalities) 

#examine those 2 obs using filter(is.na): 
  #county = unknown & county = grand total
#remove them
cleandata_Cases.and.Fatalities <- rawdata_Cases.and.Fatalities %>% 
  filter(!is.na(`Confirmed Cases`)) 

summary(rawdata_Trends) #Date should not be a character
head(rawdata_Trends$Date) #Format is year/month/day
cleandata_Trends <- rawdata_Trends %>%
  mutate(Date = as.Date(rawdata_Trends$Date, '%Y/%m/%d'))
head(cleandata_Trends$Date) #looks good

#Epidemic curve
epicurve <- ggplot(cleandata_Trends, aes(x=Date, y = `New Confirmed Cases`)) +
  geom_bar(stat = "identity", fill="lightblue3") +
  scale_x_date(date_breaks = "months" , 
               date_labels = "%b-%y", 
               limits = c(min(cleandata_Trends$Date), max = max(cleandata_Trends$Date)))
            
#Save figure
figure_file_epicurve = here("results","epicurve.png")
ggsave(filename = figure_file_epicurve, plot=epicurve, height = 5, width = 8) 

#Map
#Shapefiles came from: https://www.depts.ttu.edu/geospatial/center/TexasGISData.html
shapefile_location <- here::here("data","raw_data","Tx_CntyBndry_Jurisdictional_TIGER", "Tx_CntyBndry_Jurisdictional_TIGER.shp")
TX_shapefile <- st_read(shapefile_location) #254 features (counties)

#Merge shapefile with metadata
#Concatenate county name with "County" to match shapefile county names
cleandata_Cases.and.Fatalities$County <- paste(cleandata_Cases.and.Fatalities$County, "County", sep = " ")
merge_shp <- merge(TX_shapefile, cleandata_Cases.and.Fatalities,  by.x = "NAME", by.y = "County") 
#the order matters! if shapefile is 2nd, merge_shp is dataframe, not sf

#There's a population variable (2010 census) in the shapefile! Calcualate population density
merge_shp <- merge_shp %>%
  mutate(Population_Density = POP_2010/AREA_SQKM) %>% #population/area (km^2)
  mutate(Population_Density = round(Population_Density,2)) %>%
  arrange(desc(`Confirmed Cases`)) #sort by confirmed cases

ggplot() + geom_sf(data = TX_shapefile) #examine shapefile

tail(sort(merge_shp$`Confirmed Cases`),5) #top 5 counties with the most confirmed cases
map_cases <- ggplot(merge_shp) +
  geom_sf(aes(fill = `Confirmed Cases`)) +
  ggtitle("Confirmed Cases by County, as of 10/3/2021") +
  geom_sf_text(data = subset(merge_shp, `Confirmed Cases` >= 145433), aes(label = COUNTY), size = 2.5) +
  scale_fill_gradient(low = "light blue", high = "red", name = "Confirmed Cases", labels = comma) 
#labels = comma removes scientific notation (package 'scales')
#label only top 5 counties (in terms of confirmed cases)
#Harris County includes Houston
#Later might want to use arrows

#Save figure
figure_file_map_cases = here("results","map_cases.png")
ggsave(filename = figure_file_map_cases, plot=map_cases, height = 12, width = 13) 

tail(sort(merge_shp$Fatalities),5) #top 5 counties with the most fatalities
map_deaths <- ggplot(merge_shp) +
  geom_sf(aes(fill = `Fatalities`)) +
  ggtitle("Fatalities by County, as of 10/3/2021") +
  geom_sf_text(data = subset(merge_shp, Fatalities >= 2998), aes(label = COUNTY), size = 2.5) +
  scale_fill_gradient(low = "light blue", high = "red", name = "Fatalities", labels = comma) 

#Save figure
figure_file_map_deaths = here("results","map_deaths.png")
ggsave(filename = figure_file_map_deaths, plot=map_deaths, height = 12, width = 13) 

#Association between pop density, pop and # confirmed cases, Fatalities? 
ggplot(merge_shp, aes(x=POP_2010, y=Fatalities)) +
  geom_point() +
  geom_smooth(method='lm') + #add linear regression line 
  stat_regline_equation(label.y = 8500, aes(label = ..eq.label..)) +
  stat_regline_equation(aes(label = ..rr.label..)) +
  scale_x_continuous(labels = comma) #remove scientific notation

ggplot(merge_shp, aes(x=POP_2010, y=`Confirmed Cases`)) +
  geom_point() +
  geom_smooth(method='lm') + #add linear regression line 
  stat_regline_equation(label.y = 570000, aes(label = ..eq.label..)) +
  stat_regline_equation(aes(label = ..rr.label..)) +
  scale_x_continuous(labels = comma) + #remove scientific notation
  scale_y_continuous(labels = comma) 

ggplot(merge_shp, aes(x=Population_Density, y=Fatalities)) +
  geom_point() +
  geom_smooth(method='lm') + #add linear regression line 
  stat_regline_equation(aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 7700, aes(label = ..rr.label..)) +
  scale_x_continuous(labels = comma) #remove scientific notation

ggplot(merge_shp, aes(x=Population_Density, y=`Confirmed Cases`)) +
  geom_point() +
  geom_smooth(method='lm') + #add linear regression line 
  stat_regline_equation(label.y = 550000, aes(label = ..eq.label..)) +
  stat_regline_equation(aes(label = ..rr.label..)) +
  scale_x_continuous(labels = comma) + #remove scientific notation
  scale_y_continuous(labels = comma, limits = c(0, 600000)) 

#Linear regression
lm_deaths_pop <- lm(Fatalities~POP_2010, data = merge_shp)
fit_deaths_pop <- tidy(lm_deaths_pop)

lm_deaths_density <- lm(Fatalities~Population_Density, data = merge_shp)
fit_deaths_density <- tidy(lm_deaths_density)

lm_cases_pop <- lm(`Confirmed Cases`~POP_2010, data = merge_shp)
fit_cases_pop <- tidy(lm_cases_pop)

lm_cases_density <- lm(`Confirmed Cases`~Population_Density, data = merge_shp)
fit_cases_density <- tidy(lm_cases_density)

#Save results tables
table_file_deaths_pop = here("results", "resulttable_deaths_pop.rds")
saveRDS(fit_deaths_pop, file = table_file_deaths_pop)

table_file_deaths_density = here("results", "resulttable_deaths_density.rds")
saveRDS(fit_deaths_density, file = table_file_deaths_density)

table_file_cases_pop = here("results", "resulttable_cases_pop.rds")
saveRDS(fit_cases_pop, file = table_file_cases_pop)

table_file_cases_density = here("results", "resulttable_cases_density.rds")
saveRDS(fit_cases_density, file = table_file_cases_density)




# save data as RDS
# I suggest you save your processed and cleaned data as RDS or RDA/Rdata files. 
# This preserves coding like factors, characters, numeric, etc. 
# If you save as CSV, that information would get lost.
# See here for some suggestions on how to store your processed data:
# http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata

# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")

saveRDS(processeddata, file = save_data_location)


