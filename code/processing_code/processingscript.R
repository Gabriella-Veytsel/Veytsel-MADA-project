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
library(tidyr)
library(cowplot)
library(table1) #html table 1

#path to data
#note the use of the here() package and not absolute paths
data_location1 <- here::here("data","raw_data","CaseCountData.xlsx")
data_location2 <- here::here("data","raw_data","COVID-19 Vaccine Data by County.xlsx")
data_location3 <- here::here("data","raw_data","Texas COVID-19 New Confirmed Cases by County.xlsx")
data_location4 <- here::here("data","raw_data","Population Demographics.csv")

#Population Demographics, Texas
#******************************
rawdata_Demographics <- read_csv(data_location4)
cleandata_Demographics <- rawdata_Demographics %>%
  filter(Age == "All Ages") %>% 
  filter(FIPS != "000")

glimpse(cleandata_Demographics)

#Total Cases and Fatalities
#**************************

#the first row of data is not column names, skip
rawdata_Cases.and.Fatalities <- read_excel(data_location1, sheet = 1, skip = 1) #skip first 2 rows

#take a look at the data
#2 obs with missing probable cases, 2 obs with missing confirmed cases
summary(rawdata_Cases.and.Fatalities) 

#examine those 2 obs using filter(is.na): 
#county = unknown & county = grand total
#remove them
cleandata_Cases.and.Fatalities <- rawdata_Cases.and.Fatalities %>% 
  filter(!is.na(`Confirmed Cases`)) 

glimpse(cleandata_Cases.and.Fatalities)

#New Confirmed Cases over Time
#*****************************

#the first two rows of data are not column names, skip
rawdata_Trends <- read_excel(data_location1, sheet = 2, skip = 3)

summary(rawdata_Trends) #Date should not be a character
head(rawdata_Trends$Date) #Format is year/month/day
cleandata_Trends <- rawdata_Trends %>%
  mutate(Date = as.Date(rawdata_Trends$Date, '%Y/%m/%d'))
head(cleandata_Trends$Date) #looks good

glimpse(cleandata_Trends)

#New Confirmed Cases over Time, by County
#****************************************

rawdata_NewCasesTime <- read_excel(data_location3, skip = 2) #skip first 2 rows
cleandata_NewCasesTime <-rawdata_NewCasesTime[-c(255:260),] #remove last 6 rows
colnames(cleandata_NewCasesTime) <- gsub("New Cases", "", colnames(cleandata_NewCasesTime)) #remove "new cases" from column names

NewCasesTime_long <- cleandata_NewCasesTime %>%
  pivot_longer(!County, names_to = "Date", values_to = "NewCases") %>%
  mutate(Date = as.Date(Date, '%m-%d-%Y'))

glimpse(NewCasesTime_long)

#Vaccination by County data
#**************************
rawdata_Vaccination <- read_excel(data_location2, sheet = 2)
head(rawdata_Vaccination)
table(rawdata_Vaccination$`County Name`)
cleandata_Vaccination <- rawdata_Vaccination %>%
  filter(!grepl('Federal|Texas', `County Name`)) #remove 3 rows containing "Federal" or "Texas" in the county name

glimpse(cleandata_Vaccination)

#Texas Shapefiles, County Borders
#********************************

#Shapefiles came from: https://www.depts.ttu.edu/geospatial/center/TexasGISData.html
shapefile_location <- here::here("data","raw_data","Tx_CntyBndry_Jurisdictional_TIGER", "Tx_CntyBndry_Jurisdictional_TIGER.shp")
TX_shapefile <- st_read(shapefile_location) #254 features (counties)
#geometry, AREA_SQMI 

ggplot() + geom_sf(data = TX_shapefile) #examine shapefile

#Merge shapefile with metadata
#*****************************

#Demographic data has county = DE WITT, everything else has DEWITT
cleandata_Demographics$County <- ifelse(cleandata_Demographics$County == "DE WITT COUNTY", "DEWITT COUNTY", cleandata_Demographics$County)

#Concatenate county name with "County" to match shapefile county names
cleandata_Cases.and.Fatalities$County <- paste(cleandata_Cases.and.Fatalities$County, "County", sep = " ") 
merge_shp <- merge(TX_shapefile, cleandata_Cases.and.Fatalities,  by.x = "NAME", by.y = "County") #Case.and.Fatalities data: `Confirmed Cases`
merge_shp <- merge(merge_shp, cleandata_Vaccination, by.x = "COUNTY", by.y = "County Name") #Vaccination data: `People Vaccinated with at least One Dose`, `People Fully Vaccinated`, `Population, 65+`
merge_shp$NAME <-  toupper(merge_shp$NAME) #for merging
merge_shp <- left_join(merge_shp, cleandata_Demographics, by = c("NAME" = "County")) #Demographics data: Total, Hispanic_Total, Total_Male, NH_Asian_Total, NH_Black_Total  
#the order matters! if shapefile is 2nd, merge_shp is dataframe, not sf

glimpse(merge_shp)

#There's a population variable (2010 census) in the shapefile. Use 2019 population size instead, from demographics data
merge_shp <- merge_shp %>%
  mutate(Population_Density = Total/AREA_SQKM) %>% #population/area (km^2)
  mutate(Population_Density = round(Population_Density,2)) %>%
  arrange(desc(`Confirmed Cases`)) #sort by confirmed cases

#Add cumulative case rates
merge_shp <- merge_shp %>%
  mutate(CumCaseRatePer100 = `Confirmed Cases`/Total * 100)

#Add vaccination
#Calculate proportions
merge_shp_vac <- merge_shp %>%
  mutate(Population65Plus = as.numeric(`Population, 65+`)) %>%
  mutate(Percent65Plus = Population65Plus / Total * 100) %>%
  mutate(PopulationVaccinated_1dose = as.numeric(`People Vaccinated with at least One Dose`)) %>%
  mutate(PercentVaccinated_1dose = PopulationVaccinated_1dose /  Total * 100) %>%
  mutate(PopulationVaccinated_full = as.numeric(`People Fully Vaccinated`)) %>%
  mutate(PercentVaccinated_full = PopulationVaccinated_full /  Total * 100) %>%
  
  rename(PopulationMedCondition = `Population, 16-64\r\n Any Medical Condition`) %>%
  mutate(PopulationMedCondition = as.numeric(PopulationMedCondition)) %>%
  mutate(PercentMedCondition = PopulationMedCondition /  Total * 100) %>%
    
  mutate(PercentHispanic = Hispanic_Total /  Total * 100) %>%
  mutate(PercentAsian = NH_Asian_Total /  Total * 100) %>%
  mutate(PercentBlack = NH_Black_Total /  Total * 100) %>%
  mutate(PercentMale = Total_Male / Total * 100)

# save data as RDS
# location to save file
save_data_location1 <- here::here("data","processed_data","merge_shp_vac.rds")
save_data_location2 <- here::here("data","processed_data","cleandata_Trends.rds")
save_data_location3 <- here::here("data","processed_data","cleandata_Cases.and.Fatalities.rds")
save_data_location4 <- here::here("data","processed_data","cleandata_Vaccination.rds")
save_data_location5 <- here::here("data","processed_data","NewCasesTime_long.rds")
save_data_location8 <- here::here("data","processed_data","cleandata_Demographics.rds")

saveRDS(merge_shp_vac, file = save_data_location1)
saveRDS(cleandata_Trends, file = save_data_location2)
saveRDS(cleandata_Cases.and.Fatalities, file = save_data_location3)
saveRDS(cleandata_Vaccination, file = save_data_location4)
saveRDS(NewCasesTime_long, file = save_data_location5)
saveRDS(cleandata_Demographics, file = save_data_location8)

