# Importing library-----
library(tidyverse)
library(data.table)
library(ggplot2)
library(ggrepel)
library(maps)
library(ggthemes)
library(mapproj)

# Importing Data from given url-----
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_tbl <- fread(url)

#creating the variable of the world and giving the data of map
world <- map_data("world")
colnames((world))

#Removing all the _ and , in between the names of country 
covid_data_tbl$countriesAndTerritories <- str_replace_all(covid_data_tbl$countriesAndTerritories,"_"," ")
world_data<- covid_data_tbl %>% 
  dplyr::mutate(countriesAndTerritories = case_when(
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
  ))

options(scipen = 999)

# calculating the mortality rate of each country 
country_mortality_rate<- world_data %>% 
  dplyr::group_by(countriesAndTerritories) %>% 
  dplyr::summarise(deaths = sum(deaths, na.rm = T),
                   popData2019 = nth(popData2019,1)) %>% 
  dplyr::mutate(mortality_rate = round((deaths/popData2019)*100,3))

# Plot the mortality rate of each country 
plot_data <- country_mortality_rate %>%  
  dplyr::right_join(world,by=c("countriesAndTerritories"="region"))

plot_data %>% ggplot()+
  geom_map(map = world,aes(map_id = countriesAndTerritories,fill=mortality_rate),color = "#7f7f7f",size=0.25)+
  scale_fill_gradient(low="#FF3333",high = "#330000",name="Mortality Rate")+
  expand_limits(x= world$long,y=world$lat)+
  labs(x="",y="",title="Cnfirmed COVID-19 deaths relative to the size of population")











