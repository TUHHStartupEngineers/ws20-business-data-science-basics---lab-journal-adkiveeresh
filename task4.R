# Data Visualization

# Importing library-----

library(tidyverse)
library(data.table)
library(ggplot2)
library(ggrepel)

# Importing Data from given url-----
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_tbl <- fread(url)

class(covid_data_tbl)
colnames(covid_data_tbl)
str(covid_data_tbl)

#check the unique country present.
unique(covid_data_tbl$countriesAndTerritories)

#getting month name column
covid_data_tbl$month_name<-months(as.Date(covid_data_tbl$dateRep))

##rolling up data to month year country Level
covid_mon_yr_country_lvl <- covid_data_tbl %>% 
  dplyr::group_by(month,month_name,year,countriesAndTerritories,geoId,countryterritoryCode,continentExp) %>% 
  dplyr::summarise(cases = sum(cases, na.rm = T)) %>% 
  dplyr::ungroup()

##creating Cummulative Cases column
covid_mon_yr_country_lvl <- covid_mon_yr_country_lvl %>% 
  dplyr::arrange(countriesAndTerritories,year,month) %>% 
  dplyr::group_by(countriesAndTerritories) %>% 
  dplyr::mutate(cumulative_cases = cumsum(cases)) %>% 
  dplyr::ungroup()

##I am filtering only for those shown in the graph and for the year = 2020
covid_mon_yr_country_lvl_fil<- covid_mon_yr_country_lvl %>% 
  dplyr::filter(countriesAndTerritories %in% c("Germany","Spain","France","United_Kingdom","United_States_of_America")& year == 2020) %>%
  dplyr::rename('Continent_Country' = countriesAndTerritories)

#Graph using ggploat
covid_mon_yr_country_lvl_fil %>% 
  mutate(label = if_else(month_name == "December",as.character(cumulative_cases),NA_character_)) %>% 
  ggplot(aes(x=month,y =cumulative_cases))+
  geom_line(aes(color = Continent_Country))+
  scale_colour_brewer(palette = "Set3")+
  scale_x_continuous(breaks=covid_mon_yr_country_lvl_fil$month,labels = covid_mon_yr_country_lvl_fil$month_name)+
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6,
                                                    prefix = "",
                                                    suffix = "M"))+
  labs(title = "COVID-19 confirmed cases worldwide",
       subtitle =  "As of 12/5/2020,USA has the highest cases.",
       x = "Year 2020",
       y= "Cumulative Cases"
  )+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=45,hjust = 1))+
  geom_label_repel(aes(label=label),
                   nudge_x = 1,na.rm = TRUE)

vercovid_data_tbl %>% dplyr::filter(continentExp == "Europe")
sum(a$cases,na.rm = T)
        




