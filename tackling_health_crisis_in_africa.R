# load all required libraries

library(dplyr)

library(tidyr)

library(readr)

library(ggplot2); theme_set(theme_minimal())

library(RColorBrewer)

library(readxl)

# import dataset 

annual_deaths_by_cause <- read_csv("1. annual-number-of-deaths-by-cause.csv")

deaths_by_age_grp <- read_csv("2. number-of-deaths-by-age-group.csv")

med_doc_per_10k_pop <- read_excel("3. Medical Doctors Per 10000 population.xlsx",
                                  skip = 2)

country <- read_csv("4. ISO 3166_country-and-continent-codes-list-csv.csv")

world_pop <- read_csv("5. World Population.csv")

current_health_xp <- read_excel("6. Current health expenditure (% of GDP).xlsx",
                                skip = 4)

# viewing the structure of the datasets
# remove non-disease and infection related variables
# rename country, country and year coloumn to ensure uniformity
# remove uncleaned data from workspace after cleaning

glimpse(annual_deaths_by_cause)

annual_deaths_by_cause <- annual_deaths_by_cause %>% # removing unwanted records
  select(- Code, -Protein, -`Number of executions (Amnesty International)`,
         -`Interpersonal violence`, -Self, 
         -`Exposure to forces of nature`, -`Environmental heat and cold exposure`,
         -`Conflict and terrorism`, 
         -`Terrorism (deaths)`, -`Fire, heat, and hot substances`)

glimpse(annual_deaths_by_cause)

# renaming entity to Country to make joining tables simpler

annual_deaths_by_cause <- annual_deaths_by_cause %>% 
  select(Country = Entity, 2:26)

# colapsing all diseases into a new variables named disease

annual_deaths_cleaned <- annual_deaths_by_cause %>% 
  gather(key = "Cause", value = "Deaths", Meningitis,
         `Alzheimer's disease and other dementias`, `Parkinson's disease`,
         `Nutritional deficiencies`, Malaria, `Maternal disorders`, `HIV/AIDS`,
         `Drug use disorders`, Tuberculosis, `Cardiovascular diseases`,
         `Lower respiratory infections`, `Neonatal disorders`,
         `Alcohol use disorders`, `Diarrheal diseases`, Neoplasms,
         `Diabetes mellitus`, `Chronic kidney disease`, Poisonings, 
         `Road injuries`, `Chronic respiratory diseases`,
         `Cirrhosis and other chronic liver diseases`, `Digestive diseases`,
         `Acute hepatitis`, `Drowning`) %>% 
  mutate(Cause = as.factor(Cause))

# getting the country and continent name from the country data

glimpse(country)

country_cleaned <- country %>% 
  select(Country = Country_Name, Continent = Continent_Name)

# number of medical doctors dataset

glimpse(med_doc_per_10k_pop)

med_doc_per10k_pop_cleaned <- med_doc_per_10k_pop %>% 
  select(Country = Location, Continent = ParentLocation,
        Year = Period, MedDoctor_per10k_pop = FactValueNumeric) %>% 
  mutate(Year = as.numeric(Year))

glimpse(med_doc_per10k_pop_cleaned)

# number of deaths by age group
# collaspsing age group columns into a new variable

glimpse(deaths_by_age_grp)

deaths_by_age_grp_cleaned <- deaths_by_age_grp %>% 
  gather(key = Age_Group, value = Deaths, `Deaths 5-14 years`, `Deaths Under 5`,
         `Deaths Age: 15-49 years`, `Deaths 50-69 years`, `Deaths 70+ years`) %>%
  mutate(Age_Group = as.factor(Age_Group)) %>% 
  select(- Code, Country = Entity, Age_Group, Deaths)

# world population data 

glimpse(world_pop)

world_pop_cleaned <- world_pop %>% 
  select(Country = Entity, Year, Population =
           `Population (historical estimates)`) %>% 
  filter(Year > 2000) #filter for year greater than 1950

# current health expenditure
# remove irrelevant column names
# missing values will be dealt with later on

glimpse(current_health_xp)

health_expenditure <- current_health_xp %>% 
  gather(key = Year, value = Health_Expend_percentGDP, `2000`, `2001`, `2002`,
         `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`,
         `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`,) %>% 
  select(Country = `Country Name`, Year, Health_Expend_percentGDP) %>% 
  mutate(Year = as.numeric(Year))

glimpse(health_expenditure)


# remove all unwanted objects from workspace

remove(annual_deaths_by_cause)
remove(country)
remove(current_health_xp)
remove(deaths_by_age_grp)
remove(med_doc_per_10k_pop)
remove(world_pop)

# Exploratory data analysis
# visualising the total number of deaths by country

total.deaths.region <- annual_deaths_cleaned %>% 
  group_by(Country) %>% 
  filter(Country %in%
           c("African Region (WHO)", "European Region (WHO)", "North America (WB)",
             "South-East Asia Region (WHO)")) %>% 
  summarise(Total_death = sum(Deaths, na.rm = TRUE)
  ) %>% 
  arrange(desc(Total_death)) %>% 
  ggplot(aes(reorder(Country, Total_death), Total_death)) +
  geom_col(fill = "steelblue") +
  ylab("Death Count") +
  ggtitle("Deaths by Region/Continent") +
  coord_flip() +
  theme(
    legend.position = "none",
    axis.title = element_blank()
  )

total.deaths.region

# visualising the age group of death cases

agegrp.death.plot <- deaths_by_age_grp_cleaned %>% 
  filter(Year >= 2000) %>% 
  ggplot(aes(reorder(Age_Group, Deaths), Deaths)) +  
  geom_col(fill = "steelblue") +
  ylab("Deaths") +
  ggtitle("Deaths by Age Group in the World Since 2000") +
  theme(
    axis.title.x = element_blank(), 
    axis.text = element_text(size = 10)
  )


agegrp.death.plot

# Health Expenditure since 2000

health.xp.plot.trend <- health_expenditure %>% 
  inner_join(med_doc_per10k_pop_cleaned, by = c("Country", "Year")) %>% 
  select(Country, Continent, Health_Expend_percentGDP, MedDoctor_per10k_pop, Year) %>% 
  group_by(Year, Continent) %>% 
  summarise(MeanExpenditure = mean(Health_Expend_percentGDP, na.rm = TRUE)) %>% 
  ggplot(aes(Year, MeanExpenditure, color = Continent)) +
  geom_line(size = 0.8, alpha = 0.5) +
  ylab("% of GDP") +
  ggtitle("Mean Health Expenditure Across Continent") +
  theme(
    axis.title.x = element_blank(),
    axis.text = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = c(2000, 2004, 2008, 2012, 2016, 2020))

health.xp.plot.trend

# doctor population percentage of current populatiom

doc.pop <- med_doc_per10k_pop_cleaned %>% 
  inner_join(world_pop_cleaned, by = c("Country", "Year")) %>% 
  group_by(Year, Continent) %>% 
  summarise(
    Doctor_pop_percent = sum(round((MedDoctor_per10k_pop/Population) * 100, 2))
    ) %>% 
  ggplot(aes(Year, Doctor_pop_percent, colour = Continent)) +
  geom_line(size = 0.8, alpha = 0.5) +
  scale_x_continuous(breaks = c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024)) +
  scale_y_log10() +
  ggtitle("Doctor Population as Percentage of Population Across Continents Since 2000") +
  theme(axis.title = element_blank())

doc.pop

# exploring the leading causes of deaths 

leading.deaths <- annual_deaths_cleaned %>% 
  filter(Year >= 2000) %>% 
  group_by(Cause) %>% 
  summarise(TotalDeaths = sum(Deaths, na.rm = TRUE)) %>% 
  ggplot(aes(reorder(Cause, TotalDeaths),TotalDeaths)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme(
    axis.title = element_blank()
  ) +
  ggtitle("Leading Causes of Deaths in the World Since 2000")

leading.deaths

# Deaths by age group

agegrp.death.nigeria <- deaths_by_age_grp_cleaned %>%
  filter(Year >= 2000 & Country == "Nigeria") %>%
  ggplot(aes(Age_Group, Deaths)) +
  geom_col(fill = "steelblue") +
  ylab("Deaths") +
  ggtitle("Deaths by Age Group in Nigeria Since 2000") +
  theme(
    axis.title.x = element_blank(),
    axis.text = element_text(size = 10)
  )

agegrp.death.nigeria

# health expenditure in nigeria

health.xp.doc <- health_expenditure %>% 
  inner_join(med_doc_per10k_pop_cleaned, by = c("Country", "Year")) %>% 
  select(Country, Health_Expend_percentGDP, MedDoctor_per10k_pop, Year)


health.xp.plot.naija <- health.xp.doc %>% 
  group_by(Year) %>% 
  filter(Country == "Nigeria") %>% 
  summarise(MeanExpenditure = mean(Health_Expend_percentGDP, na.rm = TRUE)) %>% 
  ggplot(aes(Year, MeanExpenditure)) +
  geom_line(size = 0.8, alpha = 0.5) +
  ylab("% of GDP") +
  ggtitle("Mean Health Expenditure in Nigeria") +
  theme(
    axis.title.x = element_blank(),
    axis.text = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = c(2000, 2004, 2008, 2012, 2016, 2020))

health.xp.plot.naija

# doctor population percentage of current populatiom

doc.pop.naija <- health.xp.doc %>% 
  inner_join(world_pop_cleaned, by = c("Country", "Year")) %>% 
  filter(Country == "Nigeria") %>% 
  group_by(Year) %>%
  ggplot(aes(Year, MedDoctor_per10k_pop)) +
  geom_line(size = 0.8, alpha = 0.5) +
  scale_x_continuous(breaks = c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024)) +
  ggtitle("Doctor Population in Nigeria Since 2000") +
  theme(axis.title = element_blank()) +
  labs(subtitle = "Source: WHO")

doc.pop.naija

# exploring the leading causes of deaths 

leading.deaths <- annual_deaths_cleaned %>% 
  filter(Year >= 2000 & Country == "Nigeria") %>% 
  group_by(Cause) %>% 
  summarise(TotalDeaths = sum(Deaths, na.rm = TRUE)) %>% 
  ggplot(aes(reorder(Cause, TotalDeaths),TotalDeaths)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme(
    axis.title = element_blank()
  ) +
  ggtitle("Leading Causes of Deaths in Nigeria Since 2000")

leading.deaths
