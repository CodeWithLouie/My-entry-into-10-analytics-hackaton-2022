# load all required libraries

library(dplyr)

library(readr)

library(ggplot2)

library(ggthemes)

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
# create a ggplot template 

plottheme <- theme_minimal(
  base_size = 12,
  base_family = "serif",
) +
  theme(
    axis.title = element_blank(),
    line = element_blank()
  )

# visualising the total number of deaths by country

total.deaths.country <- annual_deaths_cleaned %>% 
  group_by(Country) %>% 
  filter(Country %in%
           c("African Region (WHO)", "European Region (WHO)", "North America (WB)",
             "South-East Asia Region (WHO)")) %>% 
  summarise(Total_death = sum(Deaths, na.rm = TRUE)) %>% 
  arrange(desc(Total_death)) %>% 
  ggplot(aes(Country, Total_death, fill = Country)) +
  geom_col() +
  ylab("Death Count") +
  ggtitle("Total Number of Deaths by Region/Continent") +
  coord_flip() +
  plottheme 



# visualising the age group of death cases

agegrp.death.plot <- deaths_by_age_grp_cleaned %>% 
  filter(Year >= 2000 & Country == "Nigeria") %>% 
  ggplot(aes(Age_Group, Deaths, fill = Age_Group)) +
  geom_col() +
  ylab("Deaths") +
  ggtitle("Number of Deaths by Age Group in Nigeria Since 2000") +
  plottheme

# Health Expenditure since 2000

healthxp.doc <- health_expenditure %>% 
  inner_join(med_doc_per10k_pop_cleaned, by = c("Country", "Year")) %>% 
  select(Country, Continent, Health_Expend_percentGDP, MedDoctor_per10k_pop, Year)

# expenditure trends

health.xp.plot.trend <- healthxp.doc %>%
  group_by(Year, Continent) %>% 
  summarise(MeanExpenditure = mean(Health_Expend_percentGDP, na.rm = TRUE)) %>% 
  ggplot(aes(Year, MeanExpenditure, color = Continent)) +
  geom_line() +
  ylab("Mean Health Expenditure") +
  ggtitle("Mean Health Expenditure as Percentage of GDP Across Continent") +
  plottheme

# Expenditure

health.xp.plot <- healthxp.doc %>%
  group_by(Continent) %>% 
  summarise(MeanExpenditure = mean(Health_Expend_percentGDP, na.rm = TRUE)) %>% 
  ggplot(aes(Continent, MeanExpenditure, fill = Continent)) +
  geom_col(fill = "dark red") +
  ylab("Mean Health Expenditure") +
  ggtitle("Mean Health Expenditure as Percentage of GDP Across Continent") +
  plottheme +
  theme(
    legend.position = "top"
  )
  

# doctor population plot

doc.plot <- healthxp.doc %>% 
  group_by(Continent) %>% 
  summarise(MeanDoctorPer10K = mean(MedDoctor_per10k_pop, na.rm = TRUE)) %>% 
  ggplot(aes(Continent, MeanDoctorPer10K, fill = Continent)) +
  geom_col() +
  ylab("Mean Doctor Population") +
  ggtitle("Mean Doctor Per 10k Population Across Continent") +
  plottheme +
  theme(
    legend.position = "top"
  )

# exploring the leading causes of deaths 

leading.deaths <- annual_deaths_cleaned %>% 
  group_by(Cause) %>% 
  summarise(TotalDeaths = sum(Deaths, na.rm = TRUE)) %>% 
  arrange(desc(TotalDeaths)) %>% 
  ggplot(aes(reorder(Cause, TotalDeaths),TotalDeaths)) +
  geom_col() +
  coord_flip() +
  plottheme



# to be coninued