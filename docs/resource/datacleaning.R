library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(DT)
library(plotly)

library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(xgboost)
library(xgboost)
library(caret)


GDP                <- read.csv(paste0(fpath, "GDP.csv"))
Population         <- read.csv(paste0(fpath, "Population.csv"))
Population.Working <- read.csv(paste0(fpath, "WorkingAgePopulation.csv"))
EmploymentRate     <- read.csv(paste0(fpath, "EmploymentRate.csv"))
Inflation          <- read.csv(paste0(fpath, "Inflation.csv"))
HousingPrices      <- read.csv(paste0(fpath, "HousingPrices.csv"))
SharePrices        <- read.csv(paste0(fpath, "SharePrices.csv"))
InterestRate       <- read.csv(paste0(fpath, "ShortTermInterestRate.csv"))
HouseholdIncome    <- read.csv(paste0(fpath, "HouseHoldIncome.csv"))
HouseholdDebt      <- read.csv(paste0(fpath, "HouseHoldDebt.csv"))
HouseholdSavings   <- read.csv(paste0(fpath, "HouseHoldSavings.csv"))
HouseholdSpending  <- read.csv(paste0(fpath, "HouseHoldSpending.csv"))


# Only use annual stats
EmploymentRate <- EmploymentRate %>%
  filter(FREQUENCY == 'A') %>%
  filter(MEASURE == 'PC_WKGPOP') %>%
  filter(SUBJECT == 'TOT')

HouseholdIncome <- HouseholdIncome %>%
  filter(FREQUENCY == 'A') %>%
  filter(MEASURE == 'AGRWTH') %>%
  filter(SUBJECT == 'NET')

HousingPrices <- HousingPrices %>%
  filter(FREQUENCY == 'A') %>%
  filter(SUBJECT == 'RENT')

Inflation <- Inflation %>%
  filter(FREQUENCY == 'A') %>%
  filter(SUBJECT == 'TOT') %>%
  filter(MEASURE == 'AGRWTH')

HouseholdSpending <- HouseholdSpending %>%
  filter(SUBJECT == 'TOT') %>%
  filter(MEASURE == 'PC_GDP')

PopulationGrowth <- Population %>%
  filter(SUBJECT == 'TOT') %>%
  filter(MEASURE == 'AGRWTH')

Population <- Population %>%
  filter(SUBJECT == 'TOT') %>%
  filter(MEASURE == 'MLN_PER')

SharePrices <- SharePrices %>%
  filter(FREQUENCY == 'A')

InterestRate <- InterestRate %>%
  filter(FREQUENCY == 'A')

GDP.raw <- GDP %>%
  filter(MEASURE == 'MLN_USD')

GDP.capita <- GDP %>%
  filter(MEASURE == 'USD_CAP')

# Cleanup data a bit
GDP.raw <- GDP.raw %>%
  select(c("LOCATION", "TIME", "Value")) %>%
  filter(nchar(TIME) == 4) %>%
  mutate(Country.Code=LOCATION,
         Year=as.integer(TIME),
         GDP=Value) %>%
  select(c("Country.Code", "Year", "GDP"))

GDP.capita <- GDP.capita %>%
  select(c("LOCATION", "TIME", "Value")) %>%
  filter(nchar(TIME) == 4) %>%
  mutate(Country.Code=LOCATION,
         Year=as.integer(TIME),
         GDPPerCapita=Value) %>%
  select(c("Country.Code", "Year", "GDPPerCapita"))

EmploymentRate <- EmploymentRate %>%
  select(c("LOCATION", "TIME", "Value")) %>%
  filter(nchar(TIME) == 4) %>%
  mutate(Country.Code=LOCATION,
         Year=as.integer(TIME),
         EmploymentRate=Value) %>%
  select(c("Country.Code", "Year", "EmploymentRate"))

HouseholdDebt <- HouseholdDebt %>%
  select(c("LOCATION", "TIME", "Value")) %>%
  filter(nchar(TIME) == 4) %>%
  mutate(Country.Code=LOCATION,
         Year=as.integer(TIME),
         HouseholdDebt=Value) %>%
  select(c("Country.Code", "Year", "HouseholdDebt"))

HouseholdIncome <- HouseholdIncome %>%
  select(c("LOCATION", "TIME", "Value")) %>%
  filter(nchar(TIME) == 4) %>%
  mutate(Country.Code=LOCATION,
         Year=as.integer(TIME),
         HouseholdIncome=Value) %>%
  select(c("Country.Code", "Year", "HouseholdIncome"))

HouseholdSavings <- HouseholdSavings %>%
  select(c("LOCATION", "TIME", "Value")) %>%
  filter(nchar(TIME) == 4) %>%
  mutate(Country.Code=LOCATION,
         Year=as.integer(TIME),
         HouseholdSavings=Value) %>%
  select(c("Country.Code", "Year", "HouseholdSavings"))

HouseholdSpending <- HouseholdSpending %>%
  select(c("LOCATION", "TIME", "Value")) %>%
  filter(nchar(TIME) == 4) %>%
  mutate(Country.Code=LOCATION,
         Year=as.integer(TIME),
         HouseholdSpending=Value) %>%
  select(c("Country.Code", "Year", "HouseholdSpending"))

HousingPrices <- HousingPrices %>%
  select(c("LOCATION", "TIME", "Value")) %>%
  filter(nchar(TIME) == 4) %>%
  mutate(Country.Code=LOCATION,
         Year=as.integer(TIME),
         HousingPrices=Value) %>%
  select(c("Country.Code", "Year", "HousingPrices"))

Inflation <- Inflation %>%
  select(c("LOCATION", "TIME", "Value")) %>%
  filter(nchar(TIME) == 4) %>%
  mutate(Country.Code=LOCATION,
         Year=as.integer(TIME),
         Inflation=Value) %>%
  select(c("Country.Code", "Year", "Inflation"))

InterestRate <- InterestRate %>%
  select(c("LOCATION", "TIME", "Value")) %>%
  filter(nchar(TIME) == 4) %>%
  mutate(Country.Code=LOCATION,
         Year=as.integer(TIME),
         InterestRate=Value) %>%
  select(c("Country.Code", "Year", "InterestRate"))

Population <- Population %>%
  select(c("LOCATION", "TIME", "Value")) %>%
  filter(nchar(TIME) == 4) %>%
  mutate(Country.Code=LOCATION,
         Year=as.integer(TIME),
         Population=Value) %>%
  select(c("Country.Code", "Year", "Population"))

Population.Working <- Population.Working %>%
  select(c("LOCATION", "TIME", "Value")) %>%
  filter(nchar(TIME) == 4) %>%
  mutate(Country.Code=LOCATION,
         Year=as.integer(TIME),
         WorkingPopulation=Value) %>%
  select(c("Country.Code", "Year", "WorkingPopulation"))

PopulationGrowth <- PopulationGrowth %>%
  select(c("LOCATION", "TIME", "Value")) %>%
  filter(nchar(TIME) == 4) %>%
  mutate(Country.Code=LOCATION,
         Year=1+as.integer(TIME),
         PopulationGrowth=Value) %>%
  select(c("Country.Code", "Year", "PopulationGrowth"))

SharePrices <- SharePrices %>%
  select(c("LOCATION", "TIME", "Value")) %>%
  filter(nchar(TIME) == 4) %>%
  mutate(Country.Code=LOCATION,
         Year=as.integer(TIME),
         SharePrices=Value) %>%
  select(c("Country.Code", "Year", "SharePrices"))


# Obtain GDP growth
GDP.t1 <- GDP.raw %>%
  mutate(Year=Year+1, OldGDP=GDP) %>%
  select(c('Country.Code', 'Year', 'OldGDP'))

GDPgrowth <- merge(x=GDP.raw, y=GDP.t1, by=c('Country.Code', 'Year'), all.x=T) %>%
  mutate(GDPGrowth=100*(GDP/OldGDP)-100) %>%
  select(c('Country.Code', 'Year', 'GDPGrowth'))


# Share price and Rent price change
SharePrices1 <- SharePrices %>%
  mutate(Year=Year+1, OldPrice=SharePrices) %>%
  select(c('Country.Code', 'Year', 'OldPrice'))
HousingPrices1 <- HousingPrices %>%
  mutate(Year=Year+1, OldPrice=HousingPrices) %>%
  select(c('Country.Code', 'Year', 'OldPrice'))

SharePrices <- merge(x=SharePrices, y=SharePrices1, by=c('Country.Code', 'Year'), all.x=T) %>%
  mutate(ShareGrowth=100*SharePrices/OldPrice-100) %>%
  select(c('Country.Code', 'Year', 'ShareGrowth'))
HousingPrices <- merge(x=HousingPrices, y=HousingPrices1, by=c('Country.Code', 'Year'), all.x=T) %>%
  mutate(RentGrowth=100*HousingPrices/OldPrice-100) %>%
  select(c('Country.Code', 'Year', 'RentGrowth'))

# Add next year's inflation as target
inflationPred <- Inflation %>%
  select(c('Country.Code', 'Year', 'Inflation')) %>%
  mutate(Year=Year-1, Target=Inflation) %>%
  select(c('Country.Code', 'Year', 'Target'))  
  
  
# merge data
df <- merge(x=GDP.raw, y=GDP.capita,   by=c("Country.Code", "Year"), all.x=T)
df <- merge(x=df, y=GDPgrowth,         by=c("Country.Code", "Year"), all.x=T)
df <- merge(x=df, y=EmploymentRate,    by=c("Country.Code", "Year"), all.x=T)
df <- merge(x=df, y=HouseholdSavings,  by=c("Country.Code", "Year"), all.x=T)
df <- merge(x=df, y=HouseholdSpending, by=c("Country.Code", "Year"), all.x=T)
df <- merge(x=df, y=HousingPrices,     by=c("Country.Code", "Year"), all.x=T)
df <- merge(x=df, y=SharePrices,       by=c("Country.Code", "Year"), all.x=T)
df <- merge(x=df, y=Inflation,         by=c("Country.Code", "Year"), all.x=T)
df <- merge(x=df, y=InterestRate,      by=c("Country.Code", "Year"), all.x=T)
df <- merge(x=df, y=Population,        by=c("Country.Code", "Year"), all.x=T)
df <- merge(x=df, y=PopulationGrowth,  by=c("Country.Code", "Year"), all.x=T)
df <- merge(x=df, y=Population.Working,by=c("Country.Code", "Year"), all.x=T)
df <- merge(x=df, y=inflationPred, by=c('Country.Code', 'Year'), all.x=T)

# df %>%
#   filter(is.na(Inflation)) %>%
#   group_by(Country.Code) %>%
#   summarise(counts=n())

df <- df %>%
  drop_na()

df[,3:4] <- round(df[,3:4], 0)
df[,5:13] <- round(df[,5:13], 2)


# Split train and test
train <- df %>%
  filter(Year<=1995+19) %>%
  select(c('GDP', 'GDPPerCapita', 'EmploymentRate', 'HouseholdSavings',
           'HouseholdSpending', 'RentGrowth', 'ShareGrowth', 'Inflation',
           'InterestRate', 'PopulationGrowth', 'GDPGrowth', 'Target')) %>%
  drop_na()

test <- df %>%
  filter(Year>1995+19) %>%
  select(c('GDP', 'GDPPerCapita', 'EmploymentRate', 'HouseholdSavings',
           'HouseholdSpending', 'RentGrowth', 'ShareGrowth', 'Inflation',
           'InterestRate',  'PopulationGrowth', 'GDPGrowth', 'Target')) %>%
  drop_na()