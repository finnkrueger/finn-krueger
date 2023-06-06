library(haven)
library(tidyverse)
library(readxl)
library(nlme)
library(lme4)
library(marginaleffects)
library(sjPlot)
library(stargazer)
library(texreg)
library(ggplot2)
library(robustlmm)
library(sandwich)
library(clubSandwich)
library(lmtest)
library(parameters)
library(performance)
library(merDeriv)
library(modelsummary)
library(jtools)
library(vtable)



??robustlmm

setwd('/Users/finnkruger/Documents/Hertie/Masterarbeit/Respog/other')

### levels 

# Financial Openness 
my_data <- read_excel("EWN-dataset_12-2022.xlsx",
  sheet = "Dataset")

# Select What is needed & Rename Coloumns to Match
financial_openness <- my_data%>%
  select(Country,Year,`Financial Openness`, Foreign_share, IIP_GDP)%>%
  filter(`Financial Openness` != 'na')%>%
  filter(IIP_GDP != 'na')%>%
  rename(country2 = Country,
         Financial_Open = `Financial Openness`,
         year = Year)

countries <- financial_openness%>%
filter(country2 %in% data_master1$country2)




filter

#Rename Entries to Match 
financial_openness$country2 <- financial_openness$country2%>%
  case_match("United Kingdom" ~ "Great Britain","Korea" ~ "South Korea", .default = financial_openness$country2)

#### join the data 

data2 <- read_dta('SBH_P&S_Data.dta')


data_Schakel <- read_dta('issp.dta')
colnames(data_Schakel)

table(data_master$wave)

by <- join_by(year, country2)
data_master <- left_join(data2, financial_openness, by, multiple = "all")




#### Group per Country and per Year
checking <-data_master%>%
  select(country2, year, Financial_Open, dgentav14)%>%
  print()


checking1 <-data_master%>%
  select(country2, year, Financial_Open)%>%
  group_by(country2)%>%
  summarise(Financial_O = mean(Financial_Open))%>%
  arrange(desc(Financial_O))%>%
  print()

checking2 <-data_master1%>%
  select(country2, year, Financial_Open)%>%
  group_by(year)%>%
  summarise(Financial_O = mean(Financial_Open))%>%
  arrange(desc(Financial_O))%>%
  print()


## for foreign Share 

checking_IIP_GDP <-data_master%>%
  select(country2, year, IIP_GDP)%>%
  print()


checking1_foreign <-data_master%>%
  select(country2, year, Foreign_share)%>%
  group_by(country2)%>%
  summarise(Financial_O = mean(Foreign_share))%>%
  arrange(desc(Financial_O))%>%
  print()

checking2_foreign <-data_master%>%
  select(country2, year, Foreign_share)%>%
  group_by(year)%>%
  summarise(Financial_O = mean(Foreign_share))%>%
  arrange(desc(Financial_O))%>%
  print()


### take percent

data_master1$Financial_Open <- data_master1$Financial_Open*100
data_master1$IIP_GDP <- data_master1$IIP_GDP*100

##### GRAPHS

### Histogram on capital mobility

ggplot(data_master1, aes(Financial_Open)) +
  geom_histogram(color = "#000000", fill = "#0099F8", breaks = c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,2400,2500,2600)) +
  geom_vline(aes(xintercept = median(Financial_Open)), color = "#000000", size = 0.75) +
  scale_x_continuous(breaks=c(0,100,200,300,400,500,1000,2000, 2600)) +
  labs(caption = "Figure 2: Distribution of Capital Mobility (Source: External Wealth of Nation Database)")+
  xlab('Capital Mobility (as a share of GDP)')
  
### summary statistics

?vtable


summary_stats <-  data_master1%>%
  select(p05,p50,p95,dgentav14,gent,Financial_Open_Logged,IIP_GDP,loggdpt,growtht,unempt,topic,country)

var.labs <- data.frame(var = c('p05','p50',
                               'p95','dgentav14','gent',
                               'Financial_Open_Logged','IIP_GDP','loggdpt','growtht','unempt'),
                       labels = c('Preferences of the bottom 5%',
                                  'Preferences of the Median',
                                  'Preferences of the top 5%',
                                  'Averange Change in Welfare Generosity',
                                  'Welfare Generosity',
                                  'Captial Mobility Logged',
                                  'Net international investment Position (% of GDP)',
                                  'Logged GDP',
                                  'GDP growth',
                                  'Unemployment'
                                ))
  
?st

st(summary_stats, labels = var.labs, title = 'Table 1: Summary Statistics')
st(summary_stats, labels = var.labs, title = 'Table 1: Summary Statistics', note = 'Table 1: Summary Statistics' out = 'csv',file = "/Users/finnkruger/Documents/Hertie/Masterarbeit/Respog/other/Graphs & Tables")

####


res <- cor(data_master1$dg)
round(res, 2)
