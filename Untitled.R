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

??robustlmm

setwd('/Users/finnkruger/Documents/Hertie/Masterarbeit/Respog/other')

### levels of democracy

democracy_levels <- read_excel("p5v2018.xls")%>%
  select(country, year, polity2, democ)%>%
  rename(country2 = country)


democracy_levels$country2 <- democracy_levels$country2%>%
  case_match("United Kingdom" ~ "Great Britain","Korea South" ~ "South Korea", "Germany West" ~ "Germany", .default = democracy_levels$country2)


by <- join_by(year, country2)
data_master <- left_join(data_master, democracy_levels, by, multiple = "all")

check_dem <- data_master_democ%>%
  select(country2, polity2, democ)

hist(check_dem$democ)

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

#####

data_master <- data_master%>%
  mutate(IIP = 
    case_when(
      IIP_GDP > 0 ~ "positive",
      IIP_GDP < 0 ~ "negative",
    )
  )



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

## Model Financial openness

model1 <- lm(p50 ~ Financial_Open_Logged, data = data_master1)

mod <- lm(p95 ~ Financial_Open_Logged, data = data_master1)

cor(data_master1$IIP_GDP,data_master1$loggdpt)

select(case)
summary(mod)

stargazer(model1)

hist(data_master1$IIP_GDP)


### logging financial data


data_master$Financial_Open_Logged <- log(data_master$Financial_Open)
data_master$Foreign_share_logged <- log(data_master$Foreign_share)



hist(data_master$Foreign_share)
hist(data_master1$Financial_Open_Logged)

(data_master1$Financial_Open_Logged)


data_master1 <- data_master1%>%
  mutate(Rich_vs_Poor = p95-p05)
### 



### Taking care of Outliers 

data_master1 <- data_master1%>%
  filter(Financial_Open <= 10)
  
  
data_master1$Financial_Open

unique(data_master1$country2)

data_master1 <- data_master1%>%
  filter(country2 != 'Ireland' & country2 != 'Netherlands')


data_master1 <- data_master%>%
  filter(!is.na(dgentav14))%>%
  filter(dgentav14 >= -20)


data_master1 <- data_master1%>%
  filter(IIP == 'positive')

### Replicate Real but unequal responsiveness


original_model <- lmer(dgentav14 ~ p05 + p95+ gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master1, REML = FALSE)

original_model1 <- lmer(dgentav14 ~ p95+ gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master1, REML = FALSE)

original_model2 <- lmer(dgentav14 ~ p50 + gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master1, REML = FALSE)

original_model3 <- lmer(dgentav14 ~ p05 + gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master1, REML = FALSE)

summary(original_model3)

model_parameters(original_model3)

### Robust standard errors 

check_heteroskedasticity(original_model)

cstypes <- paste0("CR", c("0", "1", "1p", "1S", "2", "3"))
rob_se_fun <- function(type) sqrt(diag(vcovCR(original_model, type = type)))

rob_se <- sapply(cstypes, rob_se_fun)
std_se <- sqrt(diag(vcov(original_model)))
cbind(std = std_se, rob_se,
      merDeriv = sqrt(diag(sand)[1:2]))

coef_test(original_model, vcov = "CR1", p_values = TRUE, test = "naive-t")


### interacting with financial openness


model <- lmer(dgentav14 ~ p95*Financial_Open_Logged + IIP_GDP + gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master1, REML = FALSE)

model2 <- lmer(dgentav14 ~ p05*Financial_Open_Logged + IIP_GDP + gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master1, REML = FALSE)

model2 <- rlmer(dgentav14 ~ p05*Financial_Open_Logged + IIP_GDP + gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master1, REML = FALSE)

model3 <- lmer(dgentav14 ~ p50*Financial_Open_Logged + IIP_GDP + gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master1, REML = FALSE)

model4 <- lmer(dgentav14 ~ Rich_vs_Poor*Financial_Open_Logged + IIP_GDP +  gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master1, REML = FALSE)


###experimenting with other models 

model5 <- lmer(dgentav14 ~ p05*Financial_Open_Logged + Financial_Open_Logged*p95 +  gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master1, REML = FALSE)

model7 <- lmer(dgentav14 ~ p05 + p95 + Financial_Open_Logged +  gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master1, REML = FALSE)


#### Summary of models 

## Robust Standard Erorrs
RSE_Model <- vcovCR(model, type = "CR1")
RSE_Model2 <- vcovCR(model2, type = "CR1")
RSE_Model3 <- vcovCR(model3, type = "CR1")

### Checking models 

coef_test(model2, vcov = "CR1", p_values = TRUE, test = "naive-t", df = 20)
coef_test(model2, vcov = "CR1", p_values = TRUE, df = 20)


model_parameters(model, vcov = RSE_Model)
model_parameters(model2, vcov = RSE_Model2)

#### Creating Output table

cm <- c('(Intercept)' = 'Intercept',
        'p95' = 'Preferences of the richest 5%',
        'p05'    = 'Preferences of the poorest 5%',
        'p50' = 'Preferences of the median',
        'Financial_Open_Logged' = 'Logged Financial Openness',
        'IIP_GDP'  = 'International Investment Position',
        'p05*Financial_Open_Logged' = 'Preferences P95',
        'p50 × Financial_Open_Logged' = 'Preferences P50 x Logged Financial Openness')


modelsummary(model, vcov = RSE_Model, stars = TRUE)
modelsummary(models1, vcov = vcov, stars = TRUE, coef_map = cm)
             
?modelsummary

vcov = list(RSE_Model, RSE_Model2, RSE_Model3)
models1 = list(model, model2, model3)

### plot 

plot_predictions(model, rug = TRUE, condition = c("p95", "Financial_Open_Logged"))
plot_predictions(model2, rug = TRUE, condition = c("p05", "Financial_Open_Logged"))

plot_predictions(model4, rug = TRUE, condition = c("Rich_vs_Poor", "Financial_Open_Logged"))


plot_model(model4, type = "int", terms = c("Rich_vs_Poor", "Financial_Open_Logged"))


plot_predictions(model6, rug = TRUE, condition = c("p95", "Financial_Open_Logged"))


### Plotting Marginal Means

plot_model(model2, type = 'diag')

#plot_model(model, type = "pred", terms = c("p95", "Financial_Open_Logged"))

plot_model(model, type = "int", terms = c("p95", "Financial_Open_Logged"), show.data = TRUE) + 
  geom_point(data = data_master1, aes(x = p95, y = dgentav14, colour = Financial_Open_Logged), inherit.aes = FALSE) +
  scale_color_continuous()

plot_model(model2, type = "int", terms = c("p05", "Financial_Open_Logged"), show.data = TRUE) + 
  geom_point(data = data_master1, aes(x = p05, y = dgentav14, colour = Financial_Open_Logged), inherit.aes = FALSE) +
  scale_fill_gradient2("Financial_Open_Logged", limits = c(2, 2.5), 
                       low = "#762A83", mid = "white", high = "#1B7837") 



plot_model(model, type = "int", terms = c("p95", "Financial_Open_Logged"), show.data = TRUE)+ geom_rug(alpha = 1/2, position = "jitter")

?plot_model
plot_model(model2, type = "int", terms = c("p05", "Financial_Open_Logged"))+ geom_rug(alpha = 1/2, position = "jitter")

plot_model(model3, type = "int", terms = c("Financial_Open_Logged", "p50"))+ geom_rug()

plot_model(model3, type = "int", terms = c("Financial_Open_Logged", "p50"))+ geom_rug()

plot_model(model6, type = "int", terms = c("p50", "Financial_Open_Logged"))+ geom_rug()



#plot_model(model1, type = "pred", terms = c("Financial_Open_Logged", "p05"))

with(data_master1, scatter.smooth(year, dgentav14))


hist()

plot_model(model, type = "int", terms = c("p95", "Financial_Open_Logged")) + 
geom_rug(data = subset(data_master1, Financial_Open_Logged > 1), aes(x = Financial_Open_Logged,y = dgentav14), color = "black", sides = "b", , inherit.aes = FALSE ) 
  


### checking for diagnosis 


plot_model(model, type = "re", terms = c("Financial_Open_Logged"))

plot_model(model, type = "re")

#
plot_model(model2, type = 'slope', terms = c('Financial_Open_Logged')) + geom_rug()


plot_model(model, type = "diag", sort.est = '(Intercept)')

### checking for endogenetiy 
endogeneity_model <- lm(p95 ~ Financial_Open_Logged, data = data_master1)
endogeneity_model2 <- lm(dgentav14 ~ Financial_Open_Logged, data = data_master1)



summary(endogeneity_model)

## PLOTTING SLOPES AND COMPARISONS MARGINAL EFFECTS 


plot_slopes(model, variables = "p95", condition = c("Financial_Open_Logged"))

plot_slopes(model2, variables = "p05", vcov = "HC1", condition = c("Financial_Open_Logged"))
plot_slopes(model2, variables = "p05", condition = c("Financial_Open_Logged"))
plot_slopes(model3, variables = "p50", condition = c("Financial_Open_Logged"))

plot_slopes(model6, variables = "p95", condition = c("Financial_Open_Logged"))


avg_slopes(model2, variables = "p05", condition = c("Financial_Open_Logged"))

plot_comparisons().

plot_comparisons(model6, variables = "p95",  condition = c("Financial_Open_Logged"))


p <- comparisons(model, variables = "Financial_Open_Logged",  condition = c("p95"), newdata = datagrid(Financial_Open_Logged = c(1.9, 2)): Financial_Open_Logged)

print(p, style = "data.frame")
