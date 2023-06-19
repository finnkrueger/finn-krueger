library(haven)
library(tidyverse)
library(readxl)
library(nlme)
library(lme4)
library(marginaleffects)
library(sjPlot) 
library(sjlabelled)
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
library(kableExtra)
library(flextable)
library(labelled)




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


### logging financial data

data_master$Financial_Open <- data_master$Financial_Open*100


data_master$Financial_Open_Logged <- log(data_master$Financial_Open)
data_master$Foreign_share_logged <- log(data_master$Foreign_share)

data_master1 <- data_master%>%
  filter(!is.na(dgentav14))

##### GRAPHS

### Histogram on capital mobility

## create theme for graphs
My_Theme = theme(
  plot.title = element_text(size=26),
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 14),
  axis.text.y = element_text(size = 14),
  axis.title.y = element_text(size = 18),
  plot.caption = element_text(size= 14),
  legend.text = element_text(size= 16),
  legend.title = element_text(size= 16))

### Create histogram for Capital mobility in percentage

data_master$Financial_Open <- data_master$Financial_Open*100


ggplot(data_master, aes(Financial_Open)) +
  geom_histogram(color = "#000000", fill = "#0099F8", breaks = c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,2400,2500,2600)) +
  geom_vline(aes(xintercept = median(Financial_Open)), color = "#000000", size = 0.75) +
  scale_x_continuous(breaks=c(0,100,200,300,400,500,1000,2000, 2600)) +
  labs(title = 'Distribution of Capital Mobility between 1985 and 2008'
    ,caption = "Figure 2: Distribution of Capital Mobility (Source: External Wealth of Nation Database)")+
  xlab('Capital Mobility (as a share of GDP)')+ 
  My_Theme

data_master$Financial_Open <- data_master$Financial_Open/100  
### summary statistics

summary_stats <-  data_master1%>%
  select(p05,p50,p95,dgentav14,gent,Financial_Open_Logged,IIP_GDP,loggdpt,growtht,unempt)

var.labs <- data.frame(var = c('p05','p50',
                               'p95','dgentav14','gent',
                               'Financial_Open_Logged','IIP_GDP','loggdpt','growtht','unempt'),
                       labels = c('Preferences of the bottom 5%',
                                  'Preferences of the Median',
                                  'Preferences of the top 5%',
                                  'Averange Change in Welfare Generosity',
                                  'Total Welfare Generosity',
                                  'Captial Mobility Logged',
                                  'Net international investment Position (%GDP)',
                                  'Logged GDP',
                                  'GDP growth (%)',
                                  'Unemployment (%)'
                                ))


summary_statistics <- st(summary_stats, labels = var.labs, title = 'Table 1: Summary Statistics', out = 'kable', col.width=c(24,rep(10.5,15)))

summary_statistics%>%
  kable_styling()%>%
save_kable(file = 'summary_stats1.html')


### correlation between preferences
correlation <- summary_stats%>%
  select(p95,p05,p50)
  

datasummary_correlation(correlation, title = 'Correlation of the Preferences between different income groups', notes = 'P95 = Preferences of the richest 5 percent')
### Taking care of Outlier

data_master1 <- data_master%>%
  filter(!is.na(dgentav14))%>%
  filter(dgentav14 >= -20)

which(data_master1$dgentav14 <= -20)

ggplot(data_master, aes(dgentav14)) +
  geom_histogram(color = "#000000", fill = "#0099F8") 
  geom_vline(aes(xintercept = median(Financial_Open)), color = "#000000", size = 0.75) +
  scale_x_continuous(breaks=c(0,100,200,300,400,500,1000,2000, 2600)) +
  labs(title = 'Figure 2: Distribution of Capital Mobility',
       caption = "Figure 2: Distribution of Capital Mobility (Source: External Wealth of Nation Database)")+
  xlab('Capital Mobility (as a share of GDP)')

### Replicate Real but unequal responsiveness


original_model <- lmer(dgentav14 ~ p05 + p95+ gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master, REML = FALSE)

original_model1 <- lmer(dgentav14 ~ p95+ gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master1, REML = FALSE)

original_model2 <- lmer(dgentav14 ~ p50 + gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master1, REML = FALSE)

original_model3 <- lmer(dgentav14 ~ p05 + gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master1, REML = FALSE)


### Robust standard errors 

check_heteroskedasticity(original_model)

cstypes <- paste0("CR", c("0", "1", "1p", "1S", "2", "3"))
rob_se_fun <- function(type) sqrt(diag(vcovCR(original_model, type = type)))

rob_se <- sapply(cstypes, rob_se_fun)
std_se <- sqrt(diag(vcov(original_model)))
cbind(std = std_se, rob_se_fun,
      merDeriv = sqrt(diag(sand)[1:2]))

coef_test(original_model, vcov = "CR1", p_values = TRUE, test = "naive-t")


### interacting with financial openness


model <- lmer(dgentav14 ~ p95*Financial_Open_Logged + IIP_GDP + gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master1, REML = FALSE)

model_within <- lm(dgentav14 ~ p95*Financial_Open_Logged + IIP_GDP + gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + as.factor(country), data = data_master1)

model2 <- lmer(dgentav14 ~ p05*Financial_Open_Logged + IIP_GDP + gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master1, REML = FALSE)

model3 <- lmer(dgentav14 ~ p50*Financial_Open_Logged + IIP_GDP + gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master1, REML = FALSE)



#### Summary of models 

## Robust Standard Erorrs
RSE_Model <- vcovCR(model, type = "CR1")
RSE_Model2 <- vcovCR(model2, type = "CR1")
RSE_Model3 <- vcovCR(model3, type = "CR1")


### Checking models 
model_parameters(model, vcov = RSE_Model)
model_parameters(model2, vcov = RSE_Model2)
model_parameters(model5, vcov = RSE_Model5)


vcov = list(RSE_Model, RSE_Model2, RSE_Model3)
models1 = list(model, model2, model3)

#### Creating Output table

cm <- c('p95' = 'Preferences of the richest 5% (P95)',
        'p05'    = 'Preferences of the poorest 5% (P05)',
        'p50' = 'Preferences of the median (P50)',
        'Financial_Open_Logged' = 'Capital Mobility Logged',
        'IIP_GDP'  = 'Net International Investment Position',
        'p95:Financial_Open_Logged' = 'Preferences P95 x Capital Mobility Logged',
        'p50:Financial_Open_Logged' = 'Preferences P50 x Capital Mobility Logged',
        'p05:Financial_Open_Logged' = 'Preferences P05 x Capital Mobility Logged',
        'gent' = 'Total Welfare Generosity (t)',
        'loggdpt' = 'Logged GDP (t)',
        'growtht' = 'Growth (t)',
        'unempt' = 'Unemployment (t)',
        'factor(topic)2' = 'Pension Policy (Reference = Health)',
        'factor(topic)3' = 'Unemployment Policy (Reference = Health)',
        'factor(wave)2' = 'Wave 2 (Reference = Wave1)',
        'factor(wave)3' = 'Wave 3 (Reference = Wave1)',
        'factor(wave)4' = 'Wave 4 (Reference = Wave1)',
        '(Intercept)' = 'Intercept')
      
  
  
full_model_html <- modelsummary(models1, vcov = vcov, stars = TRUE, coef_map = cm, notes = NULL,
                                title = 'Random Intercept Models of Changes in Welfare State Generosity 
             (Average Change from T + 1 to T + 4 relative to T).',
                                output = 'flextable') %>% 
  save_as_docx(path = "mytable2.docx")

modelsummary(models1, vcov = vcov, stars = TRUE, coef_map = cm, notes = NULL,
                                title = 'Random Intercept Models of Changes in Welfare State Generosity 
             (Average Change from T + 1 to T + 4 relative to T).')
                                

  
## visualise the log transform for interpretability

data_master1 %>%
  ggplot(aes(x=Financial_Open, y=Financial_Open_Logged)) +
  geom_line() +
  scale_y_continuous(name = "Capital Mobility Logged") +
  scale_x_continuous(name = 'Capital Mobility (Factor to GDP)')  +
  labs(title = 'Log transform of Capital Mobility (Factor to GDP)',
       caption = 'Figure 6') +
  My_Theme






#Plot the model 

predicted_p95 <- plot_model(model, type = "int", terms = c("p95", "Financial_Open_Logged"), show.data = TRUE, vcov.fun = RSE_Model, legend.title="Capital Mobility Logged", title = '') + 
  geom_point(data = data_master1, aes(x = p95, y = dgentav14, colour = Financial_Open_Logged), inherit.aes = FALSE) +
  scale_color_continuous() + My_Theme

predicted_p05 <- plot_model(model2, type = "int", terms = c("p05", "Financial_Open_Logged"), show.data = TRUE, vcov.fun = RSE_Model2, legend.title="Capital Mobility Logged", title = '') + 
  geom_point(data = data_master1, aes(x = p05, y = dgentav14, colour = Financial_Open_Logged), inherit.aes = FALSE)  +
  scale_color_continuous() + My_Theme

predicted_p50 <- plot_model(model3, type = "int", terms = c("p50", "Financial_Open_Logged"), show.data = TRUE, vcov.fun = RSE_Model3, legend.title="Capital Mobility Logged", title = '') + 
  geom_point(data = data_master1, aes(x = p50, y = dgentav14, colour = Financial_Open_Logged), inherit.aes = FALSE)  +
  scale_color_continuous() + My_Theme



predictions <- ggarrange(predicted_p95, predicted_p05, predicted_p50, ncol = 3, common.legend = TRUE, legend="bottom") 
  
                      
annotate_figure(predictions,
                top =  text_grob('Predicted Values of Average Change in Generosity (t+1-t+4)', size = 20),
                bottom = text_grob('Figure 4: Predicted Values of Change in Generosity (data points represent the actual data) | Source: authors elaboration', hjust = 1.1))


### checking for endogenetiy 


endogeneity_model <- lm(p95 ~ Financial_Open_Logged, data = data_master1)
endogeneity_model2 <- lm(p05 ~ Financial_Open_Logged, data = data_master1)
endogeneity_model3 <- lm(p50 ~ Financial_Open_Logged, data = data_master1)

endogeneity_models = list(endogeneity_model, endogeneity_model2, endogeneity_model3)


summary(endogeneity_model)
summary(endogeneity_model2)
summary(endogeneity_model3)

modelsummary(endogeneity_models, stars = TRUE, coef_map = cm, notes = NULL,
             title = 'Table4: Exogenous Preferences',
             output = 'flextable')


## PLOTTING SLOPES AND COMPARISONS MARGINAL EFFECTS 


My_Theme_slopes = theme(
  plot.title = element_text(size=14),
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.text.y = element_text(size = 14),
  axis.title.y = element_text(size = 16),
  plot.caption = element_text(size= 12),)


slopes_p95 <- plot_slopes(model, variables = "p95", vcov = RSE_Model, condition = c("Financial_Open_Logged")) +
  scale_y_continuous(name = "Coefficient Size",
                     breaks=c(0.00,0.02,0.04,0.06,0.08,0.1),
                     limits=c(-0.05,0.135)) +
  scale_x_continuous(name = 'Logged Capital Mobility') +
  labs(title = "Preferences of the richest 5 %") + 
  My_Theme_slopes


slopes_p05 <- plot_slopes(model2, variables = "p05", vcov = RSE_Model2, condition = c("Financial_Open_Logged"))+
  scale_y_continuous(name = "Coefficient Size",
                     breaks=c(0.00,0.02,0.04,0.06,0.08,0.1),
                     limits=c(-0.055,0.135)) +
  scale_x_continuous(name = 'Logged Capital Mobility') +
  labs(title = "Preferences of the poorest 5 %")    + 
  My_Theme_slopes

slopes_p50 <- plot_slopes(model3, variables = "p50", vcov = RSE_Model3, condition = c("Financial_Open_Logged"))+
  scale_y_continuous(name = "Coefficient Size",
                     breaks=c(0.00,0.02,0.04,0.06,0.08,0.1),
                     limits=c(-0.05,0.135)) +
  scale_x_continuous(name = 'Logged Capital Mobility')  +
  labs(title = 'Preferences of the Median')     + 
  My_Theme_slopes



slopes <- grid.arrange(slopes_p95, slopes_p05, slopes_p50, nrow = 1,
                            top = textGrob("Marginal Slopes of the Preferences dependent on level of Capital Mobility", gp=gpar(fontsize=25,font=8)), 
                            bottom = textGrob('Figure3: How capital mobility affects the influence of different income groups in determining changes in welfare genersoity |  Source: authors elaboration',hjust=0.85))  + 
  My_Theme_slopes
                            


### checking for diagnosis of original model. 

plot_model(original_model, type = "diag", sort.est = '(Intercept)')

### checking for diagnosis of adjusted model 

plot_model(model, type = "diag", sort.est = '(Intercept)')    

plot_model(model2, type = "diag", sort.est = '(Intercept)')

plot_model(model3, type = "diag", sort.est = '(Intercept)')



plot_model(model, type = "resid")


#############Recreate the Regression including the outlier




outlier_model <- lmer(dgentav14 ~ p95*Financial_Open_Logged + IIP_GDP + gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master, REML = FALSE)


outlier_model2 <- lmer(dgentav14 ~ p05*Financial_Open_Logged + IIP_GDP + gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master, REML = FALSE)

outlier_model3 <- lmer(dgentav14 ~ p50*Financial_Open_Logged + IIP_GDP + gent + loggdpt + growtht + unempt + factor(topic) + factor(wave) + (1 | country), data = data_master, REML = FALSE)


#### Summary of models 

## Robust Standard Erorrs
RSE_Model_outlier <- vcovCR(outlier_model, type = "CR1")
RSE_Model_outlier2 <- vcovCR(outlier_model2, type = "CR1")
RSE_Model_outlier3 <- vcovCR(outlier_model3, type = "CR1")


### Checking models 
model_parameters(outlier_model, vcov = RSE_Model_outlier)
model_parameters(outlier_model2, vcov = RSE_Model_outlier2)
model_parameters(outlier_model3, vcov = RSE_Model_outlier3)


vcov_outlier = list(RSE_Model, RSE_Model2, RSE_Model3)
models1_outlier = list(outlier_model, outlier_model2, outlier_model3)

#### Creating Output table

cm <- c('p95' = 'Preferences of the richest 5% (P95)',
        'p05'    = 'Preferences of the poorest 5% (P05)',
        'p50' = 'Preferences of the median (P50)',
        'Financial_Open_Logged' = 'Capital Mobility Logged',
        'IIP_GDP'  = 'Net International Investment Position',
        'p95:Financial_Open_Logged' = 'Preferences P95 x Capital Mobility Logged',
        'p50:Financial_Open_Logged' = 'Preferences P50 x Capital Mobility Logged',
        'p05:Financial_Open_Logged' = 'Preferences P05 x Capital Mobility Logged',
        'gent' = 'Total Welfare Generosity (t)',
        'loggdpt' = 'Logged GDP (t)',
        'growtht' = 'Growth (t)',
        'unempt' = 'Unemployment (t)',
        'factor(topic)2' = 'Pension Policy (Reference = Health)',
        'factor(topic)3' = 'Unemployment Policy (Reference = Health)',
        'factor(wave)2' = 'Wave 2 (Reference = Wave1)',
        'factor(wave)3' = 'Wave 3 (Reference = Wave1)',
        'factor(wave)4' = 'Wave 4 (Reference = Wave1)',
        '(Intercept)' = 'Intercept')



modelsummary(models1_outlier, vcov = vcov_outlier, stars = TRUE, coef_map = cm, notes = NULL,
                                title = 'Random Intercept Models of Changes in Welfare State Generosity when Outlier included
             (Average Change from T + 1 to T + 4 relative to T).',
             output = 'flextable') %>% 
  save_as_docx(path = "Random_Intercept_Outlier.docx")





