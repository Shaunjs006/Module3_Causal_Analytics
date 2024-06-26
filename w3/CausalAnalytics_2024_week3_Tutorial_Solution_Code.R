######################################################################################################################################################################
#######################  Melbourne Business School- Practice Questions - Causal Analytics - Class 4 2024 Nico Neumann   ##############################################
######################################################################################################################################################################
## clean memory
rm(list=ls()) 


## (1) Define the packages that will be needed
packages <- c( 'tidyverse','lubridate', 'lmtest',  'pwr2','effectsize')

## (2) Install them if not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
######################################################################################################################################################################


foldername <- "C:\\Users\\n.neumann" 
#foldername <- "C:\\Users\\Nico" 
setwd(paste0(foldername,"\\OneDrive\\2024_Causal_Analytics_TAs\\2024_Causal_Class3"))


#################### load libraries (install first if necessary)
library(tidyverse)
library(lubridate)
library(lmtest)
library(effectsize)
library(pwr2)

# increase print out
options(max.print=1000000)
## avoid scientific notation
options(scipen=99)

######################################################################################################################################################################
###########################################      Exercises A + B: Randomised experiment - earnings and job training     ##############################################
######################################################################################################################################################################

## load data
training.df <- read.csv("jobtraining.csv" )

## data exploration
# Columns
head(training.df)


# Summary (is there any zero?)
summary(training.df$earnings)

################# A: Analysis step 1: Check covariate balance ###########################
## a) Sample size per treatment group
table(training.df$treatment)


# b) Married
training.df %>% group_by(treatment,married) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3)) %>% 
  arrange(desc(prop)) #  slightly different proportions

# black
training.df %>% group_by(treatment,black) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3)) %>% 
  arrange(desc(prop)) # slightly different proportions

# Hispanic
training.df %>% group_by(treatment, hisp) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3)) %>% 
  arrange(desc(prop))  # quite different proportions

# For continuous variables, we can compare the mean for each treatment level
training.df %>% group_by(treatment) %>% summarize(mean(educ))

# However, better to treat education as a discrete = factor variable
edusummary=training.df %>% group_by(treatment,factor(educ)) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3)) %>% 
  arrange(desc(prop)) # quite different proportions
print(edusummary,n=Inf)

# For continuous variables, we can compare the mean for each treatment level
training.df %>% group_by(treatment) %>% summarize(mean(age))

# For continuous variables, we can compare the mean for each treatment level
training.df %>% group_by(treatment) %>% summarize(mean(earnings_past1))

# For continuous variables, we can compare the mean for each treatment level
training.df %>% group_by(treatment) %>% summarize(mean(earnings_past2))

# Unemployment
training.df %>% group_by(treatment,unemployed) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3)) %>% 
  arrange(desc(prop))  # quite different proportions


## c)
# Correlation
cor_matrix=round(cor(training.df %>% select_if(is.numeric)),2)


################# B: Analysis step 2: mean differences  ######################### 
# a) Let's plot the outcome variable
hist(training.df$earnings)
hist(log(training.df$earnings))   ## looks much more normal, let's use this hoping residuals may be more likely to be normally distributed too

## b) t-test
t.test(log(earnings+1) ~ treatment, data = training.df) 


## c) We can use OLS and obtain the same result, but we can see the causal effect of treatment
ATE <- lm(log(earnings+1) ~  treatment , data = training.df)
summary(ATE)  


## d) We can add further control variables to increase precision and address the sampling imbalance.
ATE.adjusted.log <- lm(log(earnings+1) ~ treatment+ age + factor(educ) + black + hisp + married  +earnings_past2 + earnings_past1 + unemployed, data = training.df)
summary(ATE.adjusted.log) 
AIC(ATE.adjusted.log) # 1328.019

## e) let's log-transform our continuous input variables
ATE.adjusted.log2 <- lm(log(earnings+1) ~ treatment+ log(age) + factor(educ) + black + hisp + married  +log(earnings_past2+1) + log(earnings_past1+1) + unemployed, data = training.df)
summary(ATE.adjusted.log2)  
AIC(ATE.adjusted.log2)  # 1327.639

## f)
qqnorm(ATE.adjusted.log2$residuals)  
qqline(ATE.adjusted.log2$residuals)  ## does not look normal
bptest(ATE.adjusted.log2)  ## Careful: We have heteroscedasticity


## g)
ATE.adjusted <- lm(earnings ~treatment+ age + factor(educ) + black + hisp + married  +earnings_past2 + earnings_past1 + unemployed, data = training.df)
summary(ATE.adjusted)  

qqnorm(ATE.adjusted$residuals)  
qqline(ATE.adjusted$residuals)  ## does still not look normal (so careful with p-values/ SEs)
bptest(ATE.adjusted)   # but we now have homoscedasticity



######################################################################################################################################################################
###########################################         Exercise C: Randomised experiment - online store promotion          ##############################################
######################################################################################################################################################################

## load data
promotion.df <- read.csv("online_store_promo.csv")

## check data
head(promotion.df )

## We can check the distribution
hist(promotion.df$Sales)  ## seems slightly more normal (again, only residuals matter)
hist(log(promotion.df$Sales))

## a) check balance of factors
table(promotion.df$Promotion,promotion.df$Newsletter)
# Alternative:
promotion.df %>% group_by(Promotion, Newsletter) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3))

## b) Boxplot graphs
boxplot(promotion.df$Sales ~ promotion.df$Promotion)
boxplot(Sales ~ factor(Newsletter), data = promotion.df)

## c)
m1<- aov(Sales ~ Promotion, data = promotion.df)
summary(m1)

m1a<- lm(Sales ~ Promotion, data = promotion.df)
summary(m1a)


## d)
m2<- aov(Sales ~ factor(Newsletter), data = promotion.df)
summary(m2)


## e)
m3<- aov(Sales ~ Promotion + factor(Newsletter), data = promotion.df)
summary(m3)

bptest(m3)

## similar results with linear regression        
lm3<- lm(Sales ~ factor(Promotion) + factor(Newsletter), data = promotion.df)
summary(lm3)
bptest(lm3)

## f)
m4<- aov(Sales ~ factor(Promotion) * factor(Newsletter), data = promotion.df)
summary(m4)

## Visualisation
interaction.plot(promotion.df$Promotion,promotion.df$Newsletter,promotion.df$Sales)


## g)
TukeyHSD(m4)


## h)
lm4<- lm(Sales ~ factor(Promotion) * factor(Newsletter), data = promotion.df)
summary(lm4)

## i)
bptest(lm4)
qqnorm(lm4$residuals)
qqline(lm4$residuals)


######################################################################################################################################################################
#######################################        Exercise D: Week1 Football Manager Analysis revisited                    ##############################################
######################################################################################################################################################################


######################################################################################################################################################################
#########################################################           Data preparation        ##########################################################################

# read.csv accent problem, using read_csv
football_managers_merged <-  read_csv("football_managers_all.csv")

# Encode naming columns
Encoding(football_managers_merged$manager_name)
football_managers_merged$manager_name <-  iconv(football_managers_merged$manager_name, to='UTF-8')

# Replace missing values NA from points last season with 0 (these are teams that got promoted to the premier league)
football_managers_merged$points_lastseason <-  ifelse(is.na(football_managers_merged$points_lastseason), 0,football_managers_merged$points_lastseason)


# Reminder: Below is how we formatted date columns in week 1
#football_managers_merged$date_from <-  as.Date(football_managers_merged$date_from, format = "%d-%b-%y")
#football_managers_merged$date_until <- as.Date(football_managers_merged$date_until, format = "%d-%b-%y")
#football_managers_merged$date <- as.Date(football_managers_merged$date, format = "%d/%m/%Y")

# We now use lubridate to 
football_managers_merged$date_from <- dmy(football_managers_merged$date_from)
football_managers_merged$date_until <- dmy(football_managers_merged$date_until)
football_managers_merged$date <- dmy(football_managers_merged$date)

# generate duration variable (it's measured in seconds)
football_managers_merged$duration <- as.duration(interval(football_managers_merged$date_from, football_managers_merged$date_until))

# Reminder:it's important to create a workable manager_name variable for the aggregate data set
# We need to check whether there were multiple managers in one season and create a unique variable for each team
# Adding count variable 
football_managers_merged <- football_managers_merged %>% add_count(manager_name, team, season)   

# add new name if mixed team
football_managers_merged$manager_name_clean <- ifelse(
  football_managers_merged$n < 38,
  paste0("Multiple manager ", football_managers_merged$team),
  football_managers_merged$manager_name
)


######################################################################################################################################################################

## Aggregate data to season level
football_managers_seasons <- group_by(football_managers_merged, season, team) %>% 
  reframe(
    points = sum(points),
    goals = sum(goals),
    duration= mean(duration),
    goals_opponent = sum(goals_opponent),
    points_lastseason = mean(points_lastseason),
    team = first(team),
    manager_name = first(manager_name_clean),
    caretaker = mean(caretaker)
  ) %>% 
  as.data.frame()                                        


######################################################################################################################################################################
### D Now we start running models

# a) 
ols.fit.a <- lm(points ~ manager_name + team + points_lastseason, data = football_managers_seasons)
summary(ols.fit.a)

## points_lastseason  now not significant anymore! (and note the sign)

# Adjust diagram (remove direct link of points_lastseason) and extract the top 10 performing managers
ols.fit.a.adjusted <- lm(points ~ manager_name + team, data = football_managers_seasons)
summary(ols.fit.a.adjusted)

ols.fit.a.adjusted.summary <- summary(ols.fit.a.adjusted)
coefficients <- as.data.frame(ols.fit.a.adjusted.summary$coefficients[,1], stringsAsFactors = FALSE)
colnames(coefficients) <- c("Estimate")
coefficients$Coefficient <- rownames(coefficients)

# Filter out the coefficients related to manager_name
manager_coefficients <- coefficients[grep("manager_name", coefficients$Coefficient), ]

# Sort by the estimates in descending order
manager_coefficients <- manager_coefficients[order(manager_coefficients$Estimate, decreasing = TRUE), ]

manager_coefficients[1:10,]
# manager_nameAlex Ferguson   69.07826   manager_nameAlex Ferguson




# b) Fit the OLS model without Team
ols.fit.b <- lm(points ~ manager_name + points_lastseason, data = football_managers_seasons)  
summary(ols.fit.b)

## points_lastseason- now significant and sign as expected


# d) Let's run aov
# two-way ANOVA
two.way.anova.fit.points <- aov(points ~ manager_name + team, data = football_managers_seasons)
summary(two.way.anova.fit.points)

# one-way ANOVA
anova.fit.points <- aov(points ~ manager_name, data = football_managers_seasons)
summary(anova.fit.points)


# e) 
# step 1 - aov (one-way ANOVA)
anova.fit.duration <- aov(duration ~ manager_name, data = football_managers_seasons)
summary(anova.fit.duration)

# step 2 - linear regression with mediator Team
ols.fit.e <- lm(points ~ team + manager_name + duration , data = football_managers_seasons)  
summary(ols.fit.e)  # duration is not significant -> no arrow to points


# step 3 - linear regression omitting mediator Team
# let's omit team and see whether there is an association with points (we also add points last season again)
ols.fit.e.omit.team <- lm(points ~ manager_name +  duration , data = football_managers_seasons)  
summary(ols.fit.e.omit.team)




# f) We need to standardise the two predictors, e.g. using scale()
ols.fit.z <- lm(points ~ manager_name + scale(points_lastseason) + scale(duration), data = football_managers_seasons)  
summary(ols.fit.z)


# g) final model - no mediators but controlling for team performance in previous season
ols.fit.final <- lm(points ~ manager_name + points_lastseason, data = football_managers_seasons)  
summary(ols.fit.final)

# Normality - histogram
hist(ols.fit.final$residuals)

# qqplot as an alternative to a histogram
qqnorm(ols.fit.final$residuals)  
qqline(ols.fit.final$residuals)

# Homoscedasticity check using Breusch-Pagan test
bptest(ols.fit.final) # good


# Extract the top 10 performing managers
summary_ols.final <- summary(ols.fit.final)

coefficients.final <- as.data.frame(summary_ols.final$coefficients, stringsAsFactors = FALSE)
colnames(coefficients.final) <- c("Estimate","Std. Error", "t value","p value")
coefficients.final$Coefficient <- rownames(coefficients.final)

# Filter out the coefficients related to manager_name
manager_coefficients.final <- coefficients.final[grep("manager_name", coefficients.final$Coefficient), ]

# Sort by the coefficient estimates in descending order
manager_coefficients.final <- manager_coefficients.final[order(manager_coefficients.final$Estimate, decreasing = TRUE), ]

manager_coefficients.final[1:10,]
# top: manager_namePep Guardiola   39.08417 


# h)
#Adding the coefficients
anova.final.model <- aov(points ~ manager_name + points_lastseason, data = football_managers_seasons)
summary(anova.final.model)

# Variance explained based on partial eta squared
eta_squared(anova.final.model)
# Manager explains 86% of the variance

# note - partial cohen's f (based on partial eta squared) is the default
# We usually are interested in the partial effect sizes (controlling for other variables)
cohens_f(anova.final.model)

