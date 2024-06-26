######################################################################################################################################################################
#######################  Melbourne Business School- Practice Questions - Causal Analytics - Class 4 2024 Nico Neumann   ##############################################
######################################################################################################################################################################
## clean memory
rm(list=ls()) 


## install packages if missing
## (1) Define the packages that will be needed
packages <- c( 'tidyverse','PSweight', 'lmtest',  'magrittr','MASS', 'boot', 'ggplot2')

## (2) Install them if not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}


######################################################################################################################################################################


# foldername <- "C:\\Users\\n.neumann" 
# foldername <- "C:\\Users\\nicon" 

# setwd(paste0(foldername,"\\OneDrive\\2024_Causal_Analytics_TAs\\2024_Causal_Class4"))

#################### load limath.dfaries (install first if necessary)
library(tidyverse)
library(ggplot2)
library(boot)
library(magrittr)
library(MASS)
library(PSweight)

# increase print out
options(max.print=1000000)
## avoid scientific notation
options(scipen=99)


######################################################################################################################################################################
##########################################################            Exercises A Education and Schooling ############################################################
######################################################################################################################################################################


## load data
math.df <- read.csv("catholic_schools.csv")

## 
head(math.df)


## B a) transforming outcome variable?
hist(math.df$math12)   # seems closer to normal distribution
hist(log(math.df$math12))

## B b) balance check for our treatment variable 'catholic'
table(math.df$catholic)

math.df  %>% group_by(catholic) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3))   # very imbalanced - only 10% catholic
 

## B c) let's check our observed covariates (exactly as we did with experimental data)
# Balance by gender
math.df  %>% group_by(catholic, female) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3))  # small differences


# Balance by race
math.df  %>% group_by(catholic, race) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3))  # stronger differences

# Balance by parents marital status in 8th grade
math.df  %>% group_by(catholic, parmar8) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3))  # stronger differences

# Balance by student rarely completes homework in grade 8
math.df  %>% group_by(catholic, nohw8) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3))  # stronger differences

# Balance by disrupt8
math.df  %>% group_by(catholic, disrupt8) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3))  # small differences

# Balance by figh8
math.df  %>% group_by(catholic, fight8) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3))  # small differences

# Balance by discrete variable risk to drop
math.df  %>% group_by(catholic, riskdrop8) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3))  # stronger differences


## For continuous variables, we can compare the mean for each catholic level
math.df %>% group_by(catholic) %>% summarize(math8 = mean(math8), read8 =mean(read8), faminc8 = mean(faminc8), 
                                             fathed8= mean(fathed8), mothed8=mean(mothed8))  

## -> some differences, in particular pre-treatment achievements

## We can interpret education and income as discrete too
# Balance by discrete variable education
math.df  %>% group_by(catholic, fathed8) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3))

math.df  %>% group_by(catholic, mothed8) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3))

math.df  %>% group_by(catholic, faminc8) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3))

# -> difficult to read - better to use histograms or other graphs and check differences/ overlap


## B d) check common support and balance (indirectly) for continuous variable - overlap of distributions
math.df %>%
  mutate(diff_group = ifelse(catholic == 1, "Catholic school", "Other school")) %>%
  ggplot(aes(x = math8,
            group = diff_group, colour = diff_group, fill = diff_group)) +
  geom_density(alpha = I(0.2)) +
  xlab("math score in class 8") +
  scale_fill_viridis_d("Status:", end = 0.7) +
  scale_colour_viridis_d("Status:", end = 0.7) +
  theme(legend.position = "bottom")

## income, which is discrete (different income math.dfackets)
math.df %>%
  mutate(diff_group = ifelse(catholic == 1, "Catholic school", "Other school")) %>%
  ggplot(aes(x = faminc8,
             fill = diff_group)) +
  geom_histogram(aes(y = ..density.. /sum(..density..)*100), alpha = 0.5, position = "dodge") +
  xlab("family income in class 8") +
  ylab("Percentage count") +
  scale_fill_viridis_d("Status:", end = 0.7) +
  theme(legend.position = "bottom")


math.df %>%
  mutate(diff_group = ifelse(catholic == 1, "Catholic school", "Other school")) %>%
  ggplot(aes(x = read8,
             group = diff_group, colour = diff_group, fill = diff_group)) +
  geom_density(alpha = I(0.2)) +
  xlab("read score in class 8") +
  scale_fill_viridis_d("Status:", end = 0.7) +
  scale_colour_viridis_d("Status:", end = 0.7) +
  theme(legend.position = "bottom")

math.df %>%
  mutate(diff_group = ifelse(catholic == 1, "Catholic school", "Other school")) %>%
  ggplot(aes(x = mothed8,
             fill = diff_group)) +
  geom_histogram(aes(y = ..density.. /sum(..density..)*100), alpha = 0.5, position = "dodge") +
  xlab("Mother's education") +
  ylab("Percentage count") +
  scale_fill_viridis_d("Status:", end = 0.7) +
  theme(legend.position = "bottom")


math.df %>%
  mutate(diff_group = ifelse(catholic == 1, "Catholic school", "Other school")) %>%
  ggplot(aes(x = fathed8,
             fill = diff_group)) +
  geom_histogram(aes(y = ..density.. /sum(..density..)*100), alpha = 0.5, position = "dodge") +
  xlab("Father's education") +
  ylab("Percentage count") +
  scale_fill_viridis_d("Status:", end = 0.7) +
  theme(legend.position = "bottom")

## B e)
## formula with all covariates
D.model <- glm(catholic ~ factor(female) + factor(race)  + factor(fight8)+ factor(disrupt8) + factor(fathed8) + factor(mothed8)+ faminc8  + math8 + read8 + factor(nohw8) + factor(parmar8) + factor(riskdrop8), data = math.df, family = binomial(link = 'logit'))
summary(D.model)
# Most variables matter and seem to affect treatment status



## C) stratification (subclassification) with not completing hw and disruptive behaviour dummies only
math.df  %<>%
  mutate(subclass = case_when(nohw8 == 0 & disrupt8 == 0 ~ 1,
                              nohw8 == 1 & disrupt8 == 0 ~ 2,
                              nohw8 == 0 & disrupt8 == 1 ~ 3,
                              nohw8 == 1 & disrupt8 == 1 ~ 4))


## double check
table(math.df$subclass)

## define N_treated = number of treated observations
N_treated <- math.df %>% filter(catholic == 1) %>% nrow()

## create a function using bootstrapping
strata.att <- function(data.analyse){
  
  ey11 <- data.analyse %>% 
    filter(subclass== 1 & catholic== 1) %$%
    mean(math12)
  
  ey10 <- data.analyse %>% 
    filter(subclass== 1 & catholic== 0) %$%
    mean(math12)
  
  ey21 <- data.analyse %>% 
    filter(subclass== 2 & catholic== 1) %$%
    mean(math12)
  
  ey20 <- data.analyse %>% 
    filter(subclass== 2 & catholic== 0) %$%
    mean(math12)
  
  ey31 <- data.analyse %>% 
    filter(subclass== 3 & catholic== 1) %$%
    mean(math12)
  
  ey30 <- data.analyse %>% 
    filter(subclass== 3 & catholic== 0) %$%
    mean(math12)
  
  ey41 <- data.analyse %>% 
    filter(subclass== 4 & catholic== 1) %$%
    mean(math12)
  
  ey40 <- data.analyse %>% 
    filter(subclass== 4 & catholic== 0) %$%
    mean(math12)
  
  diff1 = ey11 - ey10
  diff2 = ey21 - ey20
  diff3 = ey31 - ey30
  diff4 = ey41 - ey40
  
  
  wt1 <- data.analyse %>% 
    filter(subclass== 1 & catholic==1) %$%
    nrow(.)/N_treated 
  
  wt2 <- data.analyse %>% 
    filter(subclass== 2 & catholic==1) %$%
    nrow(.)/N_treated
  
  wt3 <- data.analyse %>% 
    filter(subclass== 3 & catholic==1) %$%
    nrow(.)/N_treated
  
  wt4 <- data.analyse %>% 
    filter(subclass== 4 & catholic==1) %$%
    nrow(.)/N_treated
  
  ## (weighted) catholic effect
  att = diff1*wt1 + diff2*wt2 + diff3*wt3 + diff4*wt4
  
  return(att)
}


strata.boot <- function(data =math.df, index = 1:nrow(math.df)) {
  data.bootstrap <- math.df %<>% slice(index)
  boot.round.result <- strata.att(data.bootstrap)
  return(boot.round.result)
}

## bootstrapping
boot.result <- boot(math.df, strata.boot, R = 100)
boot.result


## D)

## a) Matching on female, all race indicators and income levels possible?
### check if there is any overlap
check.overlap <- math.df %>% 
  group_by(female, race, faminc8) %>%
  summarise(count = n(), .groups="keep") %>%
  ungroup() %>%
  complete(female, race, faminc8) %>% as.data.frame()

## check
any(is.na(check.overlap))  # NAs -> missing counterfactuals -> Strata matching is not possible without losing observations


## b) exact matching on further variables will likely be challenging




## E) Propensity score nuisance function only using nohw8 and disrupt8
glm.ps <- glm(catholic ~ factor(nohw8) + factor(disrupt8), data = math.df, family = binomial(link = 'logit'))
# Get predicted values
math.df %<>% mutate(ps = predict(glm.ps, type = 'response'))

# see unique valuse of the PS by subclass
math.df[!duplicated(math.df[,c('ps','subclass')]),][,c('ps','subclass')]   


## E a)  ATT raw (not normalised)
# we create a new n_treated based on our trimmed data frame
n_treated <- math.df %>% filter(catholic == 1) %>% nrow()

# Manual coding (from lecture exercise in week 4)                    
ipwe.att <- math.df %>% 
  mutate(y1 = catholic * math12,
         y0 = (1-catholic)* math12 * ps/(1-ps),
         att = y1 - y0) %>%
              pull(att) %>% 
              sum() %>% 
              magrittr::divide_by(n_treated)

print(ipwe.att) # 3.329712



## E b) ATT normalised (same approach as for ATE in lecture, but ATT weights)
math.df %<>%  
  mutate(d1att = catholic,
         d0att = (1-catholic)*ps/(1-ps))

s1att <- sum(math.df$d1att)
s0att <- sum(math.df$d0att)

# our normalising adjustment factor (careful, for ATT use n_treated)
adj1 <- s1att/n_treated
adj0 <- s0att/n_treated


# manual ATT estimator with normalised weights
ipwe.att.n <-  math.df %>% 
          mutate(y1 = catholic*math12/adj1,
                 y0 = (1-catholic)*math12* (ps/(1-ps))/adj0,
                 att.norm = y1 - y0) %>% 
                  pull(att.norm)  %>% 
                   sum() %>% 
                    magrittr::divide_by(n_treated)

print(ipwe.att.n)  # 3.330033



## We can double check the normalised ATT using the PSweight function
ps.formula <- catholic ~ factor(nohw8) + factor(disrupt8)
att<-PSweight(ps.formula,yname = 'math12',data = math.df, weight = 'treated')
summary(att)  #  3.3300  # same
  




## F a) naive.estimate = simple means difference
naive.estimate <- lm(math12 ~ catholic, data= math.df)
summary(naive.estimate )  # 3.8949

## F b) regression adjustment with all covariates that could be confounders
ols <- lm(math12 ~ catholic + factor(female) + factor(race)   + factor(fight8)+ factor(disrupt8) + factor(fathed8) + factor(mothed8) + faminc8 + math8 + read8+ factor(nohw8)  + factor(parmar8) + factor(riskdrop8), data= math.df)
summary(ols) # 1.285379 
# Adjusted R-squared:   0.7193
AIC(ols) # 34463.87

# education as continuous
ols2 <- lm(math12 ~ catholic + factor(female) + factor(race)  + factor(fight8)+ factor(disrupt8) + fathed8 + mothed8 + faminc8 + math8 + read8 + factor(nohw8)  + factor(parmar8) + factor(riskdrop8), data= math.df)
summary(ols2) #  1.328835 
# Adjusted R-squared:  0.7182 
AIC(ols2) # 34475.24

ols3 <- lm(math12 ~ catholic + factor(female) + factor(race)   + factor(fight8)+ factor(disrupt8) + factor(fathed8) + factor(mothed8) + log(faminc8) + log(math8) + log(read8) + factor(nohw8)  + factor(parmar8)+ factor(riskdrop8), data= math.df)
summary(ols3) # 1.21212
# Adjusted R-squared:  0.7232 
AIC(ols3) #  34384.74

ols4 <- lm(math12 ~ catholic + factor(female) + factor(race)   + factor(fight8)+ factor(disrupt8) + factor(fathed8) + factor(mothed8) + faminc8 + log(math8) + log(read8) + factor(nohw8)  + factor(parmar8)+ factor(riskdrop8), data= math.df)
summary(ols4) # 1.19863 
# Adjusted R-squared:  0.7232 
AIC(ols4) #  34381.32



## G) 

## ensure we get the same results
set.seed(12345)

## formula with all covariates
ps.formula.base <- catholic ~ factor(female) + factor(race)  + factor(fight8)+ factor(disrupt8) + factor(fathed8) + factor(mothed8)+ faminc8  + math8 + read8 + factor(nohw8) + factor(parmar8) + factor(riskdrop8)

## logistic regression
glm.ps.all <- glm(ps.formula.base, data = math.df, family = binomial(link = 'logit'))


selected.glm.fit <- stepAIC(glm.ps.all, directions= "both")  
summary(selected.glm.fit)
AIC(selected.glm.fit) # 3580.745

## updating formula based on stepAIC (remove some predictors)
ps.formula2 <- catholic ~ factor(race)  + factor(fight8) + factor(mothed8) + math8 + read8+ factor(nohw8)  + factor(parmar8)

## let's update the propensity score and check the min/ max again
math.df %<>% mutate(ps = predict(selected.glm.fit, type = 'response'))

## let's trim the data by the most extreme 1%
math.df.trim <- math.df %>%
  filter(ps >= 0.01 & ps <= 0.99)



## G a) check propensity score overlap and balance
bal.any <- SumStat(ps.formula =ps.formula2, data = math.df, weight = c("IPW"))
plot(bal.any, type = "hist")
plot(bal.any, type = "balance") # all good

bal.att <- SumStat(ps.formula =ps.formula2, data = math.df, weight = c("treated"))
plot(bal.any, type = "hist")
plot(bal.att, type = "balance") # all good

# trimmed version
bal.any <- SumStat(ps.formula =ps.formula2, data = math.df.trim, weight = c("IPW"))
plot(bal.any, type = "hist")
plot(bal.any, type = "balance") # all good

bal.att <- SumStat(ps.formula =ps.formula2, data = math.df.trim, weight = c("treated"))
plot(bal.any, type = "hist")
plot(bal.att, type = "balance") # all good



## b) IPWE logistic regression using PSweight
## ATE (use weight 'IPW')
ate.itpwe <-PSweight(ps.formula2 ,yname = 'math12',  data = math.df,weight = 'IPW')
summary(ate.itpwe) # 2.09019

## trimmed version -> makes no difference, as we are using normalised weights
ate.itpwe.trim <-PSweight(ps.formula2 ,yname = 'math12',  data = math.df.trim ,weight = 'IPW')
summary(ate.itpwe) # 2.09019  # no difference

## ATT (use weight 'treated')
att.itpwe <-PSweight(ps.formula2 ,yname = 'math12',data = math.df,weight = 'treated')
summary(att.itpwe) # 1.30280 # no difference

att.itpwe.trim  <-PSweight(ps.formula2 ,yname = 'math12',data = math.df.trim ,weight = 'treated')
summary(att.itpwe) # 1.30280 (same)

# We just proceed with the full (non-trimmed) data set


## Gc) PS balance using GBM
bal.any.gbm <- SumStat(ps.formula =ps.formula2, method ='gbm', data = math.df, weight = c("IPW"))
plot(bal.any.gbm, type = "hist")
plot(bal.any.gbm, type = "balance")

bal.gbm.att <- SumStat(ps.formula =ps.formula2, method ='gbm', data = math.df, weight = c("treated"))
plot(bal.gbm.att, type = "hist")
plot(bal.gbm.att, type = "balance")


## G d) IPWE gbm using PSweight
ate.itpwe.gbm <-PSweight(ps.formula2 ,yname = 'math12', ps.method = "gbm", data = math.df,weight = 'IPW')
summary(ate.itpwe.gbm) # 1.87421, SE= 0.474 (number changes due to GBM values)
# standardised metric: 1.87421/ 0.474 = 3.9540

att.itpwe.gbm <-PSweight(ps.formula2 ,yname = 'math12', ps.method = "gbm", data = math.df,weight = 'treated')
summary(att.itpwe.gbm) #  1.47130, SE = 0.3725 (number changes due to GBM values)
# standardised metric: 1.47130/0.474 = 3.104


## IPWE gbm using PSweight using bootstrapped SE (100 replications)
ate.itpwe.gbm <-PSweight(ps.formula2 ,yname = 'math12', ps.method = "gbm", bootstrap = TRUE, R=100,  data = math.df,weight = 'IPW')
summary(ate.itpwe.gbm) #  1.89531,  SE =0.358 (number changes due to GBM values)
# standardised metric: 1.89531/ 0.358 = 5.2941

att.itpwe.gbm <-PSweight(ps.formula2 ,yname = 'math12', ps.method = "gbm",bootstrap = TRUE, R=100,  data = math.df,weight = 'treated')
summary(att.itpwe.gbm) #  1.2615, SE= 0.2423  (number changes due to GBM values)
# standardised metric: 1.2615/ 0.2423 = 5.2063



