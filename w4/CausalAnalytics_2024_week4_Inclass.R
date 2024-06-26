######################################################################################################################################################################
#######################   Melbourne Business School- In-class exercise - Causal Analytics - Class 4 2024 Nico Neumann   ##############################################
######################################################################################################################################################################
## clean memory
rm(list=ls()) 

## (1) Define the packages that will be needed
packages <- c( 'tidyverse','PSweight', 'lmtest',  'magrittr','MASS', 'boot')

## (2) Install them if not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

######################################################################################################################################################################

## Add your path to the data here
# foldername <- "C:\\Users\\n.neumann" 
# foldername <- "C:\\Users\\nicon" 

# setwd(paste0(foldername,"\\OneDrive\\2024_Causal_Analytics_TAs\\2024_Causal_Class4"))

#################### load libraries (install first if necessary)
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

set.seed(12345)

######################################################################################################################################################################
################################################                            Exercise 1 -  Titanic                       ##############################################
######################################################################################################################################################################

## load hotel data from class 1 assignment
titanic <- read.csv("titanic.csv")

## Let's create our treatment dummy variable
titanic  %<>% 
  mutate(d = case_when(class == 1 ~ 1, TRUE ~ 0))

################################

## 1 a) SMD using OLS
ols.smd <- lm(survived~ d, data= titanic)
summary(ols.smd)  # 0.35383


## 1 b) different from regression adjustment?
ols.smd <- lm(survived~ d + adult + male, data= titanic)
summary(ols.smd)  # 0.2320

## 1 c) treatment level balance
table(titanic$d)  # imbalanced as expected for first class


## 1 d) covariate balance 
# Balance by adult (child vs. adult)
titanic %>% group_by(d, adult) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3))   # some differences

# Balance by gender
titanic %>% group_by(d, male) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3))  # stronger differences


## 1 e) We regress our treatment on adult/ male using logistic regression
fit.check <- glm(d ~ adult + male, data= titanic, family = 'binomial')
summary(fit.check)
# Odds
exp(fit.check$coefficients[2:3])

## survival odds
fit.check2 <- glm(survived ~ adult + male, data= titanic, family = 'binomial')
summary(fit.check2)
# Odds
exp(fit.check2$coefficients[2:3])

## OK, let's create subgroups based on the observed covariates

## 1 f)
## We first need to create the strata/ subclasses
titanic %<>%
  mutate(subclass = case_when(male == 0 & adult == 1 ~ 1,
                       male == 0 & adult == 0 ~ 2,
                       male == 1 & adult == 1 ~ 3,
                       male == 1 & adult == 0 ~ 4,
                       TRUE ~ 0))

## ATE estimator - we write a loop and bootstrap the standard errors
strata.ate <- function(data.analyse){

titanic <- data.analyse

ey11 <- titanic %>% 
  filter(subclass== 1 & d == 1) %$%
  mean(survived)

ey10 <- titanic %>% 
  filter(subclass== 1 & d == 0) %$%
  mean(survived)

ey21 <- titanic %>% 
  filter(subclass== 2 & d == 1) %$%
  mean(survived)

ey20 <- titanic %>% 
  filter(subclass== 2 & d == 0) %$%
  mean(survived)

ey31 <- titanic %>% 
  filter(subclass== 3 & d == 1) %$%
  mean(survived)

ey30 <- titanic %>% 
  filter(subclass== 3 & d == 0) %$%
  mean(survived)

ey41 <- titanic %>% 
  filter(subclass== 4 & d == 1) %$%
  mean(survived)

ey40 <- titanic %>% 
  filter(subclass== 4 & d == 0) %$%
  mean(survived)

## treatment effects for each subclass
subclass.ate1 = ey11 - ey10
subclass.ate2 = ey21 - ey20
subclass.ate3 = ey31 - ey30
subclass.ate4 = ey41 - ey40

# The “exposition” pipe, %$%, which is a special pipe operator from the magrittr library,
# exposes the names within the left-hand side object to the right-hand side expression.
# allows us to use the column names from the data frame directly without specifying the data frame name again.

# ate weights
wt1 <- titanic %>% 
  filter(subclass== 1) %$%
  nrow(.)/nrow(titanic) 
# counting the number of rows in the subset of titanic where s equals 1 and d equals 0.
#The . is a placeholder for the data frame that was piped into this function.

wt2 <- titanic %>% 
  filter(subclass== 2) %$%
  nrow(.)/nrow(titanic)

wt3 <- titanic %>% 
  filter(subclass== 3) %$%
  nrow(.)/nrow(titanic)

wt4 <- titanic %>% 
  filter(subclass== 4) %$%
  nrow(.)/nrow(titanic)

## (weighted) averadult treatment effect
ate = subclass.ate1*wt1 + subclass.ate2*wt2 + subclass.ate3*wt3 + subclass.ate4*wt4
print(ate)  #  0.1959843

return(ate)
}

# check strata function to estimate ATE
strata.ate(titanic) # 0.1959843


## our bootstrap function loop (the program samples from the dataset)
strata.boot <- function(data = titanic, index = 1:nrow(titanic)) {
  data.bootstrap <- titanic %<>% slice(index)   # we take only a subset from the original data se
  boot.round.result <- strata.ate(data.bootstrap) # provide the subset to the strata.ate function we defined earlier
  return(boot.round.result)  # return the result from the function
}



### now run boot() with the earlier function boot function
# set seed for reproduction
set.seed(123)
# bootstrap simulation with 100 number of replications
boot.result <- boot(titanic, strata.boot, R = 100)
boot.result   # SE = 0.0283   (check column std. error)


## ATT estimator (no bootstrapping) -> same calculation
ey11 <- titanic %>% 
  filter(subclass== 1 & d == 1) %$%
  mean(survived)

ey10 <- titanic %>% 
  filter(subclass== 1 & d == 0) %$%
  mean(survived)

ey21 <- titanic %>% 
  filter(subclass== 2 & d == 1) %$%
  mean(survived)

ey20 <- titanic %>% 
  filter(subclass== 2 & d == 0) %$%
  mean(survived)

ey31 <- titanic %>% 
  filter(subclass== 3 & d == 1) %$%
  mean(survived)

ey30 <- titanic %>% 
  filter(subclass== 3 & d == 0) %$%
  mean(survived)

ey41 <- titanic %>% 
  filter(subclass== 4 & d == 1) %$%
  mean(survived)

ey40 <- titanic %>% 
  filter(subclass== 4 & d == 0) %$%
  mean(survived)

## treatment effects for each subclass (same as for ATE)
subclass.ate1 = ey11 - ey10
subclass.ate2 = ey21 - ey20
subclass.ate3 = ey31 - ey30
subclass.ate4 = ey41 - ey40

## Careful - now we compute the weights for ATT, which are different
# Total number of observations of treated units
obs.att = nrow(titanic %>% filter(d == 1))

wt1.att <- titanic %>% 
  filter(subclass== 1 & d == 1) %$%
  nrow(.)/obs.att 

wt2.att <- titanic %>% 
  filter(subclass== 2 & d == 1) %$%
  nrow(.)/obs.att 

wt3.att <- titanic %>% 
  filter(subclass== 3 & d == 1) %$%
  nrow(.)/obs.att 

wt4.att <- titanic %>% 
  filter(subclass== 4 & d == 1) %$%
  nrow(.)/obs.att 

# sum to one
print(wt1.att + wt2.att+ wt3.att + wt4.att)

## (weighted) averadult treatment effect
att = subclass.ate1*wt1.att + subclass.ate2*wt2.att + subclass.ate3*wt3.att + subclass.ate4*wt4.att
print(att)  # 0.2375421

######################################################################################################################################################################
 ################################################                     Exercises 2-3    Job Training                     ##############################################
######################################################################################################################################################################


## load data from RCT
training.rct <- read.csv("jobtraining_rct.csv" )

## load hotel data from class 1 assignment
nsw_dw_cpscontrol <- read.csv("jobtraining_control.csv")

################################

## 2 a) check observational data
hist(nsw_dw_cpscontrol$re78)  # seems better  
hist(log(nsw_dw_cpscontrol$re78))



## 2 b) Our ground truth
ATE.rct <- lm(re78 ~  treat, data = training.rct)
summary(ATE.rct)  # 1794.3 **

ATE.rct <- lm(re78 ~  treat + age  + factor(educ) + marr + nodegree + black + hisp + re74 + re75, data = training.rct)
summary(ATE.rct)   # 1539.30864 *




## 2 c) Regression adjustment using observational data
## SMD using OLS
ols.smd <- lm(re78~ treat, data= nsw_dw_cpscontrol)
summary(ols.smd)  # -8497.52 ***

## regression adjustment observational data
ols.adj <- lm(re78~  treat + age  + educ + marr + nodegree + black + hisp + re74 + re75 + u74 +u75 , data= nsw_dw_cpscontrol)
summary(ols.adj)  # 1066.3  . 





## 2d) treatment level balance (for our observational data)
table(nsw_dw_cpscontrol$treat)  # imbalanced / small proportion only with treatment


## covariate balance 
# Balance by married
nsw_dw_cpscontrol %>% group_by(treat, marr) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3))   # very different

# Balance by nodegree
nsw_dw_cpscontrol %>% group_by(treat,black) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3))  # strong differences

# Balance by nodegree
nsw_dw_cpscontrol %>% group_by(treat, nodegree) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3))  # strong differences

# check common support and balance (indirectly) for continuous variable - overlap of distributions
# educ
nsw_dw_cpscontrol %>%
  mutate(diff_group = ifelse(treat == 1, "Treatment", "Control")) %>%
  ggplot(aes(x = educ,
             group = diff_group, colour = diff_group, fill = diff_group)) +
  geom_density(alpha = I(0.2)) +
  xlab("Education") +
  scale_fill_viridis_d("Status:", end = 0.7) +
  scale_colour_viridis_d("Status:", end = 0.7) +
  theme(legend.position = "bottom")

# age
nsw_dw_cpscontrol %>%
  mutate(diff_group = ifelse(treat == 1, "Treatment", "Control")) %>%
  ggplot(aes(x = age,
             group = diff_group, colour = diff_group, fill = diff_group)) +
  geom_density(alpha = I(0.2)) +
  xlab("Age") +
  scale_fill_viridis_d("Status:", end = 0.7) +
  scale_colour_viridis_d("Status:", end = 0.7) +
  theme(legend.position = "bottom")

# previous earnings before training
nsw_dw_cpscontrol %>%
  mutate(diff_group = ifelse(treat == 1, "Treatment", "Control")) %>%
  ggplot(aes(x = re75,
             group = diff_group, colour = diff_group, fill = diff_group)) +
  geom_density(alpha = I(0.2)) +
  xlab("Earnings 75") +
  scale_fill_viridis_d("Status:", end = 0.7) +
  scale_colour_viridis_d("Status:", end = 0.7) +
  theme(legend.position = "bottom")



## 2 e) are covariates associated with treatment? 
# educ factor
logit_nsw0 <- glm(treat ~ age  + factor(educ) + marr + nodegree + black + hisp + re74 + re75 + u74 +u75, family = binomial(link = "logit"), 
                 data = nsw_dw_cpscontrol)
summary(logit_nsw0)
AIC(logit_nsw0) # 980.67

# educ numeric
logit_nsw <- glm(treat ~ age  + educ + marr + nodegree + black + hisp + re74 + re75 + u74 +u75, family = binomial(link = "logit"), 
                data = nsw_dw_cpscontrol)
summary(logit_nsw)
AIC(logit_nsw) # 972.45

## 2 f) check if there is any overlap
check.overlap <- nsw_dw_cpscontrol %>% 
  group_by(age, educ , marr) %>%
  summarise(count = n(), .groups="keep") %>%
  ungroup() %>%
  complete(age, educ , marr) %>% as.data.frame()

## check
any(is.na(check.overlap))  # NAs -> missing counterfactuals

## will not be possible either then



###########################################################################
############################  3  estimating PS ############################

## 3a)  estimating PS using nuisance function 
logit_nsw_full <- glm(treat ~ age + educ + marr + nodegree + black + hisp + re74 + re75 + u74 + u75, family = binomial(link = "logit"), 
                 data = nsw_dw_cpscontrol)

## stepAIC function from MASS library to find best predictive model
selected.glm.fit <- stepAIC(logit_nsw_full, directions= "both")  
summary(selected.glm.fit)
AIC(selected.glm.fit) # 969.5645


## updating PS formula based on stepAIC (remove some predictors)
logit_nsw <- glm(treat ~ age + marr + nodegree + black + hisp + re74 + re75 + u74, family = binomial(link = "logit"), 
                 data = nsw_dw_cpscontrol)

## predicted probabilities = propensity to receive treatment = propensity scores
head(logit_nsw$fitted.values)   
head(predict(logit_nsw, type = "response")) # same

# add propensity scores to data.frame
nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% mutate(pscore = logit_nsw$fitted.values)


# Show the summary of propensity scores for both treated and control groups side by side
nsw_dw_cpscontrol %>%
  group_by(treat) %>%
  summarise(summary = list(summary(pscore))) %>%
  pull(summary) %>%
  set_names(nsw_dw_cpscontrol %>%
              group_by(treat) %>%
              summarise(summary = list(summary(pscore))) %>%
              pull(treat)) %>%
  map_dfr(~as.data.frame(as.list(.)), .id = "treat") %>%
  rename_all(~c("Group", "Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."))



## check overlap by plotting distribution for both treatment and control (and some approximate idea of how similar covariate distributions are) 
nsw_dw_cpscontrol %>%
  mutate(diff_group = ifelse(treat == 1, "Treatment", "Control")) %>%
  ggplot(aes(x = pscore,
             group = diff_group, colour = diff_group, fill = diff_group)) +
  geom_density(alpha = I(0.2)) +
  xlab("Propensity score") +
  scale_fill_viridis_d("Status:", end = 0.9) +
  scale_colour_viridis_d("Status:", end = 0.9) +
  theme(legend.position = "bottom")
# seems very different distributions


## 3 b) let's trim the data
## let's trim the data by the most extreme 1%
nsw_dw_cpscontrol.trim1P <- nsw_dw_cpscontrol %>%
  filter(pscore >= 0.01 & pscore <= 0.99)

## check overlap again
nsw_dw_cpscontrol.trim1P %>%
  mutate(diff_group = ifelse(treat == 1, "Treatment", "Control")) %>%
  ggplot(aes(x = pscore,
             group = diff_group, colour = diff_group, fill = diff_group)) +
  geom_density(alpha = I(0.3)) +
  xlab("Propensity score") +
  scale_fill_viridis_d("Status:", end = 0.99) +
  scale_colour_viridis_d("Status:", end = 0.99) +
  theme(legend.position = "bottom")
# Is this acceptable? 
# We would need a love plot to really tell how close all covariates are and whether we achieved acceptable balance. We will do this later in our analysis.
# For now we proceed we a quick calculation, keeping in mind that this is an open point (although you may notice some likely issue already in this density plot).

  
## 3 c) Manual IPWE ATE non-normalised weights
nsw_dw_cpscontrol.trim1P %<>% 
  mutate(y1 = treat * re78/pscore,
         y0 = (1-treat) * re78/(1-pscore),
         ate = y1 - y0)

nsw_dw_cpscontrol.trim1P %>% 
  pull(ate) %>% 
  mean()  #  -348.2047  This is suspicious -> something not right with our analysis
# re-consider densities - there are much more extreme values with low PS in the control group 



## 3 d) Let's further trim the data by the most extreme 10%
nsw_dw_cpscontrol.trim <- nsw_dw_cpscontrol %>%
  filter(pscore >= 0.1 & pscore <= 0.9)


## again balance check using ggplot only
nsw_dw_cpscontrol.trim %>%
  mutate(diff_group = ifelse(treat == 1, "Treatment", "Control")) %>%
  ggplot(aes(x = pscore,
             group = diff_group, colour = diff_group, fill = diff_group)) +
  geom_density(alpha = I(0.3)) +
  xlab("Propensity score") +
  scale_fill_viridis_d("Status:", end = 0.99) +
  scale_colour_viridis_d("Status:", end = 0.99) +
  theme(legend.position = "bottom")


##  Manual IPWE ATE non-normalised
nsw_dw_cpscontrol.trim %<>% 
  mutate(y1 = treat * re78/pscore,
         y0 = (1-treat) * re78/(1-pscore),
         ate = y1 - y0)

nsw_dw_cpscontrol.trim %>% 
  pull(ate) %>% 
  mean()  #  1598.551      This seems more realistic and much closer to our RCT ATE of 1539.3


## 3 e) Let's normalise weights to estimate the IPWE
# total number of observations in our data set
N <- nrow(nsw_dw_cpscontrol.trim)

## we create inverse pscore, one column for treat (d1)
## and one column for control (d0)
nsw_dw_cpscontrol.trim  %<>% 
  mutate(d1 = treat/pscore,
         d0 = (1-treat)/(1-pscore))

# Then aggregate all inverse pscores
s1 <- sum(nsw_dw_cpscontrol.trim$d1)
s0 <- sum(nsw_dw_cpscontrol.trim$d0)

# our normalising adjustment factor
adj1 <- s1/N
adj0 <- s0/N

# now we calculate the (inverse) weighted effects again
# but divide/ correct every observation by the adjustment factor
nsw_dw_cpscontrol.trim %<>% 
  mutate(y1 = (treat*re78/pscore)/(adj1),
         y0 = ((1-treat)*re78/(1-pscore))/(adj0),
         ate.norm = y1 - y0) 

nsw_dw_cpscontrol.trim %>% 
  pull(ate.norm) %>% 
  mean()  # 1498.21       

## This was done manually using the PS estimated BEFORE trimming


## Note: If we calculate PS again using the trimmed data, we get different results.
# Previously we first calculated the PS, then trimmed the data
# Let's see the resultsif we again calculate new PS for the trimmed data set
logit_nsw_trim <- glm(treat ~ age + marr + nodegree + black + hisp + re74 + re75 + u74, family = binomial(link = "logit"), 
                      data = nsw_dw_cpscontrol.trim)

# add to data.frame
nsw_dw_cpscontrol.trim %<>% 
  mutate(pscore_trim = logit_nsw_trim$fitted.values)

nsw_dw_cpscontrol.trim  %<>% 
  mutate(d1_trim = treat/pscore_trim,
         d0_trim = (1-treat)/(1-pscore_trim))


# Then aggregate all inverse pscores
s1_trim <- sum(nsw_dw_cpscontrol.trim$d1_trim)
s0_trim <- sum(nsw_dw_cpscontrol.trim$d0_trim)

# our normalising adjustment factor
adj1_trim <- s1_trim/N
adj0_trim <- s0_trim/N


# now we calculate the (inverse) weighted effects again
# but divide/ correct every observation by the adjustment factor
nsw_dw_cpscontrol.trim %<>% 
  mutate(y1_trim = (treat*re78/pscore_trim)/(adj1_trim),
         y0_trim = ((1-treat)*re78/(1-pscore_trim))/(adj0_trim),
         ate.norm_trim = y1_trim - y0_trim) 

nsw_dw_cpscontrol.trim %>% 
  pull(ate.norm_trim) %>% 
  mean()  # 1637.132   # This is the result for normalised weights if we re=estimate the PS score after trimming the data


# The results for the different PS scores, before or after, are quite different.
# Which one should we use? Normally we cannot rely on an RCT as ground truth (benchmark) but must follow our diagnostics
# We will further compare the two PS scores subsequently and loot at love plots (provided through PSWeight)



## 3 f) we can use the PSweight library to estimate IPWE for ATT and ATE (reminder: weights are always normalised for this library)

## For the PSweight library, we create a ps.formula that summarises what we normally provide to a regression analysis (outcome variable ~ predictors)
## We only inlcude predictors according to our stepAIC optimisation
ps.formula <- treat ~ age + marr + nodegree + black + hisp + re74 + re75 + u74

# we further define our predictors as vector (for next steps)
var.formula <- c('age' , 'marr' ,'nodegree' , 'black','hisp', 're74' , 're75' , 'u74')


## We must provide the PS estimate manually and then zname (treatment variable) and xname = our predictors for the nuisance function and the ps.estimate command
bal.any.pscore <- SumStat(zname='treat', xname =var.formula, ps.estimate = nsw_dw_cpscontrol.trim$pscore, data = nsw_dw_cpscontrol.trim,  weight = c("IPW"))
# Histogram funtion
plot(bal.any.pscore, type = "hist")  # suggests overlap but hard to say how well we matched
# Love plot function
plot(bal.any.pscore, type = "balance") # This is problematic - some red circles are outside the 0.1 standardised mean difference, implying covariates are not sufficiently balanced


## let's estimate the PS again for the trimmed data - we can just use SumStat from the PSWeight command and now provide the ps.formula
# it will then use the method provided: glm = logistic regression (which is the default, so we could just drop it)
# For ATE, we use weight = 'IPW'.
bal.any <- SumStat(ps.formula =ps.formula, data = nsw_dw_cpscontrol.trim, method = 'glm', weight = c("IPW"))
plot(bal.any, type = "hist") # hard to tell
plot(bal.any, type = "balance") # This looks good now


## let's also check the ATT weights (we omit method command as we use the default 'logistic regression=glm').
# For ATT, we use weight = 'treated' 
bal.att <- SumStat(ps.formula =ps.formula, data = nsw_dw_cpscontrol.trim, weight = c("treated"))
plot(bal.att, type = "hist")
plot(bal.att, type = "balance") # This looks good too


## 3 g) IPWE logistic regression using PSweight
# We could also indicate ps.method = 'glm' for logistic regression, but since it is the default, the above works too
# For ATE, we use weight = 'IPW'
ate.ipwe <- PSweight(ps.formula ,yname = 're78', data = nsw_dw_cpscontrol.trim, weight = 'IPW')
summary(ate.ipwe) # 1637.13  (same as above when using PS that were calculated after trimming)

# Note: analytical SE provides a warning (it could not apply the original formula)


# we can also provide the PS manually (e.g. to have those PS that we created before trimming). This functional is also helpful if you wanted to use a predictive method that is not included in PSWeight
ate.ipwe <- PSweight(ps.formula = ps.formula, ps.estimate = nsw_dw_cpscontrol.trim$pscore,  yname = 're78', data = nsw_dw_cpscontrol.trim, weight = 'IPW')
summary(ate.ipwe) # 1498.210 (same as manual calculation earlier for old PS weights - reminder that we found that these PS scores don't show a good covariate balance, see love plot earlier)
# So we don't want to rely on this model (even though it is closer to the RCT results, we normally would not know this. We must follow the matching approach.)



## We return to using the PS scores that are calculated for the trimmed data set
# Because of the SE warning , we now use bootstraping for standard error estimation instead (we can provide R, the number of replications. Here 100. Otherwise the default for R is 50)
ate.ipwe <-PSweight(ps.formula ,yname = 're78', bootstrap = TRUE, R=100,  data = nsw_dw_cpscontrol.trim, weight = 'IPW')
summary(ate.ipwe) # 1637.13 


## Reminder: For ATT, we use weight = 'treated'
att.ipwe <-PSweight(ps.formula ,yname = 're78', data = nsw_dw_cpscontrol.trim, weight = 'treated')
summary(att.ipwe) # 1943.35  
# We get a warning about the analytical SE estimation again.

## ATT IVPE With bootstrapped SE (100 replications)
att.ipwe <-PSweight(ps.formula ,yname = 're78',bootstrap = TRUE, R=100,  data = nsw_dw_cpscontrol.trim, weight = 'treated')
summary(att.ipwe) # 1943.35  

