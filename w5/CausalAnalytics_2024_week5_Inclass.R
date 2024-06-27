######################################################################################################################################################################
#######################   Melbourne Business School- In-class exercise - Causal Analytics - Class 5 2024 Nico Neumann   ##############################################
######################################################################################################################################################################
## clean memory
rm(list=ls()) 

## install packages if missing
## (1) Define the packages that will be needed
packages <- c( 'tidyverse','AER', 'lmtest', 'magrittr')

## (2) Install them if not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

######################################################################################################################################################################
# 
# ## Add your path to the data here
# foldername <- "C:\\Users\\n.neumann" 
# foldername <- "C:\\Users\\nicon" 
# setwd(paste0(foldername,"\\OneDrive\\2024_Causal_Analytics_TAs\\2024_Causal_Class5"))

#################### load libraries (install first if necessary)
library(tidyverse)
library(lmtest)
library(AER)
library(magrittr)

# increase print out
options(max.print=1000000)
## avoid scientific notation
options(scipen=99)

######################################################################################################################################################################
################################################                            Exercise 1                                  ##############################################
######################################################################################################################################################################

## load hotel data from class 1 assignment
hotel.df <- data.frame(read.csv('~/Desktop/Causal Analytics/Module3_Causal_Analytics/w5/hotels_vienna.csv'))

# check variables /inspect data set
head(hotel.df)

# check frequencies
table(hotel.df$area)
table(hotel.df$type)
table(hotel.df$scarce_room)


# a) let's examine our  variables
hist(hotel.df$rating)  # seems more normal (reminder: only the residuals should ideally be normally distributed if we want to rely on inference statistics)
hist(log(hotel.df$rating))

hist(hotel.df$stars)  # seems OK
hist(log(hotel.df$stars))  # seems OK

hist(hotel.df$distance)  # very skewed
hist(log(hotel.df$distance+1))  # seems more normal

hist(hotel.df$price)  # very skewed
hist(log(hotel.df$price))  # seems more normal



## b)  naive estimate - raw
naive.fit = lm(rating~  area + type  +  scarce_room + stars + log(distance+1) + log(price), data=hotel.df)
summary(naive.fit)  # Adjusted R-squared:  0.1943  

## naive estimate when all are log transformed
naive.fit2 = lm(rating~  area + type  + scarce_room + log(stars) + log(distance+1) + log(price), data=hotel.df)
summary(naive.fit2)  # Adjusted R-squared:   0.1959  (we keep using log stars)



## c) Testing arrows into rating (after constructing causal diagram)
diagram.test.fit = lm(rating~  area + type   + log(stars) + log(distance+1) + log(price), data=hotel.df)
summary(diagram.test.fit)  # Adjusted R-squared:   0.189 


diagram.test.fit2 = lm(rating~  type   + log(stars) + area + log(price), data=hotel.df)
summary(diagram.test.fit2)  # Adjusted R-squared:   0.1904


diagram.test.fit3 = lm(rating~  type   + log(stars) + log(distance+1) + log(price), data=hotel.df)
summary(diagram.test.fit3)  # Adjusted R-squared:   0.192 

# we should drop Type and area, distance can be kept


### Testing arrows into log(price)
diagram.test.fit4 = lm(log(price)~   type   + log(stars) + area + log(distance+1), data=hotel.df)
summary(diagram.test.fit4)  # Adjusted R-squared: 0.4006 


diagram.test.fit5 = lm(log(price)~   type   + log(stars) + area + log(distance+1) + scarce_room, data=hotel.df)
summary(diagram.test.fit5)  # Adjusted R-squared: 0.399 

# we confirm the links of distance and stars to price (these are confounders)
# Area and apartment also seems to matter for prices



## note type and area are not significant for our rating regression -> we drop these
naive.fit.final = lm(rating~  log(stars) + log(distance+1) + log(price), data=hotel.df)
summary(naive.fit.final)  # Adjusted R-squared:  0.1809 

# price is significant and positive, which is suspicious and seems not right.




## d) ########## testing instrument strength ##########
# Always run full and null model to obtain partial F-stat for all instruments

# test just fixed effects for area as instrument
red.fit.area <- lm( log(price) ~ log(stars) + log(distance+1), data=hotel.df)
full.fit.area <- lm( log(price) ~ log(stars) + log(distance+1) + area, data=hotel.df)
# use ANOVA command to compare the difference of two fitted models
anova(red.fit.area, full.fit.area)  # F-value = 2.9847  -> Too weak


# Let's test type as instrument 
red.fit <- lm( log(price) ~ log(stars) + log(distance+1), data=hotel.df)
full.fit <- lm( log(price) ~ log(stars) + log(distance+1) + type, data=hotel.df)
# use ANOVA command to compare the difference of two fitted models
anova(red.fit, full.fit)  # F-value = 9.184  -> Close, but too weak

## Given only Apartment was significant in our previous regression, let's create a dummy for Apartment only (versus other types) = sub type
hotel.df %<>% mutate(Apartment = if_else(type == 'Apartment', 1, 0))

# rerun with Apartment
red.fit <- lm( log(price) ~ log(stars) + log(distance+1), data=hotel.df)
full.fit <- lm( log(price) ~ log(stars) + log(distance+1) + Apartment, data=hotel.df)
# use ANOVA command to compare the difference of two fitted models
anova(red.fit, full.fit)  # F-value = 43.067  -> strong


# Let's test scarce_room as instrument 
red.fit <- lm( log(price) ~ log(stars) + log(distance+1), data=hotel.df)
full.fit <- lm( log(price) ~ log(stars) + log(distance+1) + scarce_room , data=hotel.df)
# use ANOVA command to compare the difference of two fitted models
anova(red.fit, full.fit)  # F-value = 5.547  -> Too weak


### e) let's add the rental price per area

############################################################################################################
# Source: # https://www.virtualvienna.net/moving-to-vienna/accommodation-in-vienna/real-estate-rental-prices/
hotel.df <- hotel.df %>% mutate(hotel.df,
                                rent = case_when(
                                  area== "Alsergrund" ~  '14.59',
                                  area== "Donaustadt" ~  '15.54',
                                  area== "Innere Stadt" ~  '19.44',
                                  area== "Leopoldstadt" ~  '14.57',
                                  area== "Landstrasse" ~  "14.51",
                                  area== "Wieden" ~  "14.74",
                                  area== "Margareten" ~  "14.84",
                                  area== "Mariahilf" ~  "14.27",
                                  area== "Neubau" ~  "14.68",
                                  area== "Josefstadt" ~  "14.60",
                                  area== "Favoriten" ~  "13.23",
                                  area== "Simmering" ~  "11.95",
                                  area== "Hernals" ~  "12.51",
                                  area== "Graben" ~ "19.84",    # Innenstadt +extra
                                  area== "Kaerntner Strasse" ~ "19.84", # Innenstadt +extra
                                  area== "Schonbrunn" ~  "14.14",     # castle - Hietzing
                                  area== "Rudolfsheim-Funfhaus" ~  "12.66",
                                  area== "Wahring" ~ "15.12" ,
                                  area== "Schwechat" ~ "11.5",   #  Bruck an der Leitha
                                  area== "Voesendorf " ~  "13.65",    # Meidling ?
                                  area== "Ottakring" ~  "12.63",
                                  area== "Vienna" ~  "15",
                                  TRUE ~ "16")
)

hotel.df$rent <- as.numeric(hotel.df$rent)

## check for missing data
any(is.na(hotel.df$rent))

############################################################################################################


#  Using rental prices per area as instrument only
# run full and null model to obtain partial F-stat for all instruments
red.fit.rent <- lm( log(price) ~ log(stars) + log(distance+1), data=hotel.df)
full.fit.rent <- lm( log(price) ~ log(stars) + log(distance+1) + rent, data=hotel.df)
# use ANOVA command to compare the difference of two fitted models
anova(red.fit.rent, full.fit.rent)  # F-value = 37.701  -> good

# let's test the log(rent)
red.fit.rent2 <- lm( log(price) ~ log(stars) + log(distance+1), data=hotel.df)
full.fit.rent2 <- lm( log(price) ~ log(stars) + log(distance+1) + log(rent), data=hotel.df)
# use ANOVA command to compare the difference of two fitted models
anova(red.fit.rent2, full.fit.rent2)  # F-value = 34.731  -> good, but slightly weaker


# let's test both rent and Apartment
red.fit.rent3 <- lm( log(price) ~ log(stars) + log(distance+1), data=hotel.df)
full.fit.rent3 <- lm( log(price) ~ log(stars) + log(distance+1) + rent + Apartment, data=hotel.df)
# use ANOVA command to compare the difference of two fitted models
anova(red.fit.rent3, full.fit.rent3)  # F-value = 44.848  -> good, strongest one so far



## f) IV regressions
iv.fit <- ivreg(rating ~   log(stars)  + log(distance+1) + log(price+1) | log(stars)  + log(distance+1) + Apartment  + rent, data=hotel.df)  ## One way to write this - exogenous variable | endogenous variable | instrument 
summary(iv.fit)  
# log(price + 1) = -0.20648   but p= 0.1854  (NS)


# check all conditions
# histogram
hist(iv.fit$residuals)

# qqplot
qqnorm(iv.fit$residuals)
qqline(iv.fit$residuals)  # non-normal -> careful with SE

# Breusch-Pagan test
bptest(iv.fit)  # Homoscedastic 



######################################################################################################################################################################
################################################                            Exercise 2                                  ##############################################
######################################################################################################################################################################

## load data
vietnam.df <- read.csv("Vietnam_earnings.csv")

## check data
head(vietnam.df)


## check balance of factors
table(vietnam.df$drafted)

## create dummy for white people (vs. non white)
vietnam.df$white <- ifelse(vietnam.df$race==1,1,0)

## Balance by race
vietnam.df %>% group_by(drafted, race) %>%
  summarise(cnt = n()) %>%
  mutate(prop = round(cnt / sum(cnt), 3))


## Age mean comparison
vietnam.df %>% group_by(drafted) %>% summarize(mean(age))


## Let's compare the outcome variable with and without log-transformation
hist(vietnam.df$earnings)
hist(log(vietnam.df$earnings))

## a) Naive estimate
naive.fit.vietnam <- lm(log(earnings)~served +factor(year)+factor(birthyear)+ white, data=vietnam.df)
summary(naive.fit.vietnam)  # -0.006826  ns

## b) IV regression - 2SLS 
# instrument strength
red.fit.vietnam <- lm(served ~factor(year)+factor(birthyear)+ white, data=vietnam.df)
full.fit.vietnam <- lm(served ~factor(year)+factor(birthyear)+ white + drafted, data=vietnam.df)
# use ANOVA command to compare the difference of two fitted models
anova(red.fit.vietnam, full.fit.vietnam)  # F-value = 86.316  -> strong


# 2SLS using ivreg
iv.fit.vietnam <- ivreg(log(earnings) ~ served +factor(year)+factor(birthyear)+ white | factor(year)+factor(birthyear)+ white + drafted, data = vietnam.df)
summary(iv.fit.vietnam)  # -0.286562 ***

# check all conditions
hist(iv.fit.vietnam$residuals)

qqnorm(iv.fit.vietnam$residuals)
qqline(iv.fit.vietnam$residuals)

# Breusch-Pagan test
bptest(iv.fit.vietnam)  # Heteroscedasticity -> careful with SE



# let's repeat our analysis with earnings as outcome (not transformed)
iv.fit.vietnam2 <- ivreg(earnings ~ served +factor(year)+factor(birthyear)+ white | factor(year)+factor(birthyear)+ white + drafted, data = vietnam.df)
summary(iv.fit.vietnam2)  # -3794.72 ***

# check all conditions
hist(iv.fit.vietnam2$residuals)

qqnorm(iv.fit.vietnam2$residuals)
qqline(iv.fit.vietnam2$residuals) 

# Breusch-Pagan test
bptest(iv.fit.vietnam2)  # Heteroscedasticity -> careful with SE




## d) ITT
ITT.fit.vietnam <- lm(log(earnings) ~ drafted +factor(year)+factor(birthyear) + white, data = vietnam.df)
summary(ITT.fit.vietnam)  # -0.031286 ***

# check all conditions
hist(ITT.fit.vietnam$residuals)

qqnorm(ITT.fit.vietnam$residuals)
qqline(ITT.fit.vietnam$residuals) # careful with SE

# Breusch-Pagan test
bptest(ITT.fit.vietnam) # Heteroscedasticity -> careful with SE



# let's repeat our analysis with earnings as outcome (not transformed)
ITT.fit.vietnam2 <- lm(earnings ~ drafted +factor(year)+factor(birthyear) + white, data = vietnam.df)
summary(ITT.fit.vietnam2)  # -414.29 ***

# check all conditions
hist(ITT.fit.vietnam2$residuals)

qqnorm(ITT.fit.vietnam2$residuals)
qqline(ITT.fit.vietnam2$residuals) # looks better

# Breusch-Pagan test
bptest(ITT.fit.vietnam2) # Heteroscedasticity -> careful with SE