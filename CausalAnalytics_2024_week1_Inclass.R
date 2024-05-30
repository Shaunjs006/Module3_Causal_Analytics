######################################################################################################################################################################
#######################   Melbourne Business School- In-class exercise - Causal Analytics - Class 1 2024 Nico Neumann   ##############################################
######################################################################################################################################################################
## Clean memory
rm(list=ls()) 

## (1) Define the packages that will be needed
packages <- c( 'tidyverse','MASS', 'lmtest',  'ggplot2')

## (2) Install them if not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

######################################################################################################################################################################

## Add your path to the data here
# foldername <- "C:\\Users\\n.neumann" 

## Add your path to the data here
# foldername <- "C:\\Users\\nicon" 

# setwd(paste0(foldername,"\\OneDrive\\2024_Causal_Analytics_TAs\\2024_Causal_Class 1"))


#################### load libraries (install first if necessary)
library(tidyverse)
library(MASS)
library(lmtest)
library(ggplot2)

# Increase print out
options(max.print=1000000)

# Avoid scientific notation
options(scipen=99)

######################################################################################################################################################################
#########################################################           Data preparation        ##########################################################################
######################################################################################################################################################################

# read.csv accent problem, using read_csv
football_managers_merged <-  read_csv("football_managers_week1.csv")

# Encode naming columns
Encoding(football_managers_merged$manager_name)
football_managers_merged$manager_name <-  iconv(football_managers_merged$manager_name, to='UTF-8')

# We can create new variables, for example, a dummy indicating whether team is new in the premier league
football_managers_merged$promoted <-  ifelse(is.na(football_managers_merged$points_lastseason),1, 0)

# check frequency of new variable
table(football_managers_merged$promoted)

# Replace missing values NA from points last season with 0 (these are teams that got promoted to the premier league)
football_managers_merged$points_lastseason <-  ifelse(is.na(football_managers_merged$points_lastseason), 0,football_managers_merged$points_lastseason)


# We could also properly format the dates (check how your date format looks like - https://www.r-bloggers.com/2013/08/date-formats-in-r/ )
football_managers_merged$date_from <-  as.Date(football_managers_merged$date_from, format = "%d-%b-%y")
football_managers_merged$date_until <- as.Date(football_managers_merged$date_until, format = "%d-%b-%y")
football_managers_merged$date <- as.Date(football_managers_merged$date, format = "%d/%m/%Y")

# You can think about other variables that may be computed and potentially useful


######################################################################################################################################################################
#########################################################             Data Aggregation            ##########################################################################
######################################################################################################################################################################

# We want to analyse the entire seasons and aggregate our data accordingly

# Example: aggregate data and keep only one entry - you may add further variables here
football_managers_seasons <- group_by(football_managers_merged, season, team) %>% 
  reframe(
    points = sum(points),
    goals = sum(goals),
    promoted = mean(promoted),
    goals_opponent = sum(goals_opponent),
    points_lastseason = mean(points_lastseason),
    team = first(team),
    caretaker = mean(caretaker)
  ) %>% 
  as.data.frame()

# let's ensure it's unique
football_managers_seasons <- data.frame(unique(football_managers_seasons))    


######################################################################################################################################################################
#########################################################           Data exploration        ##########################################################################
######################################################################################################################################################################

# Obtain an overview of all variables, including summary statistics
summary(football_managers_seasons)

# Correlation matrix for numeric and binary dummy coded variables (you can consider removing variables that are not interesting)
round(cor(football_managers_seasons %>% select_if(is.numeric)),2)

# Let's take a look at our outcome variable using histograms (log-transformed versus not)
hist(football_managers_seasons$points)
hist(log(football_managers_seasons$points))  

# Let's check qq-plots oo
qqnorm(football_managers_seasons$points) 
qqline(football_managers_seasons$points)

qqnorm(log(football_managers_seasons$points)) 
qqline(log(football_managers_seasons$points))

# Our choice of outcome variable may depend on theoretical reasons
# But if there is no theoretical preference, then choosing the variable format which looks more 'normally distributed' should be preferred (more likely to have normal residuals)

# For our exercises 1-3, we will use the raw points only

######################################################################################################################################################################
#########################################################             Exercise 1            ##########################################################################
######################################################################################################################################################################

## Goal: Maximise R2

# Note: You should consider creating further variables (in addition to the ones we have generated above)


# Try your own model - decide which variables to include
model_e1.fit <- lm(points ~. + poly(points, 2) + poly(goals, 2), data=football_managers_seasons)
summary(model_e1.fit)





######
# Example:
ols.fit1 <- lm(points ~ points_lastseason, data=football_managers_seasons)
summary(ols.fit1)



######################################################################################################################################################################
#########################################################             Exercise 2            ##########################################################################
######################################################################################################################################################################
          
## Goal: Maximise Adjusted R2 

# Try your own model - decide which variables to include

# Example:                                              
ols.fit2 <- lm(points~ points_lastseason, data=football_managers_seasons)
summary(ols.fit2)

# We can also compare adjusted R2 with AIC (both have a penalty for overfitting)
AIC(ols.fit2)

## Manual calculation for lm object - note: AIC for the lm actually adds an extra parameter for the constant term (hence the +1 at the end)
nrow(football_managers_seasons)*(log(2*pi) + 1 + log((sum(ols.fit2$residuals^2)/nrow(football_managers_seasons)))) + ((length(ols.fit2$coefficients)+1)*2)


# Just in case: Stepwise optimisation also works for OLS
# Using stepwise AIC selection (forward, backward, both directions) from MASS library 
selected.ols.fit2 <-stepAIC(ols.fit2)
summary(selected.ols.fit2)
AIC(selected.ols.fit2)



######################################################################################################################################################################
#########################################################             Exercise 3            ##########################################################################
######################################################################################################################################################################
            
## Goal: Predict holdout sample - Season 2018, metric MAE

# Repeat data preparation for the 2018 data set
football_managers_2018 <- read_csv("football_managers_week1_2018.csv")

# Encode naming columns
Encoding(football_managers_2018$manager_name)
football_managers_merged$manager_2018 <-  iconv(football_managers_merged$manager_name, to='UTF-8')

# create dummy if team is new in premier league
football_managers_2018$promoted <-  ifelse(is.na(football_managers_2018$points_lastseason),1, 0)

# replace missing values NA from points last season with 0 (these are teams that got promoted to the premier league)
football_managers_2018$points_lastseason <- ifelse(is.na(football_managers_2018$points_lastseason), 0, football_managers_2018$points_lastseason)


# Encode naming columns
Encoding(football_managers_2018$manager_name)
football_managers_2018$manager_name <-  iconv(football_managers_2018$manager_name, to='UTF-8')

# We can create new variables, for example, a dummy indicating whether team is new in the premier league
football_managers_2018$promoted <-  ifelse(is.na(football_managers_2018$points_lastseason),1, 0)

# check frequency of new variable
table(football_managers_2018$promoted)


# We could also properly format the dates again
football_managers_2018$date_from <-  as.Date(football_managers_2018$date_from, format = "%d-%b-%y")
football_managers_2018$date_until <- as.Date(football_managers_2018$date_until, format = "%d-%b-%y")
football_managers_2018$date <- as.Date(football_managers_2018$date, format = "%d/%m/%Y")


# Aggregate data for 2018
# Don't forget to add any other variables that you may have created for the other data set (only if you did)
football_managers_2018_season <- group_by(football_managers_2018, season, team) %>% 
  summarise(
    points = sum(points),  # True points
    goals = sum(goals),
    goals_opponent = sum(goals_opponent),
    points_lastseason = mean(points_lastseason),
    caretaker = mean(caretaker)
  ) %>% 
  ungroup()

######################################################################################################################################################################

# Fit the OLS model
ols.fit3 <- lm(points ~  points_lastseason, data = football_managers_seasons)

# Try your own model - decide which variables to include


# Predict points for the 2018 season using the model
predicted_points_2018 <- predict(ols.fit3, newdata = football_managers_2018_season)


# Calculate the Mean Absolute Error (MAE) manually
true_points_2018 <- football_managers_2018_season$points
mae_value <- mean(abs(true_points_2018 - predicted_points_2018))


# Print the results
print(football_managers_2018_season)
print(paste("Mean Absolute Error (MAE) for the 2018 season and our chosen model: ", mae_value))


######################################################################################################################################################################
#########################################################             Exercise 4            ##########################################################################
######################################################################################################################################################################

## Goal: We are interested in hiring a manager now and want to leverage the data we have

# You need to decide which parts of the data you want to use? Until 2018 or 2017?

# In case you want to combine both data set, we can create this as follows
football_managers_combined <- bind_rows(football_managers_merged, football_managers_2018)


# In case you haven't done this yet, it's important to create a workable manager_name variable for the aggregate data set
# We need to check whether there were multiple managers in one season and create a unique variable for each team
football_managers_combined$manager_name_clean <- football_managers_combined$manager_name

# Adding count variable 
football_managers_combined <- football_managers_combined %>% add_count(manager_name, team, season)   

# add new name if mixed team
football_managers_combined$manager_name_clean <- ifelse(
  football_managers_combined$n < 38,
  paste0("Multiple manager ", football_managers_combined$team),
  football_managers_combined$manager_name_clean
)


########################## Part A ##########################
# Aggregate data again and keep only one entry per team per season for manager variable
football_managers_seasons <- group_by(football_managers_combined, season, team) %>% 
  reframe(
    points = sum(points),
    goals = sum(goals),
    promoted = mean(promoted),
    goals_opponent = sum(goals_opponent),
    points_lastseason = mean(points_lastseason),
    team = first(team),
    manager_name = first(manager_name_clean),
    caretaker = mean(caretaker)
  ) %>% 
  as.data.frame()



# Manager performance - simple summary stats and ranking by avg_points
managers_performance_season <- football_managers_seasons %>%
  group_by(manager_name) %>%
  summarise(
    total_points = sum(points),
    avg_points = mean(points),
    num_seasons = n(),
    num_teams = n_distinct(team)
  ) %>%
  arrange(desc(avg_points))  # Rank by average points in descending order

# Print manager performance data
print(managers_performance_season)


########################## Part B ##########################
## Question - how to set up the OLS regression model to answer this question?

# Fit the OLS model - example
ols.fit4 <- lm(points ~ manager_name + points_lastseason, data = football_managers_seasons)
summary(ols.fit4)

# Try your own model - decide which variables to include


## Final check for your chosen model
# Normality of residuals for valid inference
# histogram
hist(ols.fit4$residuals)


# Possible additional graph
qqnorm(ols.fit4$residuals)
qqline(ols.fit4$residuals)

# Homoscedasticity check using Breusch-Pagan test
bptest(ols.fit4)


########################## Part C ##########################
# Standardise the numeric variables using scale(),don't forget to adjust for the variables of your own model
football_managers_combined_std <- football_managers_combined %>%
  mutate(across(c(goals, caretaker, goals_opponent, points_lastseason), ~ scale(.)[,1]))


# Fit the OLS model - example (don't forget to adjust for your own model)
ols.fit4z <- lm(points ~ manager_name + points_lastseason, data = football_managers_combined_std)
summary(ols.fit4z)

# Extract the coefficients
coefficients <- summary(ols.fit4z)$coefficients
coefficients_df <- as.data.frame(coefficients)
coefficients_df$Variable <- rownames(coefficients_df)
rownames(coefficients_df) <- NULL

# Let's plot the coefficients using a bar chart
ggplot(coefficients_df, aes(x = reorder(Variable, Estimate), y = Estimate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Contribution of Variables to Points Model",
       x = "Variables",
       y = "Standardized Coefficients")