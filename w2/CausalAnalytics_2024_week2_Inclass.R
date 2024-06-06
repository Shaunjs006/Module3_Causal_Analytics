######################################################################################################################################################################
#######################   Melbourne Business School- In-class exercise - Causal Analytics - Class 2 2024 Nico Neumann   ##############################################
######################################################################################################################################################################
## clean memory
rm(list=ls()) 

## (1) Define the packages that will be needed
packages <- c( 'tidyverse', 'lmtest')

## (2) Install them if not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

######################################################################################################################################################################

## Add your path to the data here
foldername <- "C:\\Users\\N.Neumann" 

## Add your path to the data here
# foldername <- "C:\\Users\\nicon" 

setwd(paste0(foldername,"\\OneDrive\\2024_Causal_Analytics_TAs\\2024_Causal_Class2"))

#################### load libraries (install first if necessary)
library(tidyverse)
library(lmtest)


# increase print out
options(max.print=1000000)
## avoid scientific notation
options(scipen=99)

######################################################################################################################################################################

## load data 
df <- read.csv("salary_data.csv", header=T, fileEncoding="UTF-8-BOM")

### correlation matrix
round(cor(df %>% select_if(is.numeric)),2)

## age and experience are highly correlated - but careful, these are conceptually different.
## we need to check our causal diagram to see whether we can omit one of these.

## let's check the distribution of our outcome variable
hist(df$salary)
hist(log(df$salary))

# no clear pattern -> let's stick with salary (no log) for now (we will check residuals again)


### Exercise 5
# Diagram A
modelA <- lm(salary~ male + education + exp, data=df)  
summary(modelA)

# Diagram B
modelB <- lm(salary~ male + education + exp + age, data=df)  
summary(modelB) 


### Exercise 6
# a)
model.exp <- lm(exp ~ male + age + education , data=df)  
summary(model.exp) 
# education     0.01167  ns
# age           0.85736  ***
# male          0.3892   ns

# b) We omit male (which is non significant)
model.nomale <- lm(exp ~ age + education, data=df)  
summary(model.nomale ) 
# education     0.02171  ns
# age           0.8658 ***

    # Extra: We omit education (which is non significant)
    model.noeduc <- lm(exp ~ age + male, data=df)  
    summary(model.noeduc ) 
    # male          0.3906 ns
    # age           0.8562 ***

# c) What happens if we omit age (which was significant)?
model.noage <- lm(exp ~ male + education , data=df)  
summary(model.noage) 
# education    -2.5547  ***   (we will try to recover this incorrect estimate below)
# male          2.8985  ***  (this is the effect that is mediated through age)

## -> Omitted variable bias would make us conclude that education and male affect experience
## -> age is mediating the effect of male (without the mediator, male has an impact)

# d) OVB
reverse.fit <- lm(age ~ education + male, data=df)  
summary(reverse.fit) 

OVB = model.exp$coefficients['age'] * reverse.fit$coefficients['education']
print(OVB)   ## -2.566361

## confirm the result of C) is the sum of correct effect and OVB
print(model.exp$coefficients['education'] + OVB)  # -2.5547  qed



### Exercise 7
# a)
model <- lm(education ~ age + male, data=df)  
summary(model)  # No impact of male on education (remove arrow)

# b) Check impact of male on age
model <- lm(age ~ male, data=df)  
summary(model) # 3.6594 ***


# c) Impact of age on salary


# Option 1: Through mediators

## repeat model A
modelA <- lm(salary~ male + education + exp, data=df)  
summary(modelA)

## indirect way: path 1
model1 <- lm(education ~ age, data=df)  
summary(model1)  # 

# path 2:
model2 <- lm( exp~ age, data=df)  
summary(model2) 

##  Combined all paths (use variables from model)
indirect.path1effect <- model1$coefficients['age'] * modelA$coefficients['education']
indirect.path2effect <- model2$coefficients['age'] * modelA$coefficients['exp']

indirect.path1effect + indirect.path2effect  # 1.679614   


# Option 2: Don't include mediators, but control for confounder male
model.quick <- lm(salary ~ age + male, data=df)  
summary(model.quick)  # 1.67653    (very close to 1.679, a bit of noise in the data)




### Exercise 8
# Let's run the correct model without any transformation again for a benchmark
modelA <- lm(salary~ male + education + exp, data=df)  
summary(modelA)
AIC(model)  # 1875.873

# a) factor for education
modelA2 <- lm(salary~ male + factor(education) + exp, data=df)  
summary(modelA2)
AIC(modelA2)  # 1717.47

# b) log transformation for exp
modelA.int <- lm(salary~ male + factor(education) + log(exp+1), data=df)  
summary(modelA.int)
AIC(modelA.int)  # 1685.739

# c) interaction of education and eperience 
modelA.log <- lm(salary~ male + factor(education) * log(exp+1), data=df)  
summary(modelA.log)
AIC(modelA.log)  # 1689.805

# d) a squared term for exp 
modelA.sq <- lm(salary~ male + factor(education) + exp + I(exp^2), data=df)  
summary(modelA.sq)
AIC(modelA.sq )  # 1622.822


# e) final checks
# normal residuals
hist(modelA.sq$residuals) ## looks good

qqnorm(modelA.sq$residuals)
qqline(modelA.sq$residuals)  ## looks good

# Breusch-Pagan test
bptest(modelA.sq)  ## we can assume homoscedasticity