################################################################################
#### Intermediate Econometrics - HOMEWORK 3 - Thomas DESIRE's Coding Sample ####
################################################################################



# Download and install necessary R packages:

install.packages("tidyverse")
install.packages("AER")
install.packages("car")

library(tidyverse)
library(AER) # for dataset CollegeDistance
library(car)



##################
### EXERCISE 1 ###
##################



### Q0) Attach the AER package and load the CollegeDistance data:

data("CollegeDistance")
CollegeDistance$mcollege <- ifelse(CollegeDistance$mcollege=="yes",1,0)
CollegeDistance$ethnicity <- ifelse(CollegeDistance$ethnicity=="afam",1,0)
college <- CollegeDistance # Rename dataset
view(college)


### ORDINARY LEAST SQUARES APPROACH ###


### Q1)

    ## 1) Let's estimate model 1 by OLS:

# We create a new variable log(wage) (logwage):
college$logwage=log(college$wage)

# OLS estimation:
model1 <- lm(logwage ~ education + unemp + ethnicity + gender + urban, data = college)  
summary1 <- summary(model1)

# Extract the coefficient and standard error for education:
coefficient1 <- summary1$coefficients["education", "Estimate"]
stderror1 <- summary1$coefficients["education", "Std. Error"]

# Print the results:
print(paste("Coefficient for education:", coefficient1))
print(paste("Standard Error for education:", stderror1))

    ## 2) Significance test of \beta_1:

# t statistic:
print(coefficient1/stderror1)

# Critical value:
print(qnorm(p=.05/2, lower.tail=FALSE))

# So we cannot reject H_0 because t statistic is smaller than critical value.


### Q2)

    ## 2) and 3) First stage estimation by OLS:

first_stage_1 <- lm(education ~ unemp + ethnicity + gender + urban + distance, data = college)
summary(first_stage_1)

    ## 4) Significance test of coefficient for distance:

summary2 <- summary(first_stage_1)

# Extract the coefficient and standard error for distance:
coefficient2 <- summary2$coefficients["distance", "Estimate"]
stderror2 <- summary2$coefficients["distance", "Std. Error"]

# Print the results
print(paste("Coefficient for distance:", coefficient2))
print(paste("Standard Error for distance:", stderror2))

# t statistic:
print(coefficient2/stderror2)

# Critical value:
print(qnorm(p=.05/2, lower.tail=FALSE))

# So we reject H_0.

    ## 5. Simple IV method:

educ_fit <- fitted(first_stage_1) # Fitted values of endogenous variable from stage 1

model_IV <- ivreg(logwage ~ education + unemp + ethnicity + gender + urban | 
                      unemp + ethnicity + gender + urban + distance, data = college)

summary(model_IV)

summary3 <- summary(model_IV)

# Extract the coefficient and standard error for education:
coefficient3 <- summary3$coefficients["education", "Estimate"]
stderror3 <- summary3$coefficients["education", "Std. Error"]

# Print the results:
print(paste("Coefficient for education:", coefficient3))
print(paste("Standard Error for education:", stderror3))

# t statistic:
print(coefficient3/stderror3)

# Critical value:
print(qnorm(p=.05/2, lower.tail=FALSE))

# So we reject H_0.

    ## 6) Hausman test:

# We compute the test statistic:
betaOLS_1 <- coefficient1
varOLS_1 <- (stderror1)^2
beta2SLS_1 <- coefficient3
var2SLS_1 <- (stderror3)^2
hausman <- (beta2SLS_1 - betaOLS_1) / sqrt(var2SLS_1 - varOLS_1)
hausman
# |hausman|>1.96 so we reject H_0 (exogeneity of education at the level 5%)


### Q3)

    ## 1) We estimate \beta_1 by the 2SLS method:

# Stage 1:
model_distance_mcollege.1 <- lm(education ~ unemp + ethnicity + 
                                  gender + urban + 
                                  distance + mcollege, 
                                data = college)
educ_fit_2 <- fitted(model_distance_mcollege.1)

# Stage 2:
model_distance_mcollege.2 <-  lm(logwage ~ educ_fit_2 + unemp + 
                                   ethnicity + gender + 
                                   urban , data = college)
summary(model_distance_mcollege.2)

summary4 <- summary(model_distance_mcollege.2)

# Extract the coefficient and standard error for education:
coefficient4 <- summary4$coefficients["educ_fit_2", "Estimate"]
stderror4 <- summary4$coefficients["educ_fit_2", "Std. Error"]

# Print the results:
print(paste("Coefficient for education:", coefficient4))
print(paste("Standard Error for education:", stderror4))

  ## 2) Sargan test:

# We can, for example, get the residuals from simple IV method:
IV_model_Q3 <- ivreg(logwage ~ education + unemp + ethnicity + gender + urban | 
                      unemp + ethnicity + gender + urban + distance + mcollege, data = college)
residuals_IV <- residuals(IV_model_Q3)
summary(IV_model_Q3)

# Sargan test:
sargan <- lm(residuals_IV ~ unemp + ethnicity + gender + urban + distance + mcollege, data = college)
summary(sargan)

# We compute the test statistic:
linearHypothesis(sargan, c("distance = 0", "mcollege = 0")) # Test joint nullity coeffs instruments
l <- 2
J <- l*12.407
J

# Under H0, the test statistic J follows a chi-square distribution with degree of freedom 2-1=1.
# Compute the p-value:
pvalue  <-  1 - pchisq(J, 1)
pvalue

# p-value < 0.05 so we reject H0 at the 5% level.
