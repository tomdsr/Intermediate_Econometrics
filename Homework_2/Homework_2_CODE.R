################################################################################
#### Intermediate Econometrics - HOMEWORK 2 - Thomas DESIRE's Coding Sample ####
################################################################################



# Install R packages:

install.packages("tidyverse")
install.packages("lmtest")
install.packages("sandwich")
install.packages("car")
install.packages("gap")
install.packages("MASS")

library(tidyverse)  # for importing and reading data sets
library(lmtest)     # for coefci()
library(sandwich)   # for vcovHC()
library(car)        # for linearHypothesis()    
library(gap)        # for chow.test()
library(MASS)

# Set the proper working directory.



##################
### Exercise 2 ###
##################



# Read Trade data set:
trade = read_delim("Trade.txt", delim = "\t", trim_ws = TRUE)
head(trade) # (To display the first rows of the trade data frame)

########## Question 1)

# Let's create a new variable: the log of sulfdm (logsulfdm)
trade$logsulfdm = log(trade$sulfdm)

# Let's estimate the following model using OLS:
model1 <- lm(logsulfdm ~ inc + pwtopen + polity + lareapc, data = trade)
summary(model1)

########## Question 2)

# We create the interaction term between inc and oecd (inc_oecd) [inc is already in log!]:
trade$inc_oecd=trade$inc*trade$oecd

# We now estimate the new model (with the interaction term and oecd) using OLS:
model2 <- lm(logsulfdm ~ inc + pwtopen + polity + lareapc + oecd + inc_oecd, data = trade)  
summary(model2)

########## Question 3)

# Let's get the residuals and squared residuals from model1:
resOLS <- residuals(model1)
trade$resOLS2 <- resOLS^2

# Let's create the squared variables for the test:
trade$inc2 <- (trade$inc)^2
trade$pwtopen2 <- (trade$pwtopen)^2
trade$polity2 <- (trade$polity)^2
trade$lareapc2 <- (trade$lareapc)^2

# Auxiliary regression:
aux <- lm(resOLS2 ~ inc + pwtopen + polity + lareapc + inc2 + pwtopen2 + polity2 + lareapc2, data = trade)
summary(aux)

# Non robust Variance-Covariance Matrix
vcov(aux)
# Robust Variance-Covariance Matrix
vcovHC(aux)
# F statistic
linearHypothesis(aux, c("inc=0", "pwtopen=0", "polity=0", "lareapc=0", 
                        "inc2=0", "pwtopen2=0", "polity2=0", "lareapc2=0"),
                 vcov. = vcovHC(aux))

########## Question 4)

# 4)a) We use the HCCME to reestimate model1
coeftest(model1, vcov. = vcovHC(model1))

# 4)b) FGLS
trade$lresOLS2 <- log(trade$resOLS2) #  log of the square of the OLS residuals
# Regress it on an intercept and the explanatory variables:
laux <- lm(lresOLS2 ~ inc + pwtopen + polity + lareapc, data = trade)
summary(laux)
ghat <- fitted(laux) # Extract the fitted values
hhat <- exp(ghat) # Estimator of the variance
# Then we estimate the initial model by WLS:
FGLS <- lm(logsulfdm ~ inc + pwtopen + polity + lareapc, weights = I(1 / hhat), data = trade)
summary(FGLS)

# 4)c) (GLS/) WLS using the option weights in the lm function
WLS <- lm(logsulfdm ~ inc + pwtopen + polity + lareapc, weights = I(1 / inc), data = trade)
summary(WLS)



##################
### Exercise 3 ###
##################



# Read data cps_85
cps_85 <- read.delim("cps_85.txt")
head(cps_85)

########## Question 1)

# Let's create a new variable: the log of wage (logwage)
cps_85$logwage=log(cps_85$wage)

# OLS logwage ~ education + experience + married + south
model_logwage <- lm(logwage ~ education + experience + married + south, data = cps_85)  
summary(model_logwage)

########## Question 2)

# We produce the confidence intervals:
confint(model_logwage, level = 0.99)

########## Question 3)

# We create subdatasets for females and for males
df_f = subset(cps_85, female == 1,)
df_m = subset(cps_85, female == 0,)

# Chow/Stability test:
chow.test(df_f$logwage, c(df_f$education, df_f$experience, df_f$married, df_f$south),
          df_m$logwage, c(df_m$education, df_m$experience, df_m$married, df_m$south))
