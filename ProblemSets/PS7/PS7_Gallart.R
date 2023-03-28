library(tidyverse)
library(tidyr)
library(mice)
library(modelsummary)
library(dplyr)

# 4. load wages.csv
setwd("ProblemSets/PS7")
getwd()
wages <- read.csv("wages.csv")
head(wages)
dim(wages)

# 5. drop observations where either hgc or tenure are missing
wages <- wages %>% drop_na(hgc, tenure)

# 6. Create a summary table
datasummary_skim(wages, output='latex', histogram= FALSE)

### At what rate are log wages missing? 25%
### Do you think the logwage variable is most likely to be MCAR, MAR, or MNAR? MNAR because of the type of data it is


# 7.1 estimate the regression using only complete cases (i.e. do listwise deletion onthe log wage variable ... 
# this assumes log wages are Missing Completely AtRandom)
wagesMCAR <- na.omit(wages)
wagesMCAR <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, wagesMCAR)
wagesMCAR
modelsummary(wagesMCAR)

 
# 7.2 perform mean imputation to fill in missing log wages
wagesMI <- wages %>% drop_na(hgc, tenure)
logwage_mean <- mean(wages$logwage, na.rm=TRUE)
wagesMI$logwage[is.na(wagesMI$logwage)] <- logwage_mean

wagesMI <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, wagesMI)
summary(wagesMI)


# 7.3 impute missing log wages as their predicted values from the complete cases regression above (i.e. this would be consistent with the “Missing at Random” assumption)
wagesMAR <- wages
wagesMAR$logwage_fitted <- predict(wagesMCAR,newdata = wagesMAR)
wagesMAR$logwage[is.na(wagesMAR$logwage)] <- wagesMAR$logwage_fitted[is.na(wagesMAR$logwage)]

wagesMAR <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, wagesMAR)
summary(wagesMAR)


# 7.4 use the mice package to perform a multiple imputation regression model

wagesMULTI <- mice(wages[, c("logwage", "hgc", "college", "tenure", "age", "married")], 
                   m = 5, maxit = 50, method = "pmm", seed = 500)

# Fit the regression model on each imputed dataset
wagesMICE <- with(data = wagesMULTI, exp = lm(logwage ~ hgc + college + tenure + 
                                                     I(tenure^2) + age + married))


wagesMICE_pool <- pool(wagesMICE)
summary(wagesMICE_pool)

# 7.5 use modelsummary to create one regression table which has the estimates of the four regression models
regmods <- list()
regmods[['Complete Cases Regression']] <- wagesMCAR
regmods[['Mean Imputation Regression']] <- wagesMI
regmods[['Fitted Value Imputation Regression']] <- wagesMAR
regmods[['Multiple Imputation Regression']] <- wagesMICE_pool

## ATTEMPT 1
# modelsummary(regmods,stars=TRUE,output="latex")

## ATTEMPT 2
#output_file <- "regression_table.tex"
#cat(modelsummary(regmods, stars = TRUE, output = "latex"), file = output_file)

## ATTEMPT 3
fileConn<-file("output.tex")
cat(modelsummary(regmods,stars=TRUE,output="latex"), file=fileConn, sep="\n")
close(fileConn)


regmods #this was the only one that i was able to get to print
modelsummary(regmods,stars=TRUE,output="latex")


