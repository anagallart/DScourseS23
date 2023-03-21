library(tidyverse)
library(xts)
library(TTR)
library(zoo)
library(quantmod)
library(dplyr)

# 3. Get data from FRED

## Personal Income and Personal Consumption Expenditures 
## (both in Billions of Dollars) from Jan 1959 to Jan 2023
getSymbols('PI', src = "FRED")
PIdf <- data.frame(date = index(PI), value = coredata(PI)) #make it into a dataframe

getSymbols('PCE', src = "FRED")
PCEdf <- data.frame(date = index(PCE), value = coredata(PCE))

## Bind the PI and PCE data by column since the data points are from the same date and cover the same range
PI_CE <- bind_cols(PIdf, PCEdf$PCE)

## Change the column names and see a bit of the data
colnames(PI_CE) <- c("Date", "Personal_Income", "Personal_Consumption_Expenditures")
head(PI_CE)


# 4. Create vizualizations 
library(ggplot2)

## the whole range of data
ggplot(PI_CE, aes(x = Date)) +
  geom_line(aes(y = `Personal_Income`, color = "Personal Income")) +
  geom_line(aes(y = `Personal_Consumption_Expenditures`, color = "Personal Consumption Expenditures")) +
  scale_color_manual(values = c("Personal Income" = "blue", "Personal Consumption Expenditures" = "red")) +
  labs(x = "Date", y = "Billions of dollars", title = "Personal Income and Personal Consumption Expenditures") +
  theme_minimal() + guides(color= "none") +
  geom_text(aes(x = as.Date("1997-04-01"), y = 12000, label = "Personal Income", color = "Personal Income"), size = 3) +
  geom_text(aes(x = as.Date("2010-01-01"), y = 4500, label = "Personal Consumption Expenditures", color = "Personal Consumption Expenditures"), size=3)

## 2019- 2023 data, want to see the impacts and recovery of pandemic
ggplot(PI_CE, aes(x = Date)) +
  geom_line(aes(y = `Personal_Income`, color = "Personal Income")) +
  geom_line(aes(y = `Personal_Consumption_Expenditures`, color = "Personal Consumption Expenditures")) +
  scale_color_manual(values = c("Personal Income" = "blue", "Personal Consumption Expenditures" = "red")) +
  labs(x = "Date", y = "Billions of dollars", title = "Personal Income and Personal Consumption Expenditures") +
  theme_minimal() + guides(color= "none") +
  geom_text(aes(x = as.Date("2022-04-01"), y = 19500, label = "Personal Income", color = "Personal Income"), size = 3) +
  geom_text(aes(x = as.Date("2021-10-01"), y = 13000, label = "Personal Consumption Expenditures", color = "Personal Consumption Expenditures"), size=3) +
  xlim(as.Date("2019-01-01"), as.Date("2023-01-01")) +
  geom_vline(xintercept = seq(as.Date("2019-01-01"), as.Date("2023-01-01"), by = "quarter"), color = "gray90") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## difference between income and consumption expenditure

### Create a new column for the difference between Personal Income and Personal Consumption Expenditure
PI_CE$Difference <- PI_CE$Personal_Income - PI_CE$Personal_Consumption_Expenditures

### Plot the difference over time using ggplot2
ggplot(PI_CE, aes(x = Date, y = Difference)) +
  geom_line() +
  labs(x = "Date", y = "Billions of dollars", title = "Difference between Personal Income and Personal Consumption Expenditures") +
  theme_minimal()


