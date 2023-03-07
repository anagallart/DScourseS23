# Webscraping page without an API
library(tidyverse)
library(rvest)

## Grammy Awards: viewership, ratings, and ad price through the years

### getting the data
url <- 'https://en.wikipedia.org/wiki/Grammy_Awards'
html_path <-'#mw-content-text > div.mw-parser-output > table:nth-child(135)'

### making it into a dataframe and confirming its class
df        <- read_html(url) %>% html_nodes(html_path) %>% html_table() %>% `[[`(1)
class(df)

### view dataframe
head(df, n=10)
tail(df, n=10)



# Webscraping with an API: I did two examples because I wanted to look at financial data as well as car sales
library(tidyverse)
library(xts)
library(TTR)
library(zoo)
library(quantmod)

## Total Vehicle Sales (in millions) from FRED

### Get data using getSymbols()
getSymbols('TOTALSA', src = "FRED")

plot(TOTALSA) # the yaxis is in millions of units

### Convert into a data frame
dfRED <- data.frame(date = index(TOTALSA), value = coredata(TOTALSA))

### View the data frame
tail(dfRED, n=50) # interested in the current car sale changes



## Car Manufacturer Returns

start_date <- as.Date("2019-01-01")
end_date <- as.Date("2023-03-03")

## Car manufacturers financial data
getSymbols(c("TSLA", "F", "TM", "HYMTF", "NSANY", "HMC"), src= "yahoo", from= start_date, to = end_date)


# Calculate returns for each stock
df.TSLA <- data.frame(diff(TSLA$TSLA.Adjusted)/lag(TSLA$TSLA.Adjusted))
df.F <- data.frame(diff(F$F.Adjusted)/lag(F$F.Adjusted))
df.TM <- data.frame(diff(TM$TM.Adjusted)/lag(TM$TM.Adjusted))
df.HYMTF <- data.frame(diff(HYMTF$HYMTF.Adjusted)/lag(HYMTF$HYMTF.Adjusted))
df.HMC <- data.frame(diff(HMC$HMC.Adjusted)/lag(HMC$HMC.Adjusted))
df.NSANY <- data.frame(diff(NSANY$NSANY.Adjusted)/lag(NSANY$NSANY.Adjusted))


#combined dataframe
df_cars <- data.frame(TSLA = df.TSLA, F = df.F, TM = df.TM,
                 HYMTF = df.HYMTF, HMC = df.HMC, NSANY = df.NSANY)


head(df_cars, n=30)
tail(df_cars, n=30)




