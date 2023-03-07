library(jsonlite)
library(tidyverse) # TO USE TIBBLE
# able to get data without downloading it and then uploading it 
# API has a limit that it caps at 1000, 
# you can edit the url and input the limit as higher if you want some different amount
# LIKE THIS
## fromJSON("https://data.cityofnewyork.us/resource/nwxe-4ae8.json?$limit=5")

nyc_trees = 
  fromJSON("https://data.cityofnewyork.us/resource/nwxe-4ae8.json") %>%
  as_tibble()
nyc_trees


#plot location of trees
nyc_trees %>% 
  select(longitude, latitude, stump_diam, spc_common, spc_latin, tree_id) %>% 
  mutate_at(vars(longitude:stump_diam), as.numeric) %>% 
  ggplot(aes(x=longitude, y=latitude, size=stump_diam)) + 
  geom_point(alpha=0.5) +
  scale_size_continuous(name = "Stump diameter") +
  labs(
    x = "Longitude", y = "Latitude",
    title = "Sample of New York City trees",
    caption = "Source: NYC Open Data"
  )


### FRED EXAMPLE
library(fredr)
library(tidyverse)

fredr_set_key(Sys.getenv("FRED_API_KEY"))

#the hard way:
#https://api.stlouisfed.org/fred/series/observations?series_id=GNPCA&api_key=YOUR_API_KEY&file_type=json
#then handle the JSON yourself

#the easy way
us_gnp <- fredr(
  series_id = "GNPCA",
  observation_start = as.Date("1900-01-01"),
  observation_end = as.Date("2023-01-01")
)


#graph
us_gnp %>%
  ggplot(aes(date, value)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x="Date", y="2012 USD (Billions)",
    title="US Real Gross National Product", caption="Source: FRED"
  )



