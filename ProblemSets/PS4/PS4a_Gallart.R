##download JSON file
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20230209&lang=en"')

##print file
system('cat dates.json')

library(jsonlite)
library(tidyverse)

#convert to df
mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])

#obj type
class(mydf)
class(mydf$date)

#list n rows
head(mydf, n = 20)


