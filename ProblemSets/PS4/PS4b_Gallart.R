library(sparklyr)
library(tidyverse)

## connection to spark
spark_install(version = "3.0.0")
sc <- spark_connect(master = "local")

##df1 tibble for iris data
df1 <- as_tibble(iris)

##copy to spark
df <- copy_to(sc, df1)

##data type-- what was the type of each (.tex file)
class(df1)
class(df)

## column name comparison (in .tex file)
colnames(df1)
colnames(df) 

## RDD operation: select
df %>% select(Sepal_Length, Species) %>% head %>% print

## RDD operation: filter
df %>% filter(Sepal_Length>5.5) %>% head %>% print

## combine select and filter
df %>% select(Sepal_Length, Species) %>% filter(Sepal_Length>5.5) %>% head %>% print

## RDD operation: groupby
df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length),count = n()) %>% head %>% print

## RDD operation: sort
df2 %>% arrange(Species) %>% head %>% print
