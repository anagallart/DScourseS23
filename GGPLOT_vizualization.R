library(tidyverse) # includes ggplot
library(ggthemes)
library(hexbin)

df <- mpg

## install.packages("ggthemes")
## install.packages("hexbin")

# Example: mpg dataset (city and highway fuel economy)

# Basic scatterplot
ggplot(data = mpg, aes(x = cty, y = hwy)) # this plot is empty because we haven't provided any geom's
mpg

ggplot(data = mpg, aes(x = cty, y = hwy))+
  geom_point()) # have to use "+" to add geom's

ggplot(data = mpg, aes(x = cty, y = hwy)) + 
  geom_jitter() # this will always fall on the interger values

ggplot(data = mpg, aes(x = cty, y = hwy)) + 
  geom_jitter() + theme_bw() # if you don't want a gray theme, but it has a white background 

ggplot(data = mpg, aes(x = cty, y = hwy)) + 
  geom_jitter() + theme_minimal() # this doesnt have a backgroung

ggsave("badgraph.png") #bianary file
# ggsave("badgraph.eps") # this will give you the source code for an image, 
                          #  you can change stuff, prefered for opening it on a text editor

# use a size variable, should be continuous not categorical/discrete
ggplot(data = mpg, aes(x = cty, y = hwy, size=as.factor(manufacturer))) + 
  geom_jitter() + theme_minimal()

# use color instead of size
ggplot(data = mpg, aes(x = cty, y = hwy, color=as.factor(manufacturer))) + 
  geom_jitter() + theme_minimal()

# look at year to group them by color instead of manufacturere
ggplot(data = mpg, aes(x = cty, y = hwy, color=year)) + geom_jitter() + theme_minimal()
#there's only really teo values here so why is there a gradient, that's confusing and stupid

# waht about when you dont say "factor"?
ggplot(data = mpg, aes(x = cty, y = hwy, color=as.numeric(manufacturer))) + 
  geom_jitter() + theme_minimal() # this loses meaning and looks stupid

# this is much better than the gradient year one 
ggplot(data = mpg, aes(x = cty, y = hwy, color=as.factor(year))) + geom_jitter() + theme_minimal()


# Create sub-plots with facet wrap
ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_jitter() + 
  theme_minimal() + facet_wrap(vars(as.factor(manufacturer)), ncol = 4)

## Better way to do it is by year
# Create sub-plots with facet wrap
ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_jitter() + 
  theme_minimal() + facet_wrap(vars(as.factor(year)), ncol = 2)


