install.packages("dplyr")
library(MASS)
library(dplyr)

ls(Cars93)

Cars93_Ex2 = filter(Cars93, Type == "Small")
head(Cars93_Ex2)

table(Cars93$Type)

Cars93_Ex3 = select(filter(Cars93, Type == "Small"), Model, EngineSize, Horsepower, MPG.highway)
head(Cars93_Ex3)

# filter: lets you filter out the data you dont want... for your rows
# select: lets you select the columns you want
# == test for equality
# | is an or operator.. called ampersand
# %in% is used to check whether a value is in a list of possible values
# %>% is the pipe operator... alternate of nesting