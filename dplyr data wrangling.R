install.packages("dplyr")
library(MASS)
library(dplyr)

ls(Cars93)

Cars93_Ex2 = filter(Cars93, Type == "small")
head(Cars93_Ex2)

table(Cars93$Type)