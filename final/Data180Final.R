# Set working directory and read the csv file
setwd("/Users/williamferris/Desktop/DATA-180-Will-Ferris/final")
data <- read.csv("/Users/williamferris/Desktop/DATA-180-Will-Ferris/final/loan_default_data_set.csv")

# Find the dimensions of the data : 20000 rows and 21 columns
dataDimensions = dim(data)
dataDimensions

# Names of the columns : 
head(data)

# type of data
str(data)

# Check for missing values in each column
missing_values <- colSums(is.na(data))
# Columns with missing values
columns_with_missing <- names(missing_values[missing_values > 0])
columns_with_missing
# The percentage of missing values in each column
percentage_missing <- missing_values / nrow(data) * 100
percentage_missing
