# Set working directory and read the csv file
setwd("/Users/williamferris/Desktop/DATA-180-Will-Ferris/final")
data = read.csv("/Users/williamferris/Desktop/DATA-180-Will-Ferris/final/loan_default_data_set.csv")

# Install Packages
install.packages("dplyr")
install.packages('ggplot2')
library(dplyr)
library(ggplot2)

# Find the dimensions of the data : 20000 rows and 21 columns
dataDimensions = dim(data)
dataDimensions

# Names of the columns : 
head(data)

# type of data
str(data)

# Check for missing values in each column
missing_values = colSums(is.na(data))
# Columns with missing values
columns_with_missing = names(missing_values[missing_values > 0])
columns_with_missing
# The percentage of missing values in each column
percentage_missing = missing_values / nrow(data) * 100
percentage_missing

# Data with no missing values
ComData = na.omit(data)
ComData
#Dimensions of ComData
dim_ComData = dim(ComData)
dim_ComData

# Summary of the data set
summary(ComData)

#Find mean and median of specific columns
MMM = summary(data %>% select(num_card_inq_24_month, tot_amount_currently_past_due, credit_age))
MMM

# Mode of 3 columns 
get_mode = function(x) {
  ux = unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mode_num_card_inq = get_mode(data$num_card_inq_24_month)
mode_num_card_inq
mode_tot_amount_past_due = get_mode(data$tot_amount_currently_past_due)
mode_tot_amount_past_due
mode_credit_age = get_mode(data$credit_age)
mode_credit_age

# Plot histograms for each of the three variables
# Histogram for num_card_inq_24_month
Plot1 = ggplot(data, aes(x = num_card_inq_24_month)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of num_card_inq_24_month")
Plot1

# Histogram for tot_amount_currently_past_due
Plot2 = ggplot(data, aes(x = tot_amount_currently_past_due)) +
  geom_histogram(binwidth = 100, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Histogram of tot_amount_currently_past_due")
Plot2

# Histogram for credit_age
Plot3 = ggplot(data, aes(x = credit_age)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Histogram of credit_age")
Plot3
     