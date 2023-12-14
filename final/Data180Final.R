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

# I can see the names of all the columns using : 
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
#Dimensions of the complete data (ComData): 
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

mode_num_card_inq = get_mode(ComData$num_card_inq_24_month)
mode_num_card_inq
mode_tot_amount_past_due = get_mode(ComData$tot_amount_currently_past_due)
mode_tot_amount_past_due
mode_credit_age = get_mode(ComData$credit_age)
mode_credit_age

# Plot histograms for each of the three variables

# Histogram for num_card_inq_24_month: this is right skewed
Plot1 = ggplot(ComData, aes(x = num_card_inq_24_month)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of num_card_inq_24_month")
Plot1

# Histogram for tot_amount_currently_past_due: nothing on the graph
Plot2 = ggplot(ComData, aes(x = tot_amount_currently_past_due)) +
  geom_histogram(binwidth = .5, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Histogram of tot_amount_currently_past_due")
Plot2

# Histogram for credit_age: this is bell shaped
Plot3 = ggplot(ComData, aes(x = credit_age)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Histogram of credit_age")
Plot3

# Bar Graph for 'Def_Ind'
Bar1 = ggplot(ComData, aes(x = Def_ind)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Graph of Def_Ind", x = "Def_Ind", y = "Count")
Bar1

# Bar graph for 'rep_education'
Bar2 = ggplot(ComData, aes(x = rep_education)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Bar Graph of rep_education", x = "rep_education", y = "Count")
Bar2

# Histogram of 'rep_income'
Plot4 = ggplot(ComData, aes(x = rep_income)) +
  geom_histogram(binwidth = 10000, fill = "salmon", color = "black", alpha = 0.7) +
  labs(title = "Histogram of rep_income", x = "rep_income", y = "Frequency")
Plot4

# Box plot of 'tot_balance'
Box1 = ggplot(ComData, aes(y = tot_balance)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of tot_balance", y = "tot_balance")
Box1

# 5 number summary of the box plot for 'tot_balance'
five_number_summary <- summary(ComData$tot_balance)
five_number_summary

# Outliers using the IQR method
Q1 = quantile(ComData$tot_balance, 0.25)
Q3 = quantile(ComData$tot_balance, 0.75)
IQR = Q3 - Q1
lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR

# Outliers 
outliers = ComData$tot_balance[ComData$tot_balance < lower_bound | ComData$tot_balance > upper_bound]
outliers

