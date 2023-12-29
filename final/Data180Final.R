#40/40. This is very well done Will. Good job. 
# Set working directory and read the csv file
setwd("/Users/williamferris/Desktop/DATA-180-Will-Ferris/final")
data = read.csv("/Users/williamferris/Desktop/DATA-180-Will-Ferris/final/loan_default_data_set.csv")

# Install Packages
install.packages("dplyr")
install.packages('ggplot2')
library(dplyr)
library(ggplot2)

## ---------------------------------------------------------------------------- ##

# DATA WRANGLING:

## ---------------------------------------------------------------------------- ##

# QUESTION 1a:

# Find the dimensions of the data : 
dataDimensions = dim(data)
dataDimensions

# there are 20000 rows and 21 columns

## ---------------------------------------------------------------------------- ##

# QUESTION 1b:

# I can see the names of all the columns using : 
colnames(data)

# tot_balance, avg_bal_cards, credit_age, credit_age_good_account, 
# credit_card_age, num_acc_30d_past_due_12_months, num_acc_30d_past_due_6_months,
# num_mortgage_currently_past_due, tot_amount_currently_past_due, num_inq_12_month, 
# num_card_inq_24_month, num_card_12_month, num_auto_.36_month,  uti_open_card, 
# pct_over_50_uti, uti_max_credit_line, pct_card_over_50_uti, ind_XYZ, rep_income, 
# rep_education, Def_ind

## ---------------------------------------------------------------------------- ##

# QUESTION 1c:

# type of data
str(data)

# When using the str() function, it tells me that there is mainly numeric data 
# like ‘credit_age’ and ‘tot_balance’, and just one of the columns is categorical, 
# ‘rep_education.’

## ---------------------------------------------------------------------------- ##

# QUESTION 1d:

# Check for missing values in each column
missing_values = colSums(is.na(data))
# Columns with missing values
columns_with_missing = names(missing_values[missing_values > 0])
columns_with_missing
# The percentage of missing values in each column
percentage_missing = missing_values / nrow(data) * 100
percentage_missing

# The columns missing values are: ‘pct_card_over_50_uti,’ and ‘rep_income.’
# 9.79 percent of ‘pct_card_over_50_uti’ is missing and 7.795 percent of ‘rep_income’ is missing.

## ---------------------------------------------------------------------------- ##

# QUESTION 1e:

# How do you think we should deal with missing values? 
#     i.	You can drop all the rows with missing values. 
#     ii.	You can add in predicted values into the missing spots.
#     iii.	You can note where there is missing values in the data to account for the blank space. 
#           In a way, work around them.
#     iv.	You can put in information from similar records/sources

## ---------------------------------------------------------------------------- ##

# QUESTION 1f:

# With this data, would you fit a supervised or an unsupervised learning model? Why? 
#       i. Supervised learning because we have a label on all the data and a goal to determine 
#           whether someone is worthy of approving or declining of credit.

## ---------------------------------------------------------------------------- ##

# QUESTION 1g:

# Data with no missing values
ComData = na.omit(data)
ComData
#Dimensions of the complete data (ComData): 
dim_ComData = dim(ComData)
dim_ComData

# The dimensions are 16653 rows and 21 columns


## ---------------------------------------------------------------------------- ##

# DATA SUMMARY STATISTICS

## ---------------------------------------------------------------------------- ##

# QUESTION 2a:

# Summary of the data set
summary(ComData)

## ---------------------------------------------------------------------------- ##

# QUESTION 2b:

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

#Based on the mean, mode, and median, is “num_card_inq_24_month” bell shaped, left, right skewed? 
#How about “tot_amount_currently_past_due”? “credit_age”? 
#       i. For num_card_inq_24_month: 
#                 The mode is 0, the median is 0 and the mean is 1.053.
#                 Based on this info, since the mean is larger than the mode and median, 
#                     I can assume that this graph will be right skewed.
#       ii.	For tot_amount_currently_past_due:
#                 The mode is 0, the median is 0, and the mean is 352.5.
#                 Based on this info, since the mean is larger than the mode and the median, 
#                     I can assume this is right skewed.
#       iii.	For credit_age:
#                 The mode is 295, the median is 280, and the mean is 280.7.
#                 Based on this info, because the mean, median, and mode are so close, 
#                     I can assume credit_age is bell shaped.

## ---------------------------------------------------------------------------- ##

# QUESTION 2c:

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

#Do the shapes of the histograms confirm the skewness you found in b?
#     Yes, the shapes of the histogram match the skewness I found from part b.

## ---------------------------------------------------------------------------- ##

# QUESTION 2d:

#How would your convert the “rep_education” column into numerical data? Name two ways. 

#i.	You could assign each variable to a number. I believe there are 3 variables: 
#       high_school, graduate, and college. You could make high_school = 1, graduate = 2, and college = 3. 
#       By using the unclass() function, you could do this.
#ii. You could make different columns for each option, so for this case: 
#       high_school, college, and  graduate. And for each one, if the row indicates one 
#       of these it would be filled in with a ‘1’ and the other two would be ‘0.’

## ---------------------------------------------------------------------------- ##

# DATA VISUALIZATION

## ---------------------------------------------------------------------------- ##

# QUESTION 3e:

# Bar Graph for 'Def_Ind'
Bar1 = ggplot(ComData, aes(x = Def_ind)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Graph of Def_Ind", x = "Def_Ind", y = "Count")
Bar1

# Description:
#i.	The def ind column of this data set is the indicator of default which means whether 
#     someone has no payments within the last three months. It is a binary column where if 
#     the value is 1, the account defaulted after an account was approved and opened with the 
#     bank in the past 18 months. If the value is 0, then there is not default. So in the graph, 
#     there are two bars where one represents when the value is defaulted at 1 and the other is 
#     at 0 for active accounts.

## ---------------------------------------------------------------------------- ##

# QUESTION 3f:

# Bar graph for 'rep_education'
Bar2 = ggplot(ComData, aes(x = rep_education)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Bar Graph of rep_education", x = "rep_education", y = "Count")
Bar2

# Description:
# i. The rep education column has four options: high school, college, graduate, and other. 
#       The represents the unverified self-reported education level of the applicant. 
#       When looking at the graph, many are under college, the second highest is high school, 
#       then graduate, then other.

## ---------------------------------------------------------------------------- ##

# QUESTION 3g:

# Histogram of 'rep_income'
Plot4 = ggplot(ComData, aes(x = rep_income)) +
  geom_histogram(binwidth = 10000, fill = "salmon", color = "black", alpha = 0.7) +
  labs(title = "Histogram of rep_income", x = "rep_income", y = "Frequency")
Plot4

## ---------------------------------------------------------------------------- ##

# QUESTION 3h:

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

#Are there any outliers for this variable? 
# i. Yes, there are many outliers. When you run the ‘outliers’ variable in the bottom of my script, 
#       you can see all the outliers. Some clear outliers include: 
#       200,000, 194485.99, 0, and 37431.07.

## ---------------------------------------------------------------------------- ##