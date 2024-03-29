##################################################################
# The purpose of this R script is to demonstrate a T-test 
# statistical analysis. This was made mostly for my own reference and a way to 
# practice these functions so it is far from perfect or comprehensive,  
# but I will share it for others who may find it helpful.
#
# Script by: Kim Fake
#
#
###################################################################

# load packages
library(dplyr) # pretty much always load for filtering and manipulating data
library(Rmisc) # helpful function (summarySE) for summarizing data

# Load made up data --------------------------------------------------------

#load data
data <- read.csv (
  './Biology 101 Data CLEAN.csv', 
  stringsAsFactors = FALSE, 
  fileEncoding = 'UTF-8-BOM'
)

# as you can see this data is on 2 biology classes
# we want to use a t-test to determine if the grades (%) 
# significantly differ between the classes

# subset data by class
class_1 <- data %>%
  filter(Class == 1)
class_2 <- data %>%
  filter(Class == 2)

# helpful summary of data
Summary <- summarySE(data, measurevar = "Percent_Grade", groupvars = "Class")
Summary

# Shapiro-Wilk Test for normality
# t-test assumes data of each group is normally distributed
# p value must be > 0.05 to pass
shapiro.test(class_1$Percent_Grade)
shapiro.test(class_2$Percent_Grade)


# bartlett test for equal variance
# t-test assumes equal variances of groups
# p value must be > 0.05 to pass
bartlett.test(data$Percent_Grade, data$Class)

# t-test
# note that paired is set to false 
# because these are two separate independent classes
# but if we wanted to compare test grade of one class (i.e. the same students)
# before and after a treatment (perhaps before and after studying)
# this would be a a paired t-test and we would set paired = TRUE
t.test(class_1$Percent_Grade, class_2$Percent_Grade, paired=FALSE)  
# p<0.05 would indicate a significant difference
# thus we cannot reject the null hypothesis
# Null Hypothesis: there is no difference in mean percent grade between the two classes




