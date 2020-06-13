# A/B Testing
# Group 3

# Load in packages
library(rlang)
library(tidyverse)
library(lubridate) # date and time package
library(ggplot2)
library(brew)
library(dplyr)

### Load in Data
final_data1 <- read.csv("clean_data_group3.csv", stringsAsFactors = FALSE)

# Read in the first few lines to understand how the data is set up 
head(final_data1)

# Understand the structure of the dataframe 
str(final_data1)

######### Creating a table of means 
# First subset the new pages of the ones that converted then subset the old pages from the ones that converted 
new <- mean(final_data1$converted[final_data1$landing_page == "new_page"])
old <- mean(final_data1$converted[final_data1$landing_page == "old_page"])
# Bind the new and old variables and create new row and column names
mean_table <-cbind(new,old)
rownames(mean_table)<-c("Mean")
colnames(mean_table)<-c("New", "Old")

# Print out the table 
mean_table

######### Creating a table of Standard Deviation 
## Use the same process as the table of means but use the sd() function instead of the mean() function. Remember to set new variable names. 
new1 <- sd(final_data1$converted[final_data1$landing_page == "new_page"])
old1 <- sd(final_data1$converted[final_data1$landing_page == "old_page"])
sd_table <-cbind(new1,old1)
rownames(sd_table)<-c("SD")
colnames(sd_table)<-c("New", "Old")

# Print out the table
sd_table

#############################################################################
############## conversion rate 
#############################################################################

# Conversion rate is defined as the total amount of an action performed divided by the total amount of data. For example Conversion Rate = (Number of people who clicked on an advertisement)/(Total number of people who visited the site). First, subset all of the rows in the converted column that equal one since in our dataset, one is equal to converted and zero means not converted; save this as all_converted. Then divide the total rows in our final_data1 to find the conversion rate.

all_converted <- final_data1 %>%
  filter(converted == 1)
rate_all <- nrow(all_converted)/nrow(final_data1)
rate_all

# We can also find the conversion rate based on the different groups. Use group_by() and insert groups (Treatment vs Control). Use the summarize function to find the mean rates per group.

a1<- final_data1 %>%
  group_by(group)%>%
  summarize(conversion_rate = mean(converted))
a1

# Creating a Barchart 
# Using geom_bar, create a bar chart that shows the conversion rates of each group. How do they compare?
ggplot(a1, aes(x = group, y = conversion_rate)) + geom_bar(stat = "identity") + scale_y_continuous(limits = c(0,1))

# Create data2 which only looks at three columns: group, landing page and converted
data2 <- cbind.data.frame(group=final_data1$group, landing=final_data1$landing_page, converted=final_data1$converted)
head(data2)

# Subset the converted and landing columns so that all of the possible combinations are used.
# Old and converted subset
old_converted<-nrow(data2 %>% 
                      filter(converted == "1" & landing== "old_page"))
# New and converted subset
new_converted<-nrow(data2 %>% 
                      filter(converted == "1" & landing == "new_page"))
# Old and not-converted subset
old_no <- nrow(data2 %>% 
                 filter(  converted == "0" & landing== "old_page"))
# New and not-converted subset
new_no <- nrow(data2 %>% 
                 filter(  converted == "0" & landing== "new_page"))

# Create a 2 way table of converted and not converted by first making a dataframe of the four subsets we just created, merging the two converted variables together and the two not converted variables together, and renaming the rows. 

data.frame(old_converted, new_converted, old_no, new_no)
did.convert<-rbind(old_converted, new_converted)
no.convert<-rbind(old_no, new_no)
table1<-data.frame(did.convert, no.convert)
rowNames<-c("Old pages","New pages")
colNames<-c("Convert","Noconvert")
rownames(table1)<-rowNames
colnames(table1)<-(colNames)

# Print out the table
table1

# The previous table only gave us a count of each of the four groups. Use mutate to create visits with has the total as well as conRate which is the conversion rate. 
#conversion rate table
pagePerformance <- table1 %>%
  mutate(visits = Convert + Noconvert) %>%
  mutate(conRate = round((Convert / visits) * 100, 2))
# With this table, 1 = Old page and 0 = New page. The conRate for 1 is also known as the conversion rate control. The conRate for 0 is also known as the conversion rate treatment. 
pagePerformance

# We can also create the same table as above but it the not converted pages. The conRate from the previous table and the NoconRate should total to 100%. 
BadpagePerformance <- table1 %>%
  mutate(visits = Convert + Noconvert) %>%
  mutate(NoconRate = round((Noconvert / visits) * 100, 2))
BadpagePerformance

#############################################################################
############ statistical analysis 
#############################################################################

# In order to create a more in depth graph, group the data by the group (Treatment vs Control) as well as the day. Since our dataframe is all in January, there are no overlaps of days in different months. The Data_sum variable now shows the conversion rate of each day of each group. 
data_sum <- final_data1 %>%
  group_by(day, group)%>%
  summarize(conversion_rate = mean(converted))
data_sum

# Create a visualization 
# Make a line plot showing the difference in conversion rates per group by setting the color = group and group = group. Make sure to change the x and y - axis as well as making a title for the graph. What does this visualization tell us? Look at the y-axis and how stretched out in this. Does this change what you think about the comparison of rates between the two groups?
ggplot(data_sum,
       aes(x = day, y = conversion_rate, color = group, group = group)) +
  geom_point() + geom_line()  +
  labs(x = "Day", y = "Conversion Rate") +
  ggtitle("Difference in Conversion Rates Based on Treatment vs Control by Day") + 
  theme(plot.title = element_text(hjust = 0.5))

# Chi-Square Test
# The assumptions of a chi-square test are that 1. It's a large sample, 2. There's independence of observations. We have a sample size of 290,584 data points with over 30,000 converted users and over 255,000 converted users checking off assumption 1. Each of our collected data points is also independent since it is known that in the cleaning process of the data frame any duplicate user id was removed. Therefore, we can use the chi-square test and now we will state the hypotheses. 
# Our null hypothesis is: There is no difference in conversion rates between the old and new page. 
# Our altternative hypothesis is: There is a difference in conversion rates b/w old and new page.
# Run a chi-square test on the table we created previously in order to see if there is a difference. 
chisq.test(table1)

# Find the confidence interval as well by converting table1 to a matrix and running a 2-sample test for equality of poroportions with continuity correction. This is to see if the true mean of 0 lies within the confidence interval to show that there is no difference between the old and new page.   
t2 <- as.matrix(table1)
prop.test(t2, conf.level = .95)

# Since the p-value is 0.1918 < 0.05 (alpha) we fail to reject the null hypothesis. There is no difference in conversion rates between the old and new page. Based on this result, 0 being inside the confidence interval, and from the visualizations we created that showed little to no change between the groups, we would recommend that the company sticks with the old page since the updated page results in no change in conversion rates. 




