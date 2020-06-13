#final project code 
#group 3

library(rlang)
library(tidyverse)
library(lubridate) # date and time package
library(ggplot2)
library(brew)
library(dplyr)

data <- read.csv("ab_data1.csv", header=T) #read in data
summary(data) #summary
str(data) #str 
data[1:20,] #look at first 20 rows

ind <- which(is.na(data)) #check if there's any missing values, there are none
length(data$user_id) #total length of df: 294478

#############################################################################
############ removing not alligned data 
#############################################################################

#finding not alligned data (1. treatment & old, 2. control & new)
na_user <- data %>% 
  filter( group == "treatment" & landing_page == "old_page")
na_user1 <- data %>% 
  filter( group == "control" & landing_page == "new_page")
na_user2 <- rbind(na_user,na_user1)
dim(na_user2) # 3893

#we need to remove the not aligned data from the original data 
new_data <- data %>% anti_join(na_user2)

#arrange by order of timestamp
new_data1 <- new_data %>%
  group_by(user_id)%>%
  arrange(timestamp)
#only keep the 1st result if a user clicked multiple times
final_data <- new_data1[!duplicated(new_data1$user_id),]
head(final_data) #look at the head of the new dataframe
dim(final_data) #290584 datapoints

#table of means with the new and old page (EDA)
new <- mean(final_data$converted[final_data$landing_page == "new_page"])
old <- mean(final_data$converted[final_data$landing_page == "old_page"])
mean_table <-cbind(new,old)
rownames(mean_table)<-c("Mean")
colnames(mean_table)<-c("New", "Old")
mean_table
#         New       Old
# Mean 0.1188081 0.1203863

new1 <- sd(final_data$converted[final_data$landing_page == "new_page"])
old1 <- sd(final_data$converted[final_data$landing_page == "old_page"])
sd_table <-cbind(new1,old1)
rownames(sd_table)<-c("SD")
colnames(sd_table)<-c("New", "Old")
sd_table
#       New       Old
# SD 0.3235636 0.3254138
#############################################################################
############## conversion rate 
#############################################################################

#conversion rate overall 
all_converted <- final_data %>%
  filter(converted == 1)
rate_all <- nrow(all_converted)/nrow(final_data)
# 0.1195971

#conversion rate based on the group (control vs treatment)
a1<- final_data %>%
  group_by(group)%>%
  summarize(conversion_rate = mean(converted))

#barchart to show the % converted in each group
ggplot(a1, aes(x = group, y = conversion_rate)) + geom_bar(stat = "identity") + scale_y_continuous(limits = c(0,1))

#new df that only looks at group, landing page and converted
data2 <- cbind.data.frame(group=final_data1$group, landing=final_data1$landing_page, converted=final_data1$converted)

#old and converted subset
old_converted<-nrow(data2 %>% 
                      filter(converted == "1" & landing== "old_page"))

#new and converted subset
new_converted<-nrow(data2 %>% 
                      filter(converted == "1" & landing == "new_page"))

#old and not-converted subset
old_no <- nrow(data2 %>% 
               filter(  converted == "0" & landing== "old_page"))

#new and not-converted subset
new_no <- nrow(data2 %>% 
               filter(  converted == "0" & landing== "new_page"))

#creating old/new page and convert/did not convert 2 way table
data.frame(old_converted, new_converted, old_no, new_no)
did.convert<-rbind(old_converted, new_converted)
no.convert<-rbind(old_no, new_no)
table1<-data.frame(did.convert, no.convert)
rowNames<-c("Old pages","New pages")
colNames<-c("Convert","Noconvert")
rownames(table1)<-rowNames
colnames(table1)<-(colNames)
table1
#            Convert Noconvert
# Old pages   17489    127785
# New pages   17264    128046

#conversion rate table
pagePerformance <- table1 %>%
  mutate(visits = Convert + Noconvert) %>%
  mutate(conRate = round((Convert / visits) * 100, 2))
#    Convert Noconvert visits conRate
#   17489    127785 145274   12.04 (This value (12.04) is the conversion rate control)
#   17264    128046 145310   11.88 (This value (11.88) is the conversion rate treatment)

#non-conversion rate table
BadpagePerformance <- table1 %>%
  mutate(visits = Convert + Noconvert) %>%
  mutate(NoconRate = round((Noconvert / visits) * 100, 2))
#   Convert Noconvert visits NoconRate
#   17489    127785 145274     87.96
#   17264    128046 145310     88.12

#############################################################################
############ statistical analysis 
#############################################################################

#extracting just the day from timestamp
final_data1 <- final_data[order(as.Date(final_data$timestamp,format="%Y-%m-%d")),,drop=FALSE]
final_data1$day<- substring(final_data1$timestamp, 3,4)

#remove the / after certain days
final_data1$day <- as.integer(gsub('/', '', final_data1$day))
sd(final_data1$day)

#group the data by the group as well as the day 
data_sum <- final_data1 %>%
  group_by(day, group)%>%
  summarize(conversion_rate = mean(converted))
summary(final_data1$user_id)

#line plot showing the difference in conversion per group each day
ggplot(data_sum,
       aes(x = day, y = conversion_rate, color = group,group = group)) +
           geom_point() + geom_line()  +
           labs(x = "Day", y = "Conversion Rate") +
           ggtitle("Difference in Conversion Rates Based on Treatment vs Control by Day") + 
           theme(plot.title = element_text(hjust = 0.5))


#chi sq test on conversion rates 
#null: there is no difference in conversion rates b/w old and new page 
#alt: there is a difference in conversion rates b/w old and new page 
chisq.test(table1)

# X-squared = 1.7036, df = 1, p-value = 0.1918
# fail to reject the null hypothesis, there is no difference in conversion rates between the old and new page 
table(final_data1$converted)

###confidence interval of the test

prop.test(t2, conf.level = .95)
##show that true mean of 0 lies between the confidence interval
##no difference between old page and new page conversion rate.

## cleaned data file 
write.csv(final_data1, "clean_data_group3.csv") 


