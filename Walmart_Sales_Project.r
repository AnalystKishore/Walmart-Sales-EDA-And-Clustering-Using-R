#getting work directory
rm(list= ls())

#uploading data
proj_data <- read.csv("~/Downloads/WALMART_SALES_DATA.csv")
head(proj_data)

#Calling libraries

library("lubridate")
library(car) #Companion to Applied Regression for Regression Visualisations
require(stats)
library(corrplot)
library(caTools)
library(MLmetrics)
library("repr")
library("dplyr") #Calling dplyr function for data manipulation 
library("ggplot2") # for data visualisation
library("scales") #for change of scales in data visualisation
library("zoo")
library("tidyverse")
library("tidyr")

#UNDERSTANDING DATA AND CLEANING DATA
head(proj_data)
# dimensions
dim(proj_data)
# class
class(proj_data)
#structure
str(proj_data)
#summary
summary(proj_data)
#Checking for any NA values in the dataset
colSums(is.na(proj_data)) 
#Checking for Duplicate Values in the dataset
all(duplicated(proj_data) == TRUE)


#FIND TOTAL SALES OF ALL STORES AND FINDING THE STORE WITH HIGHEST SALES.
Store_max_sale = aggregate(Weekly_Sales ~ Store, data = proj_data, sum)
Store_max_sale
colnames(Store_max_sale) <-c("Store Number", "Total Sales of each Store") #Changing column names
Store_max_sale
Store_max_sale <-arrange(Store_max_sale, desc(Store_max_sale$`Total Sales of each Store`))#Arranged Stores based on Sales in descending order
Store_max_sale
Store_max_sale[1,] 
#Interpretation: Store 20 has the highest sales with sale of 301397792.46



# Without changing the order of the graph we convert store column into factor
Store_max_sale$`Store Number` <- as.character(Store_max_sale$`Store Number`)
Store_max_sale$`Store Number` <- factor(Store_max_sale$`Store Number`, levels=unique(Store_max_sale$`Store Number`))



#We Plot Store Vs the Total Sales Which is Done

options(repr.plot.width = 15, repr.plot.height = 8)

sales_graph<-ggplot(Store_max_sale, aes(Store_max_sale$`Store Number`, Store_max_sale$`Total Sales of each Store` )) + geom_bar(stat="identity",fill="light pink") +
  scale_x_discrete(breaks = proj_data$Store)+
  scale_y_continuous(labels=label_number(suffix = " M", scale = 1e-6))+
  labs( x = "Store number",y = "Total sales of each store",title = " STORE VS SALES")+theme_bw()
sales_graph
#Inference:
#Store number 20 has the highest sale (301397792.46) 
#Store number 33 has the least sales (37160222)



# We find out the store with the maximum standard deviation i.e the sales in this particular store vary a lot 
Sales_fluctuation<-summarise(group_by(proj_data,Store),sd(Weekly_Sales), mean(Weekly_Sales));Sales_fluctuation

#Changing column names
colnames(Sales_fluctuation) <-c("Store Number", "Standard deviation of weekly sales","Mean of weekly sales")
Sales_fluctuation


#Finding out the row with highest standard deviation 
Sales_fluctuation[which.max(Sales_fluctuation$`Standard deviation of weekly sales`), ]
#Store number 14 has the highest fluctuation in sales

# Store with highest fluctuation ##

#Finding out the row with highest standard deviation 
Sales_fluctuation[which.max(Sales_fluctuation$`Standard deviation of weekly sales`), ]



#Density Plot for Store 14
#Density Plot for Store 14 weekly sales fluctuation
Store_14 <- proj_data[proj_data$Store == 14, ]
store14_sales <- ggplot(Store_14, aes(x=Weekly_Sales)) + geom_density(color="red",fill="lightpink",alpha=0.2)+
  scale_x_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  labs( x = "Weekly Sales",y = "Density",title = " STORE 14 WEEKLY SALES FLUCTUATION")+theme_bw()

store14_sales

#Store 14 weekly sales fluctuate the most; the fluctuation is high in earlier weeks

projdata2<-proj_data

#WE WILL CREATE A mm_yy COLUMN
projdata2$m_Year = substring(projdata2$Date, 4, 10)
projdata2$m_Year

#Quartile 1-data ( 01-2012,02-2012,03-2012), Quartile 2-data (i.e, 04-2012,05- 2012,06-2012),Quartile 3-data (i.e, 07-2012,08- 2012,09-2012)
quartile1<- filter(projdata2,m_Year == "01-2012" | m_Year== "02-2012" | m_Year== "03-2012")
quartile2<- filter(projdata2,m_Year == "04-2012" | m_Year== "05-2012" | m_Year== "06-2012")
quartile3 <- filter(projdata2,m_Year == "07-2012" | m_Year== "08-2012" | m_Year== "09-2012")

#Aggregating sales for stores for 3rd quartile
quartile3_weeklysales<-summarise(group_by(quartile3,Store),sum(Weekly_Sales))
colnames(quartile3_weeklysales)[2] <- "3rd quartiles sales by store"

#Aggregating sales by store for Q3-2012 
quartile3_weeklysales<-summarise(group_by(quartile3,Store),sum(Weekly_Sales))

#Changing column names
colnames(quartile3_weeklysales)[2] <- "quartile3_weeklysales_by_Store"

#Aggregating sales by store each Q2-2012 
quartile2_Sales<-summarise(group_by(quartile2,Store),sum(Weekly_Sales))

#Changing column names
colnames(quartile2_Sales)[2] <- "quartile2_Sales_by_Store"

#merging two quarters data by store
quartile3_growth <- merge ( quartile2_Sales , quartile3_weeklysales , by = 'Store')

#Creating Growth rate column for Sales by Store in the above dataframe 
quartile3_growth <- mutate(quartile3_growth, Growth_Rate = ((quartile3_weeklysales_by_Store - quartile2_Sales_by_Store)*100) / quartile2_Sales_by_Store)

#Creating only positive growth rates
pos_growth <- filter(quartile3_growth, Growth_Rate > 0 ) 
pos_growth<-arrange(pos_growth, desc(Growth_Rate)) 
View(pos_growth)
pos_stores<- pos_growth$Store

#store 7 has highest positive growth rate
#Positive growth is seen in stores7 16 35 26 39 41 44 24 40 23


options(repr.plot.width = 14, repr.plot.height = 8)

# We are doing a representation(visual of growth rate)
growth_plot<-ggplot(data=quartile3_growth, aes(x=Store, y=Growth_Rate)) +geom_bar(stat ="identity",fill="darkgreen")+
  scale_x_continuous("Stores", labels = as.character(quartile3_growth$Store), breaks =
                       quartile3_growth$Store)+ labs( x = "Stores",y = "Growth Rate",title = " STORE 14 WEEKLY SALES FLUCTUATION")+theme_bw()
growth_plot

#Store 14 has highest negative growth
#Store 7 has highest positive growth 



## We find Holidays which have higher sales than the mean sales in non-holiday season for every store together as they might have a negative impact on sales
#CREATE Holidays Data for Occasions dataframe
Holidays_Dates <- c("31-12-2010", "30-12-2011", "28-12-2012", "27-12-2013","10-09-2010", "09-09-2011", "07-09-2012", "06-09-2013","26-11-2010", "25-11-2011", "23-11-2012", "29- 11-2013","12-02-2010", "11-02-2011", "10-02-2012", "08-02-2013")
Occasion <-c(rep("Christmas", 4),rep("Labour Day", 4),rep("Thanksgiving", 4),rep("Super Bowl", 4))
Holidays_Data_Sales <- data.frame(Occasion,Holidays_Dates)

#MERGING DATAFRAMES
proj_data3<-merge(proj_data,Holidays_Data_Sales, by.x= "Date", by.y="Holidays_Dates", all.x = TRUE)

#NULL Values in Event are replaced with No_Holiday

proj_data3$Occasion[is.na(proj_data3$Occasion)] <- "No_Holiday"

## Printing to check if the changes are shown

head(proj_data3)

# We create a new data frame for Mean Sales with respect to Occasion
Sales_Holiday<-aggregate(Weekly_Sales ~ Occasion, data = proj_data3, mean)
#Changing column names
colnames(Sales_Holiday)[2] <- "Mean_Sales_By_Ocaasion"
View(Sales_Holiday)

## From our analysis, we conclude that the occasions Christmas and Labour Day have a negative impact on sales
## Also, the occasions SuperBowl and Thanksgiving have a positive impact on the sales

# Checking negative impact based on holiday date and non- holiday date
# Filtering holiday dates and finding mean of Weekly Sales 

# Create a new column with holiday status
proj_data3 <- proj_data3 %>%
  mutate(Holiday_Flag = ifelse(Occasion == "No_Holiday", 0, 1))

# Calculate mean sales for holidays and non-holidays
Holiday_Date_Sales <- proj_data3 %>%
  group_by(Date, Holiday_Flag) %>%
  summarise(Mean_Sales = mean(Weekly_Sales)) %>%
  filter(Holiday_Flag == 1) %>%
  mutate(higher_than_non_holiday = Mean_Sales > mean(proj_data3$Weekly_Sales[proj_data3$Holiday_Flag == 0]))

View(Holiday_Date_Sales)

non_holiday_sales <- mean(filter(proj_data3,Holiday_Flag ==0)$Weekly_Sales) 

## There were majority days when the weekly sales for Holidays were greater than the mean for non holiday sales.
## We have 6 cases where the Weekly sales during holidays was greater 
##than the weekly non holiday sales.

weekly_aggregate_sales <- aggregate(Weekly_Sales~Date, data=proj_data,mean)
weekly_aggregate_sales$Date <-as.Date(weekly_aggregate_sales$Date, "%d-%m-%Y")
weekly_aggregate_sales <-arrange(weekly_aggregate_sales,Date)
weekly_aggregate_sales$Date <-factor(weekly_aggregate_sales$Date)


options(repr.plot.width = 13, repr.plot.height = 7)

# plotting weekly mean sales
Weekly_meansales <- ggplot(data=weekly_aggregate_sales, aes(x=Date, y=Weekly_Sales, group=1))+
  geom_line(color="black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust=0.9))+
  scale_x_discrete(breaks = levels(weekly_aggregate_sales$Date)[c(T, rep(F, 9))])+
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6))+
  labs(x= "Week numbers", y = "sales per week", tilte = "plot of weekly sales" )

Weekly_meansales
#Plotting Sales on Labour day
Weekly_meansales + labs(title = 'SALES ON LABOUR DAY')+
  geom_point(aes(x = factor("2010-09-10"), y = 1014097.7), color = "red", size = 2) +
  geom_point(aes(x = factor("2011-09-09"), y = 1039182.8), color = "red", size = 2) +
  geom_point(aes(x = factor("2012-09-07"), y = 	1074001.3), color = "red", size = 2) +
  theme_bw()+
  geom_hline(aes(yintercept = non_holiday_sales), linetype=2)

#Plotting Sales for Superbowl
Weekly_meansales + labs(title= 'SALES DURING SUPER BOWL')+
  geom_point(aes(x = factor("2010-02-12"), y = 	1074148.4), color = "gold", size = 2) +
  geom_point(aes(x = factor("2011-02-11"), y = 1051915.4), color = "gold", size = 2) +
  geom_point(aes(x = factor("2012-02-10"), y = 1111320.2), color = "gold", size = 2) +
  theme_bw()+
  geom_hline(aes(yintercept = non_holiday_sales), linetype=2)

#Plotting Sales during Christmas
Weekly_meansales + labs(title = 'SALES DURING CHRISTMAS') +
  geom_point(aes(x = factor("2010-12-31"), y = 898500.4), color = "green", size = 2)+
  geom_point(aes(x = factor("2011-12-30"), y = 1023165.8), color = "green", size = 2)+
  theme_bw()+
  geom_hline(aes(yintercept = non_holiday_sales), linetype=2)

#Plotting Sales during Thanks Giving
Weekly_meansales + labs(title = 'SALES DURING THANKS GIVING')+
  geom_point(aes(x = factor("2010-11-26"), y = 	1462689.0), color = "purple", size = 2) +
  geom_point(aes(x = factor("2011-11-25"), y = 1479857.9), color = "purple", size = 2) +
  theme_bw()+
  geom_hline(aes(yintercept = non_holiday_sales), linetype=2)

#Though stores on Super Bowl, Labour day have higher sales they are very close to mean
#Thanks giving does create a high positive impact on sales than others
#Christmas Holiday Flag has lower sales than mean sales of a Non Holiday.
#Both the dates "30- December-2011", "31-December-2010" related to Christmas 
#has  a negative impact on sales.
#However, from the graph we can clearly visualize that the week just before the 
#Christmas bagged highest sales. It may be because customers did shopping 
#beforehand for preparation/ Advent is popularly celebrated there

#Converting the date column  into a factor

date_factor<-as.factor(projdata2$Date)

#HERE WE DEFINE THE FORMAT OF THE DATE

d_format<-strptime(date_factor,format="%d-%m-%Y") 

#defining what is the desired format of your date

projdata2$mm_yy<-as.Date(d_format,format="%Y-%m-%d")
projdata2$mm_yy = as.yearmon(projdata2$mm_yy)


## We Find the Sum of 'Weekly Sales' by arranging data in 'Month-Year' Format and we convert it into a df
Sales_mm_yy<-summarise(group_by(projdata2,mm_yy),sum(Weekly_Sales))
colnames(Sales_mm_yy)[2] <- "Sales_by_Month"
Sales_mm_yy<- as.data.frame(Sales_mm_yy)


#Without changing the order we convert Year-Month to a factor
Sales_mm_yy$mm_yy<- as.character(Sales_mm_yy$mm_yy)
Sales_mm_yy$mm_yy<- factor(Sales_mm_yy$mm_yy,
                                   levels=Sales_mm_yy$mm_yy)

#plotting line graph as the monthly sales is a time series data
time_series_plot <- ggplot(data=Sales_Month_Year, aes(x=Sales_Month_Year$Month_Year, y=Sales_by_Month,group=1)) +
  geom_line(color="red")+
  geom_point()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))+
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6))+
  xlab("Months for 2010 to 2012") + ylab("Sales per Month")+
  ggtitle('Monthly Sales for 2010 to 2012')+
  theme(plot.title = element_text(hjust = 0.5))


time_series_plot


#sales vs particular semester - using lubridate library
#converting to date format
projdata2$Date <- dmy(projdata2$Date)


#creating semester column with year

projdata2$sem <- semester(projdata2$Date, with_year=TRUE)

#creating a dataframe 'sales' which has total sales for every semester

sales <- aggregate(Weekly_Sales~sem,data=projdata2, sum)

# Addding a new column by Rewriting semester and yr to different format

sales$semester_year <- paste('Sales in',substr(sales$sem,1,4),substr(sales$sem,6,6),
                        sep = '-')

#Plotting the graph semester vs Sales

semester_sales <- ggplot(data=sales, aes(x=semester_year, y=Weekly_Sales, group=2)) +
  geom_line(color="purple")+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust=1))+
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6))+
  ggtitle('Semester Sales - 2010 to 2012')+
  theme(plot.title = element_text(hjust = 0.9))+
  xlab("Semester") + ylab("Total number of sales done in a semester")

semester_sales


#Insights:
#The sales were highest in the month of December and it 
#was Lowest in the month of  January
#The sales were higher in second semester of every year
#The visualization shows a drop in S2-2012 & S1-2010. 
#It is due to absence of January data in S1-2010 & Nov-Dec 2012 data in S2-2012.

#Clustering starts

# Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(factoextra)

# Load the Walmart sales dataset
walmart_sales <- read.csv("WALMART_SALES_DATA.csv", header = TRUE)
View(walmart_sales)

# Now let's explore the dataset to understand its features:

# Examine the structure of the dataset
str(walmart_sales)


# Convert Date column to date format
#walmart_sales$Date <- as.Date(walmart_sales$Date)
#walmart_sales$DateFormated = as.Date(walmart_sales$Date)
View(walmart_sales)
# Remove missing values
walmart_sales <- na.omit(walmart_sales)
str(walmart_sales)

# Select variables for clustering
walmart_cluster <- select(walmart_sales, Store, Weekly_Sales, Temperature, Fuel_Price, CPI, Unemployment)

# Scale the variables
walmart_cluster_scaled <- scale(walmart_cluster[, 2:6])
walmart_cluster_scaled
# Perform k-means clustering
walmart_kmeans <- kmeans(walmart_cluster_scaled, centers = 5, nstart = 25)
str(walmart_cluster)
# Print the cluster centers
walmart_kmeans$centers
walmart_kmeans$cluster


# Add cluster labels
walmart_sales$Cluster <- as.factor(walmart_kmeans$cluster)
walmart_sales
#colnames(walmart_cluster_scaled ) <- c("Weekly_Sales_Scaled",  "Temperature_Scaled",   "Fuel_Price_Scaled", "CPI_Scaled", "Unemployment_Scaled")
#cbind(walmart_sales,walmart_cluster_scaled)


ns <-  ggplot(walmart_sales, aes(x = Cluster, y = Weekly_Sales, color = Cluster)) +
  geom_boxplot()  +
  labs(title = "Walmart Sales per Clustering Results",
       x = "Cluster",
       y = "Sales",
       color = "Cluster") +
  theme_minimal() 
ns


aggdf <- aggregate(cbind(Weekly_Sales,Fuel_Price,Temperature ,CPI) ~ Cluster, data=walmart_sales, mean )
aggdf

ggplot(aggdf, aes(Cluster, Weekly_Sales)) + 
  geom_text(aes(label=Cluster, color=Cluster),hjust=0, vjust=0)