#-----------------------------------------------------------------------------------------------------------------
# UBER CASE STUDY
  
#Part 1: Data cleaning and preparation 

#Identify the data quality issues and clean the data so that you can use it for analysis.
#Ensure that the dates and time are in the proper format. Derive new variables which will be useful for analysis.


#-----------------------------------------------------------------------------------------------------------------

# Calling the packages needed for this assignment
  
library(dplyr)
library(tidyr)  
library(stringr)
library (lubridate)
library(ggplot2)
library (scales)

#Importing the Uber dataset

uber <- read.csv("Uber Request Data.csv")


#Step 1: Checking for duplicate rows and deduplicating incase duplicates are found

sum(duplicated(uber$Request.id)) # There are no duplicates

#Step 2: Check for missing values

sapply(uber,function(x) sum(is.na(x))) 
sum(is.na(uber)) # 6564 NA's

#There are total of 6564 NA values, Column Driver ID has 2650 NA's and Drop.timestamp has 3914 missing values


#To find the blank values

sapply(uber,function(x) length(which(x==""))) #There are none


#Step 3: Check Individual columns

summary(uber)

#Request.id: This is of type integer, converting to factor

uber$Request.id <- factor(uber$Request.id)
str(uber$Request.id)


#Pickup point
summary(uber$Pickup.point) #No issues

#Driver.id
#Driver.id: This is of type integer, converting to factor

uber$Driver.id<- factor(uber$Driver.id)
str(uber$Driver.id)


#Status
summary(uber$Status)
  
#Fixing and standardising the timestamp formats:

#1. standardize the format to be separated by "-"

uber$Request.timestamp <- str_replace_all(uber$Request.timestamp,"/","-")

uber$Request.timestamp <- dmy_hms(uber$Request.timestamp,truncated = 2)

uber$Drop.timestamp <- str_replace_all(uber$Drop.timestamp,"/","-")

uber$Drop.timestamp <- dmy_hms(uber$Drop.timestamp,truncated = 2)


summary(uber)
#Data looks good, lets jump into the analysis!!

#-----------------------------------------------------------------------------------------------------------------


#Performing Univariate analysis to get different metrics to get meaningful insights


#----------------------------------------------------------------------------------------
#For Pickup point
#-----------------------------------------------------------------------------------------
#Understanding the data

# Appending the data driven derived metrics for the commute time , (approx)commute time= Request time- Drop time

uber$commute_time_min <- abs(with(uber, difftime(Request.timestamp,Drop.timestamp,units="mins")))
uber$commute_time_min <- as.integer(uber$commute_time_min)

pickup_points_commutetime <- uber %>% group_by(Pickup.point) %>% summarise(mean(commute_time_min,na.rm=T))

summary(uber)  # Median commute time is 52 minutes
#The mean commuting times To and from Airport is around 52 minutes

summary(uber$Pickup.point)
pickup_point <- ggplot(uber,aes(x=Pickup.point)) #assigning ggplot to an object
pickup_point+geom_bar(fill="blue",width=0.2)  # Shows that pickups from City is greater than from airports

#Want to see the summary of status by pickup points
pickup_point_summary <- uber %>% group_by(Pickup.point,Status) %>% summarise(counts=n()) 
pickup_point_summary

#Extending the plot to see the distribution of Status(Cancelled/ No carsAvailable/Completed)
ggplot(uber,aes(x=Pickup.point,fill=Status))+geom_bar(width=0.2,position="dodge")

#Choosing bar plot to show the status of requests by pickup points and displaying in a dodge position or side by side
#The visualisation shows Airport pick up show high non availability of cars and City pick up shows high cancellations 
# The trips completed from City is higher than Trips completed from Airport

  #Finding the summary of average trips made by each driver

average_driver_trip <- uber %>% filter(Status=="Trip Completed") %>% group_by(Driver.id) %>% summarise(no_of_trips=n())

summary(average_driver_trip) #Average number of trips made by a driver is 9 trips 


#--------------------------------------------------------------------------------------------------------------------
#Time series plotting
#---------------------------------------------------------------------------------------------------------------------

#My next problem is to Breakdown the time intervals into bins:
#a) For this i want to understand the pattern of requests for entire day(24hours) and compare it for each days

#Creating a subset dataframe , i.e grouping by pickup points , by date,by hour and summarising by count of requests

demand_by_date <- uber %>%  group_by(Pickup.point,date=as.Date(Request.timestamp),hour=hour(Request.timestamp)) %>% summarise(num_of_req=n())

#Plotting by each date facet

ggplot(demand_by_date,aes(x=hour,y=num_of_req,col=Pickup.point))+geom_smooth()+facet_wrap(~date,ncol=1)
# Choosing the smooth as geom to draw a trend line along the pattern of requests and viewing it by facet of different dates

#Inference : As the pattern for the cab request is comparable across all the weekdays, we can now aggregate all the
#at an hour level 


#------------------------------------------------------------------------------------------------------------------------

#Now i want to see the request patterns at an hourly basis across all the dates
#Creating another dataframe aggregating the dates and grouping by pickup points and hour 

demand_by_hour <- uber %>% group_by(Pickup.point,hour=hour(Request.timestamp)) %>% summarise(num_of_req=n())
ggplot(demand_by_hour,aes(x=hour,y=num_of_req,fill=Pickup.point))+geom_bar(stat="identity",position="dodge")+scale_x_continuous(breaks = seq(0,23,1))

#Choosing the barplot as the geom and stat as identity as the the data on y axis is the number of requests and hours on the x axis
#Scaling for clear display of plot




#The above plot shows , we can actually split the 24 hours into Bins as each interval in the plot have 
# similar pattern of requests for both Airport and City

#-------------------------------------------------------------------------------------------------------------------------

#Binning the request time into 5 time slots
# Will name this time slots as Pre_morning ,Morning_peak,Day_time,Evening_peak,Late_night
#1.Pre_morning : 0 to 4 H
#2. Morning_rush : 5 to 9 H
#3. Day_time : 10 to 16 H
#4. Evening_rush : 17 to 21 H
#5. Late_evening : 22: 23H

uber1 <- uber #duplicating the main dataframe
uber1$hour <- hour(uber1$Request.timestamp)  #Extracting the hour from timestamp


#Binning (getting a derived metric by name hour_bin)

uber1$hour_bin <- ifelse(uber1$hour %>% between(0,4),"Pre_morning",ifelse(uber1$hour %>% between(5,9),"Morning_rush",ifelse(uber1$hour%>% between(10,16),"Day_time",ifelse(uber1$hour %>% between(17,21),"Evening_rush",ifelse(uber1$hour %>% between(22,23),"Late_evening","None")))))

uber1$hour_bin <- factor(uber1$hour_bin) # converting from Character to factor type

str(uber1$hour_bin)  # Factor w/ 5 levels "Day_time","Evening_rush",..: 1 2 4 2 4 2 4 4 2 4 ...       

ggplot(uber1,aes(x=hour_bin ))+geom_bar(width=0.4,fill="blue")+labs(title="Number of requests in different intervals",x ="request intervals", y = "Number of requests")

#Plotting different time intervals on x axis of bar plot to view the count of requests  on y axis

#There is a spike in the number of requests for the Evening rush and Morning rush intervals : Will break this down further



#-------------------------------------------------------------------------------------------------------------------------
#Performing segmented Univariate analysis on Pickup.point to understand the distribution of requests for different hours bin

ggplot(uber1,aes(x =reorder(hour_bin,hour_bin,function(x)-length(x)),fill=Pickup.point)) +geom_bar(stat='count',position="dodge",width=0.4)+labs(title="Number of requests in different intervals",x ="request intervals", y = "Number of requests")+geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.5),vjust=-0.5)

#Using the reorder function to display bars in descending order of requests,adding a layer of geom_text to add labels

# Morning rush is between 5AM to 9AM , Evening rush is between 5PM to 9PM
# The number of airport requests are higher in Evening and City requests are higher in Morning

#-----------------------------------------------------------------------------------------------------------------------------

#Performing bivariate analysis by using another categorical variable "Status" and understand the status booking
# requests in each of the interval Bins

uber2 <- uber1 %>% group_by(Pickup.point,Status,hour_bin) %>% summarise(num_of_requests=n())
uber2$num_of_requests_as_percent <- percent(uber2$num_of_requests/sum(uber2$num_of_requests))
uber2$num_of_requests_as_percent <-as.numeric(sub("%", "", uber2$num_of_requests_as_percent))/100

ggplot(uber2,aes(x=hour_bin,y=num_of_requests_as_percent,fill=Status))+geom_bar(stat="identity")+facet_wrap(~Pickup.point,ncol=1)+scale_y_continuous(labels=percent)


# Visualising the relationship between the commute time and the time intervals

ggplot(uber1,aes(x=hour_bin,y=commute_time_min))+geom_boxplot()



#Inference:There are 2 problematic time slots, the Morning rush hour and the Evening rush hour 

#1. Requests from Airport as pickup point has highest instance of non availability of cars during evening
# rush (between 5PM to 9pm)  (Supply and demand gap at the Airport due to Cars not available)

#2. Requests from City as a pickup point has the highest instance of Cancellations during 
#Morning rush (Between 5AM to 9AM) (Supply and demand gap at the city due to drivers cancelling the requests)

#3. It appears the median commute time is the same across the intervals, and implies traffic no bearing on the issue

########################################## DEMAND SUPPLY GAP ANALYSIS##################################################################



#-----------------------------------------------------------------------------------------------------------------------------
#THE DEMAND SUPPLY GAP:I go by the below definitions:


#Demand and supply gap at the City:
# For this case, the demand is the number of trip requests made at the city, whereas the supply is the number of trips completed from city to the airport.

#Demand and supply gap at the Airport:
# For this case, the demand is the number of trip requests made at the airport, whereas the supply is the number of trips completed from airport to the city.

#---------------------------------------------------------------------------------------------------------------------------------------------


#Function to generate the 2 dataframes to summarise the demand supply gap at Airport and City

demand_supply_function <- function(x){
  
  uber3a <- uber1 %>% filter(Pickup.point==x) %>% group_by(hour_bin) %>% summarise(total_requests=n())
  
  #Subsetting the dataframe  to get the requests those are "Completed"
  
  uber3b <- uber1 %>% filter(Pickup.point ==x, Status=="Trip Completed") %>% group_by(hour_bin) %>% summarise(completed_Requests=n())
  
  #Joining the above 2 dataframes
  
  x_demandsupply_gap <- inner_join(uber3a,uber3b,by="hour_bin")
  x_demandsupply_gap$demandsupply_gap <- x_demandsupply_gap$total_requests - x_demandsupply_gap$completed_Requests #(Demand supply gap = Total requests - Completed requests)
  
  return(x_demandsupply_gap)
  
}


city_demandsupply_gap <- demand_supply_function("City")  #Dataframe summarizing the gap analysis for city
airport_demandsupply_gap <- demand_supply_function("Airport") #Dataframe summarizing the gap analysis for Aiport

#------------------------------------------------------------------------------------------------

#Visualising the supply gap at City ( from City--> to Airport)

ggplot()+geom_bar(data=city_demandsupply_gap,aes(x=hour_bin,y=total_requests),fill="blue",width=0.7,stat="identity")+geom_bar(data=city_demandsupply_gap,aes(x=hour_bin,y=completed_Requests),fill="red",width=0.5,stat="identity",alpha=0.4)+geom_bar(data=city_demandsupply_gap,aes(x=hour_bin,y=-(demandsupply_gap)),fill="red",width=0.6,stat="identity")+scale_y_continuous(breaks=seq(0,-1800,-200))+labs(title="Demand supply gap for city where blue(total req),purple(completed requests),red bar on the negative Y axis shows the supply gap",x ="request periods", y = "number of requests")


#Inference : Morning Rush hour has the highest gap of 1205 unfulfilled requests, with 1677 total requests 
# out which only 472 requests are completed 


#--------------------------------------------------------------------------------------------------------------------

#Visualising the Demand and Supply gap at the Airport across the time periods

ggplot()+geom_bar(data=airport_demandsupply_gap,aes(x=hour_bin,y=total_requests),fill="blue",width=0.7,stat="identity")+geom_bar(data=airport_demandsupply_gap,aes(x=hour_bin,y=completed_Requests),fill="red",width=0.5,stat="identity",alpha=0.4)+geom_bar(data=airport_demandsupply_gap,aes(x=hour_bin,y=-(demandsupply_gap)),fill="red",width=0.6,stat="identity")+scale_y_continuous(breaks=seq(0,-1800,-200))+labs(title="Demand supply gap for airport where blue(total req),purple(completed requests),red bar on the negative Y axis shows the supply gap",x ="request periods", y = "number of requests")
#Plotting the gap on the negative Y axis , the length of the bar indicates the gap.

#Inference : Evening Rush hour has the highest gap of 1427 unfulfilled requests, with 1800 total requests 
# out which only 373 requests are completed 


#----------------------------------------------------------------------------------------
#Finding what percentage of issues contribute to the demand supply gap

#Function to include columns for the above dataframes to show what is the percentage of 
#issues contributing to the gap

percentage_demand_Supply <- function(x,y){

cancelled_numbers<- uber1 %>% filter(Status %in% "Cancelled",Pickup.point== x) %>% group_by(Status,hour_bin) %>% summarise(cancelled_num=n())

not_available_numbers<- uber1 %>% filter(Status %in% "No Cars Available",Pickup.point== x) %>% group_by(Status,hour_bin) %>% summarise(no_cars_available_num=n())

#Joining with airport_demandsupply_gap dataframe
#appending columns for cancelled number
y <- left_join(y,cancelled_numbers,by="hour_bin")
y <- left_join(y,not_available_numbers,by="hour_bin")

y <- y[,-c(5,7)] #removing Status.x and Status.y columns

y$cancelled_cars_percentage <- round((y$cancelled_num/y$demandsupply_gap)*100,2)
y$notavailable_cars_percentage <- round((y$no_cars_available_num/y$demandsupply_gap)*100,2)

return(y)

}

airport_demandsupply_gap <- percentage_demand_Supply("Airport",airport_demandsupply_gap) #Dataframe for airport data
city_demandsupply_gap    <- percentage_demand_Supply("City",city_demandsupply_gap)       #Dataframe for city data

View(airport_demandsupply_gap)
View(city_demandsupply_gap)
