##Environment Setup
install.packages("tidyverse")
install.packages("lubridate")
library(lubridate)
library(tidyverse)
library(readr)



# Ask phase question 1: How do annual members and casual riders use Cyclistic bikes
#differently?

#What problem am I trying to solve? I'm trying to identify if there are any notable differences between casual users and members.
#How can my insights drive business decisions?

##Business Task: Identify notable differences between members and casual users

# Data Sources: Cyclistic Bike Share data quarter 1 of 2020

#Stakeholders : Cyclistic Executive Team



## Import data set using readr package and store data set in variable
bike_Data1 <- read_csv("/Users/selinnaescabi/Desktop/Cyclistic Bike-Share Dataset/Divvy_Trips_2020_Q1.csv")


#data is organized by individual ride, no identifier for individual riders which will make it impossible to identify outliers that may skew results of analysis.

#Credibility: seeing as this is financial data directly from a company. I would consider this credible data.


# Add column for length of each ride to see if there is a significant difference in type of usage between members and casual riders.

bike_data_with_ride_length <- bike_Data1 %>%
  mutate(time_of_ride = bike_Data1$ended_at - bike_Data1$started_at )

#clean data to remove records where ride time was negative.
corrected_ride_length <- bike_data_with_ride_length %>%
  filter(time_of_ride >= 0)

#set back original variable name for cleaned data
bike_data_with_ride_length <- corrected_ride_length

#add column to data set to identify which day of the week a particular ride was taken on
add_weekday <- bike_data_with_ride_length %>%
  mutate(dayOfRide = wday(started_at, label= TRUE))

bike_data_with_ride_length <- add_weekday

rm(add_weekday)

  

#Plot data to see if there are any differences that make themselves readily apparent in a visualization.
ggplot(data = bike_data_with_ride_length) +
  
  geom_point(mapping = aes(x = time_of_ride, y = member_casual, color = member_casual))

# Based on plot data it appears that casual members rides tend to skew longer. To get a more clear view of this


# Below, I find the median and average ride times and compare them with a bar chart.
# As you can tell, casual riders have significantly larger median and average ride lengths then annual members.


#create data frame that contains observations for median ride length for casuals and members respectively
median_Diffs <- data.frame("Median_Ride_Time" = c(median(casuals_with_ride_length$time_of_ride), 
                                                     median(members_with_ride_length$time_of_ride)),
                              member_casual = c("casual", "member"))

#create data frame that contains observations for average ride length for casuals and members respectively

average_Diffs <- data.frame("Average_Ride_Time" = c(mean(casuals_with_ride_length$time_of_ride), 
                                                   mean(members_with_ride_length$time_of_ride)),
                            member_casual = c("casual", "member"))

#Use barchart to visually display differences in both median and average ride times between casual riders and members.
ggplot(data = median_Diffs) + 
  geom_bar(mapping = aes(x = member_casual, y = Median_Ride_Time), stat = 'identity')

ggplot(data = average_Diffs) + 
  geom_bar(mapping = aes(x = member_casual, y = Average_Ride_Time), stat = 'identity')

#use barchart to visually display different days of use faceted by members and casual riders

ggplot(data = bike_data_with_ride_length) +
  geom_bar(mapping = aes(x = dayOfRide)) +
  facet_wrap(~member_casual)
  

                              
median(casuals_with_ride_length$time_of_ride)
median(members_with_ride_length$time_of_ride)
mean(casuals_with_ride_length$time_of_ride)
mean(members_with_ride_length$time_of_ride)






 
sortedStations <- bike_Data %>%
   distinct(start_station_id) %>%
    arrange(start_station_id)

casual_members <- bike_Data1 %>%
  filter(member_casual == "casual")

casual_stations <- casual_members %>%
  distinct(start_station_id) %>%
  arrange(start_station_id)

members <-bike_Data1 %>%
  filter(member_casual == "member")

member_stations <- members %>%
  distinct(start_station_id) %>%
  arrange(start_station_id)
  
casuals_with_ride_length  <- casual_members %>%
  mutate(time_of_ride = ended_at - started_at)


rm(bike_Data, bike_Data2, bike_Data3, )
    
members_with_ride_length <- members %>%
  mutate(time_of_ride = ended_at - started_at)

ride_time_average_casuals <- mean(casuals_with_ride_length$time_of_ride)
ride_time_average_members <- mean(members_with_ride_length$time_of_ride)

median_ride_time_casuals <-  median(casuals_with_ride_length$time_of_ride)
median_ride_time_members <-  median(members_with_ride_length$time_of_ride)

range_of_ride_times_casuals <- max(casuals_with_ride_length$time_of_ride) - min(casuals_with_ride_length$time_of_ride)
range_of_ride_times_members <- max(members_with_ride_length$time_of_ride) - min(members_with_ride_length$time_of_ride)

ride_time_average_casuals
ride_time_average_members
median_ride_time_casuals
median_ride_time_members
range_of_ride_times_casuals
range_of_ride_times_members

data_diffs <- data.frame("ride_time_average_casuals" = ride_time_average_casuals,
                         "ride_time_average_members" = ride_time_average_members,
      "median_ride_time_casuals" = median_ride_time_casuals, 
      "median_ride_time_members" = median_ride_time_members, 
      "range_of_ride_times_casuals" = range_of_ride_times_casuals,
      "range_of_ride_times_members" = range_of_ride_times_members,
      stringsAsFactors = FALSE
      
      )

neg_times_casual <- casuals_with_ride_length %>%
  filter(time_of_ride < 0)

 

str(bike_Data)
  
ggplot(data = data_diffs) +
  geom_bar(mapping = aes(x = time_of_ride, y = member_casual)) 



