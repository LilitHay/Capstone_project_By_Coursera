#Installation of required packages

install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot")


#Loading packages
library(tidyverse)
library(lubridate)
library(ggplot2)
getwd()
setwd("/cloud/project/Case_study_Cyclistic")


#Uploading Divvy data sets (csv files) of last 12 months (December 2020-November 2021).

m12_2020 <- read_csv("202012-divvy-tripdata.csv")
m01_2021 <- read_csv("202101-divvy-tripdata.csv")
m02_2021 <- read_csv("202102-divvy-tripdata.csv")
m03_2021 <- read_csv("202103-divvy-tripdata.csv")
m04_2021 <- read_csv("202104-divvy-tripdata.csv")
m05_2021 <- read_csv("202105-divvy-tripdata.csv")
m06_2021 <- read_csv("202106-divvy-tripdata.csv")
m07_2021 <- read_csv("202107-divvy-tripdata.csv")
m08_2021 <- read_csv("202108-divvy-tripdata.csv")
m09_2021 <- read_csv("202109-divvy-tripdata.csv")
m10_2021 <- read_csv("202110-divvy-tripdata.csv")
m11_2021 <- read_csv("202111-divvy-tripdata.csv")


#Checking columns names
colnames(m12_2020)
colnames(m01_2021)
colnames(m02_2021)
colnames(m03_2021)
colnames(m04_2021)
colnames(m05_2021)
colnames(m06_2021)
colnames(m07_2021)
colnames(m08_2021)
colnames(m09_2021)
colnames(m10_2021)
colnames(m11_2021)

#Checking the structure of data
str(m12_2020)
str(m01_2021)
str(m02_2021)
str(m03_2021)
str(m04_2021)
str(m05_2021)
str(m06_2021)
str(m07_2021)
str(m08_2021)
str(m09_2021)
str(m10_2021)
str(m11_2021)

#Combining all data from last 12months into a single file
joined_df <-rbind(m12_2020,m01_2021,m02_2021,m03_2021,m04_2021,m05_2021,m06_2021,
                  m07_2021,m08_2021,m09_2021,m10_2021,m11_2021)

#Removing start_lat, start_lng, end_lat and end_lng fields

joined_df <- joined_df %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))
            


#Documentation of data processing
#Check the data to see if there are errors:

head(joined_df) #showing first 6 rows

tail(joined_df) #showing the last 6 rows


colnames(joined_df) #checking columns names

nrow(joined_df) #checking the number of rows in joined data set

dim(joined_df) #looking the dimensions of data set

str(joined_df)#Checking columns and data types.


#Statistical summary of the data set.
summary(joined_df)


#Checking column variables "Member_Casual".
unique(joined_df$member_casual)


#Checking existence of empty rows and columns.

sum(is.na(joined_df) == nrow(joined_df))
sum(is.na(joined_df) == ncol(joined_df))



#Checking if exists duplicates in the column Ride_id
joined_df[duplicated(joined_df$ride_id),]



#Looking the number of observations falling under each "Member_Casual"
table(joined_df$member_casual)


#Adding columns that list the date, month, day, and year of each ride

joined_df$date <- as.Date(joined_df$started_at) #The default format is yyyy-mm-dd
joined_df$month <- format(as.Date(joined_df$date), "%m")
joined_df$day <- format(as.Date(joined_df$date), "%d")
joined_df$year <- format(as.Date(joined_df$date), "%Y")
joined_df$day_of_week <- format(as.Date(joined_df$date), "%A")


# Adding a "ride_length" calculation to all_trips (in seconds)
joined_df$ride_length <- difftime(joined_df$ended_at,joined_df$started_at)



# Inspecting the structure of the columns
str(joined_df)


#Checking the data type of the new created ride_length column(type: numeric + character)
str(joined_df$ride_length)



#Converting "ride_length" from Factor to numeric for making calculations on the data
is.factor(joined_df$ride_length)
joined_df$ride_length <- as.numeric(as.character(joined_df$ride_length))
is.numeric(joined_df$ride_length)



#Removing "bad" data
#Cleaning data set by removing rows where ride_length was negative

joined_df_v2 <- joined_df[!(joined_df$ride_length<0),]

#Descriptive analysis on ride_length (all figures in seconds)
mean(joined_df_v2$ride_length) #straight average (total ride length / rides)
max(joined_df_v2$ride_length) #longest ride
min(joined_df_v2$ride_length) #shortest ride

summary(joined_df_v2)



#Comparing members and casual users by mean, median, max and min
aggregate(joined_df_v2$ride_length ~ joined_df_v2$member_casual, FUN = mean)
aggregate(joined_df_v2$ride_length ~ joined_df_v2$member_casual, FUN = max)
aggregate(joined_df_v2$ride_length ~ joined_df_v2$member_casual, FUN = min)


#Discovering patterns between the average ride time by each day for members vs casual users
aggregate(joined_df_v2$ride_length ~ joined_df_v2$member_casual + 
            joined_df_v2$day_of_week, FUN = mean)




#Arranging the days of the week by order.
joined_df_v2$day_of_week <- ordered(joined_df_v2$day_of_week, 
                                    levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))




#Discovering patterns between the average ride time by each day for members vs casual users on weekday order
aggregate(joined_df_v2$ride_length ~ joined_df_v2$member_casual +
            joined_df_v2$day_of_week, FUN = mean)





#Analyzing ridership data by user type and day of the week

joined_df_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by user type and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts


#Creating a visualization for number of rides vs per user type per weekday

joined_df_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title = "Usage of bicycles", subtitle = "By day of the week", x = "Day of week", y = "Number of Rides", fill = 'User type')+
  geom_col(position = "dodge")


#Analyzing average duration of travel by user type and weekday and creating a visualization 
joined_df_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title = "Usage of bicycles", subtitle = "By average duration", x = "Day of week", y = "Average duration", fill = 'User type')+
  
  geom_col(position = "dodge")




#Usage of specific type of bicycle during a week per user type

joined_df_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(rideable_type) %>%
  summarize(count=length(ride_id),
            percentage_of_total=(length(ride_id)/nrow(joined_df_v2))*100,
            members_count=sum(member_casual=="member"),
            casual_count=sum(member_casual=="casual"))


joined_df_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  ggplot(aes(rideable_type, fill = member_casual)) +
    
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title = "Bicycle type per user", subtitle = "By day of week", x = "Bicycle type", y = "Number of Rides", fill = 'User type')+
    geom_bar()+
    facet_wrap(~weekday)+
    theme(axis.text.x = element_text(angle=70))
  
  



#Visualization of number of rides on monthly matter per user type

joined_df_v2 %>% 
  mutate(month = month(started_at, label= TRUE)) %>% 
  group_by(member_casual, month) %>%
  summarize(number_of_rides =n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual,month)
            


ggplot(joined_df_v2, aes(month(started_at), fill=member_casual))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title = "Use of the bicycle",subtitle = "By month", x = "Month", y = "Number of Rides", fill = 'Type of user')+
  geom_bar()


#Number of casual vs member types of users
ggplot(joined_df_v2, aes(member_casual, fill=member_casual))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title = "Users",subtitle = "Comparison of the number of users", x = "Users", y = "Number of Users", fill = 'Type of user')+
  geom_bar()

joined_df_v2 %>%
  group_by(member_casual) %>%
  summarize(number_of_rides =n())


#Saving our data in csv file
write.csv(joined_df_v2, file = "Capstone_analysis.csv", row.names = TRUE)

