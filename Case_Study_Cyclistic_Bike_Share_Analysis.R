install.packages("rmarkdown") # to save the rmarkdown file
install.packages("tidyverse") # to data import and wrangling
install.packages("lubridate") # to use date functions
install.packages("ggplot") # to visualize

library(tidyverse)  #helps wrangle data
library(lubridate) #helps wrangle date attributes
library(ggplot2) #helps visualize data
library(dplyr) # for efficiently manipulating datasets
library(tidyr) #to help to create tidy or clean data
setwd("/Users/ASUS/Documents/Cyclistic_Blike_Share/") #sets your working directory to simplify calls to data ... make sure to use your OWN username instead of mine

# STEP 1: COLLECT All DATA
## Loading data from 01/2021 to 04/2022

trip_2021_01 <-read.csv("202101-divvy-tripdata.csv")
trip_2021_02 <-read.csv("202102-divvy-tripdata.csv")
trip_2021_03 <-read.csv("202103-divvy-tripdata.csv")
trip_2021_04 <-read.csv("202104-divvy-tripdata.csv")
trip_2021_05 <-read.csv("202105-divvy-tripdata.csv")
trip_2021_06 <-read.csv("202106-divvy-tripdata.csv")
trip_2021_07 <-read.csv("202107-divvy-tripdata.csv")
trip_2021_08 <-read.csv("202108-divvy-tripdata.csv")
trip_2021_09 <-read.csv("202109-divvy-tripdata.csv")
trip_2021_10 <-read.csv("202110-divvy-tripdata.csv")
trip_2021_11 <-read.csv("202111-divvy-tripdata.csv")
trip_2021_12 <-read.csv("202112-divvy-tripdata.csv")
trip_2022_01 <-read.csv("202201-divvy-tripdata.csv")
trip_2022_02 <-read.csv("202202-divvy-tripdata.csv")
trip_2022_03 <-read.csv("202203-divvy-tripdata.csv")
trip_2022_04 <-read.csv("202204-divvy-tripdata.csv")

# WRANGLE DATA AND COMBINE IT INTO A SINGLE FILE
## Check the column names in each of the files is the same order or not, if not match by order, we must to update them before joining them into one file.

colnames(trip_2021_01)
colnames(trip_2021_02)
colnames(trip_2021_03)
colnames(trip_2021_04)
colnames(trip_2021_05)
colnames(trip_2021_06)

colnames(trip_2021_07)
colnames(trip_2021_08)
colnames(trip_2021_09)
colnames(trip_2021_10)
colnames(trip_2021_11)
colnames(trip_2021_12)

colnames(trip_2022_01)
colnames(trip_2022_02)
colnames(trip_2022_03)
colnames(trip_2022_04)

# Combine all file of 2021 as a single file
bike_share_2021<-bind_rows(trip_2021_01,trip_2021_02,trip_2021_03,trip_2021_04,
                           trip_2021_05,trip_2021_06,trip_2021_07,trip_2021_08,
                           trip_2021_09,trip_2021_10,trip_2021_11,trip_2021_12)

# Combine all file of 2022 as a single file
bike_share_2022<-bind_rows(trip_2022_01,trip_2022_02,trip_2022_03,trip_2022_04)

# CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
# Now let's see what the data is look like

# Viewing all file by View function
View(bike_share_2021)
View(bike_share_2022)

# Viewing the internal structure of all columns and data types (numeric, character, etc)
str(bike_share_2021)
str(bike_share_2022)

nrow(bike_share_2021)  #How many rows are in data frame?
nrow(bike_share_2022)

dim(bike_share_2021)  #Dimensions of the data frame?
dim(bike_share_2022)

head(bike_share_2021)  #See the first 6 rows of data frame.  Also tail(all_trips)
head(bike_share_2022)

# Statistical summarize all data by summary function
summary(bike_share_2021)
summary(bike_share_2022)

# Drop all Null

# There are 4771 NA in bike_share_2021 and also have 746 NA in bike_share_2022. So, I drop NA using drop_na function. After that, check clean data frame.
clean_bike_share_2021<-drop_na(bike_share_2021)
View(clean_bike_share_2021)
dim(clean_bike_share_2021)

clean_bike_share_2022<-drop_na(bike_share_2022)
View(clean_bike_share_2022)
dim(clean_bike_share_2022)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level

clean_bike_share_2021$date <- as.Date(clean_bike_share_2021$started_at) #The default format is yyyy-mm-dd
clean_bike_share_2021$month<-format(as.Date(clean_bike_share_2021$date), "%m")
clean_bike_share_2021$day <- format(as.Date(clean_bike_share_2021$date), "%d")
clean_bike_share_2021$year <- format(as.Date(clean_bike_share_2021$date), "%Y")
clean_bike_share_2021$day_of_week <- format(as.Date(clean_bike_share_2021$date), "%A")

clean_bike_share_2022$date <- as.Date(clean_bike_share_2022$started_at)
clean_bike_share_2022$month <- format(as.Date(clean_bike_share_2022$date), "%m")
clean_bike_share_2022$day <- format(as.Date(clean_bike_share_2022$date), "%d")
clean_bike_share_2022$year <- format(as.Date(clean_bike_share_2022$date), "%Y")
clean_bike_share_2022$day_of_week <- format(as.Date(clean_bike_share_2022$date), "%A")

## Add a "ride_length" calculation to all clean_bike_share data frame(in seconds)
clean_bike_share_2021$ride_length <- difftime(clean_bike_share_2021$ended_at,clean_bike_share_2021$started_at)
clean_bike_share_2022$ride_length <- difftime(clean_bike_share_2022$ended_at,clean_bike_share_2022$started_at)

# Inspect the structure of the columns
str(clean_bike_share_2021)
str(clean_bike_share_2022)

# Convert "ride_length" from factor to numeric so we can run calculations on the data
is.factor(clean_bike_share_2021$ride_length)
clean_bike_share_2021$ride_length <- as.numeric(as.character(clean_bike_share_2021$ride_length))
is.numeric(clean_bike_share_2021$ride_length)

is.factor(clean_bike_share_2021$ride_length)
clean_bike_share_2021$ride_length <- as.numeric(as.character(clean_bike_share_2021$ride_length))
is.numeric(clean_bike_share_2021$ride_length)

# Remove "missing" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new update dataframe since data is being removed
update_clean_bike_share_2021 <- clean_bike_share_2021[!(clean_bike_share_2021$start_station_name == "HQ QR" | clean_bike_share_2021$ride_length<0),]
update_clean_bike_share_2022 <- clean_bike_share_2022[!(clean_bike_share_2022$start_station_name == "HQ QR" | clean_bike_share_2022$ride_length<0),]

# Descriptive analysis on ride_length of 2021(all figures in seconds)
mean(update_clean_bike_share_2021$ride_length) #straight average (total ride length / rides)
median(update_clean_bike_share_2021$ride_length) #midpoint number in the ascending array of ride lengths
max(update_clean_bike_share_2021$ride_length) #longest ride
min(update_clean_bike_share_2021$ride_length) #shortest ride

# Descriptive analysis on ride_length of 2022(all figures in seconds)
mean(update_clean_bike_share_2022$ride_length) #straight average (total ride length / rides)
median(update_clean_bike_share_2022$ride_length) #midpoint number in the ascending array of ride lengths
max(update_clean_bike_share_2022$ride_length) #longest ride
min(update_clean_bike_share_2022$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(update_clean_bike_share_2021$ride_length)
summary(update_clean_bike_share_2022$ride_length)

# Compare members and casual users
aggregate(update_clean_bike_share_2021$ride_length ~ update_clean_bike_share_2021$member_casual, FUN = mean)
aggregate(update_clean_bike_share_2021$ride_length ~ update_clean_bike_share_2021$member_casual, FUN = median)
aggregate(update_clean_bike_share_2021$ride_length ~ update_clean_bike_share_2021$member_casual, FUN = max)
aggregate(update_clean_bike_share_2021$ride_length ~ update_clean_bike_share_2021$member_casual, FUN = min)

aggregate(update_clean_bike_share_2022$ride_length ~ update_clean_bike_share_2022$member_casual, FUN = mean)
aggregate(update_clean_bike_share_2022$ride_length ~ update_clean_bike_share_2022$member_casual, FUN = median)
aggregate(update_clean_bike_share_2022$ride_length ~ update_clean_bike_share_2022$member_casual, FUN = max)
aggregate(update_clean_bike_share_2022$ride_length ~ update_clean_bike_share_2022$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(update_clean_bike_share_2021$ride_length ~ update_clean_bike_share_2021$member_casual + update_clean_bike_share_2021$day_of_week, FUN = mean)
aggregate(update_clean_bike_share_2022$ride_length ~ update_clean_bike_share_2022$member_casual + update_clean_bike_share_2022$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
update_clean_bike_share_2021$day_of_week <- ordered(update_clean_bike_share_2021$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
update_clean_bike_share_2022$day_of_week <- ordered(update_clean_bike_share_2022$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(update_clean_bike_share_2021$ride_length ~ update_clean_bike_share_2021$member_casual + update_clean_bike_share_2021$day_of_week, FUN = mean)
aggregate(update_clean_bike_share_2022$ride_length ~ update_clean_bike_share_2022$member_casual + update_clean_bike_share_2022$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
  update_clean_bike_share_2021 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by user type and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

  update_clean_bike_share_2022 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by user type and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type of 2021
  update_clean_bike_share_2021 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")+
  facet_wrap(~month) +
  labs(title="Analyze the number of rides by rider type of 2021")

# Let's visualize the number of rides by rider type of 2022  
  update_clean_bike_share_2022 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")+ 
  facet_wrap(~month) +
  labs(title="Analyze the number of rides by rider type of 2022")

# visualization for average duration of 2021
  update_clean_bike_share_2021 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday,month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge2")+
  facet_wrap(~month) +
  labs(title="Visualization for average durationof by 2021")

# visualization for average duration of 2022
  update_clean_bike_share_2021 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday,month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge2")+
  facet_wrap(~month) +
  labs(title="Visualization for average durationof by 2022")
  
