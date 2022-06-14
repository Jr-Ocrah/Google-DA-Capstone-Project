#Library necessary for importing excel files
install.packages("readxl")
library(readxl)

#Data Cleaning Process
install.packages("tidyverse")
library(tidyverse) #helps wrangle data

install.packages("lubridate")
library(lubridate) #helps wrangle date attributes

install.packages("skimr")
library(skimr) #get summary data
install.packages("janitor")
library(janitor)
library(anytime)

install.packages("scales")
library(scales)
install.packages("mapview")
library(mapview)#for visualization

#Importing the data
Trips_2014Q1Q2 <- read_csv('C:/Users/Dell/Downloads/Bike Sharing/2014/Cyclistic_2014_Q1Q2/Divvy_Trips_2014_Q1Q2.csv')
Trips_2014July <- read_csv('C:/Users/Dell/Downloads/Bike Sharing/2014/Cyclistic_2014_Q3Q4/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q3-07.csv')
Trips_2014_0809 <- read_csv('C:/Users/Dell/Downloads/Bike Sharing/2014/Cyclistic_2014_Q3Q4/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q3-0809.csv')
Trips_2014Q4 <- read_csv('C:/Users/Dell/Downloads/Bike Sharing/2014/Cyclistic_2014_Q3Q4/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q4.csv')
Trip_2014_StationsQ1Q2 <- read_excel('C:/Users/Dell/Downloads/Bike Sharing/2014/Cyclistic_2014_Q1Q2/Divvy_Stations_2014_Q1Q2.xlsx')
Trips_2014_StationsQ3Q4 <- read_csv('C:/Users/Dell/Downloads/Bike Sharing/2014/Cyclistic_2014_Q3Q4/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Stations_2014-Q3Q4.csv')

#Comparing column names manually
colnames(Trips_2014Q1Q2)
colnames(Trips_2014July)
colnames(Trips_2014_0809)
colnames(Trips_2014Q4)

#Comparing datatypes
str(Trips_2014Q1Q2)
str(Trips_2014July)
str(Trips_2014_0809)
str(Trips_2014Q4)

#Identifying the different columns
compare_df_cols(Trips_2014Q1Q2,
                Trips_2014July,
                Trips_2014_0809,
                Trips_2014Q4, return = "mismatch")

#Adding the data
Trip_2014 <- bind_rows(Trips_2014Q1Q2,
                       Trips_2014July,
                       Trips_2014_0809,
                       Trips_2014Q4)
Trip_dir <- bind_rows(Trip_2014_StationsQ1Q2,
                      Trips_2014_StationsQ3Q4)


#Previewing combined data
colnames(Trip_2014)
dim(Trip_2014)
head(Trip_2014)
str(Trip_2014)
summary(Trip_2014)
skim(Trip_2014)

#Finding popular stations
Stations_1 <- Trip_2014 %>% 
  group_by(from_station_name) %>% tally()%>% arrange(-n)
Stations_1 = Stations_1[1:10,]

#Creating data frames for popular stations
start_lat <- c(41.89107,41.88096,41.92628,41.88338,41.86406,41.88103,41.88209,41.87895,41.91172,41.88473 )
start_long <- c(-87.6122,-87.61674,-87.63083,-87.64117,-87.62373,-87.62408,-87.63983,-87.63975,-87.62680,-87.61952 )
start_name <- c("Streeter Dr & Illinois St","Lake Shore Dr & Monroe St","Theater on the Lake","Clinton St & Washington Blvd","Michigan Ave & Oak St","Millennium Park","Canal St & Madison St","Canal St & Adams St","Lake Shore Dr & North Blvd", "Columbus Dr & Randolph St ")
start_num <- c(54214,41326,38667,37755,34668,32075,30277,30165,29208,26797)
popular_start <- data.frame(start_name, start_lat,start_long,start_num)

Stations_2 <- Trip_2014 %>% 
  group_by(to_station_name) %>% tally()%>% arrange(-n)
Stations_2 = Stations_2[1:10,]
end_name <- c("Streeter Dr & Illinois St","Lake Shore Dr & Monroe St","Theater on the Lake","Clinton St & Washington Blvd","Michigan Ave & Oak St","Millennium Park","Canal St & Madison St","Lake Shore Dr & North Blvd","Canal St & Adams St", "Museum Campus")
end_lat <- c(41.89107,41.88096,41.92628,41.88338,41.86406,41.88103,41.88209,41.91172,41.87895,41.86521 )
end_long <- c(-87.6122,-87.61674,-87.63083,-87.64117,-87.62373,-87.62408,-87.63983,-87.62680,-87.63975,-87.61776 )
end_num <- c(67048,42060,41297,39517,37422,35481,34650,32613,29082,26982)
popular_end <- data.frame(end_name, end_lat, end_long,end_num)

Finding popular stations by usertype
pop_start = Trip_2014 %>% 
  group_by(
    usertype, from_station_name
  ) %>% 
  summarise(
    nstart = n()
  ) %>% 
  arrange(-nstart)

pop_end = Trip_2014 %>% 
  group_by(
    usertype, to_station_name
  ) %>% 
  summarise(
    n_end = n()
  ) %>% 
  arrange(-n_end)

#Changing date format
Trip_2014$starttime <- as.POSIXct(Trip_2014$starttime, "%m/%d/%Y %H:%M", tz = 'Europe/London')

#Separating date attributes for analysis
Trip_2014$date <- as.Date(Trip_2014$starttime) 
Trip_2014$month <- format(as.Date(Trip_2014$date), "%m")
Trip_2014$day_of_week <- format(as.Date(Trip_2014$date), "%A")


#Data Analysis and Visualizations

#Day of the week analysis and visualization
ride_week = Trip_2014 %>% 
  group_by(
    usertype, day_of_week
  ) %>% 
  summarise(
    nr_rides_week = n(),
    avg_rides_week = mean(tripduration),
    total_duration_week = sum(tripduration)
  )

#Number of Trips by Week Days
ride_week = drop_na(ride_week)
ride_week$day_of_week <- factor(ride_week$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
ride_week %>% 
  ggplot(aes(day_of_week, nr_rides_week, fill = usertype))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  labs(
    title = "Number of Trips by Week Days and Segmented by Users",
    subtitle = "Number of trips for each day of the year",
    caption = "Fig 1 - Samuel Junior Ocrah",
    x = "day of the week",
    y = "number of trips"
  )+
  theme()

#Total Time Trips by Week Days
ride_week %>% 
  ggplot(aes(day_of_week, total_duration_week, fill = usertype))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  labs(
    title = "Total Time Trips by Week Days and Segmented by Users",
    subtitle = "Total Trips Time for every week of the year",
    caption = "Fig 2 - Samuel Junior Ocrah",
    x = "day of the week",
    y = " total time trips"
  )+
  theme()

#Month of the year Analysis and Visualization
ride_month = Trip_2014 %>% 
  group_by(
    usertype, month
  ) %>% 
  summarise(
    nr_rides_month = n(),
    avg_rides_month = mean(tripduration),
    total_time_month = sum(tripduration)
  )

#Changing month numbers to names
ride_month = drop_na(ride_month)
str(ride_month)
ride_month$month = as.numeric(as.character(ride_month$month))
ride_month$month = month.name[ride_month$month]
ride_month$month <- factor(ride_month$month, levels = c("January","February","March","April","May","June","July","August","September
","October","November","December"))

#Number of Trips by Month 
ride_month %>% 
  ggplot(aes(month, nr_rides_month, fill = usertype))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  labs(
    title = "Number of Trips by Month and Segmented by Users",
    subtitle = "Number Trips Time for every Month",
    caption = "Fig 3 - Samuel Junior Ocrah",
    x = "month",
    y = " number of trips"
  )+
  theme()

#Total Trips Time by Month
ride_month %>% 
  ggplot(aes(month, total_time_month, fill = usertype))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  labs(
    title = "Total Trips Time by Month and Segmented by Users",
    subtitle = "Total Trips Time for every Month",
    caption = "Fig 4 - Samuel Junior Ocrah",
    x = "month",
    y = "total trips time"
  )+
  theme()

#Bar chart of Most Popular Start Stations
pop_start[1:10,] %>% 
  ggplot(aes(from_station_name, nstart, fill = usertype))+
  geom_col(position = "dodge")+
  coord_flip()+
  labs(
    title = "Most Popular Start Stations",
    subtitle = "Top 10 most popular start stations",
    caption = "Fig 5 - Samuel Junior Ocrah",
    x = "station name",
    y = "number of trips"
  )+
  theme()

#Bar Chart of Most Popular End Stations
pop_end[1:10,] %>% 
  ggplot(aes(to_station_name, n_end, fill = usertype))+
  geom_col(position = "dodge")+
  coord_flip()+
  labs(
    title = "Most Popular End Stations",
    subtitle = "Top 10 most popular end stations",
    caption = "Fig 6 - Samuel Junior Ocrah",
    x = "station name",
    y = "number of trips"
  )+
  theme()

#Mapview of Most Popular Start Stations
popular_start %>% 
  mapview(
    xcol = "start_long", 
    ycol = "start_lat",
    cex = "start_num",
    alpha = 0.9, 
    crs = 4269,
    color = "#8b0000",
    grid = F, 
    legend = T,
    layer.name = "10 Most Popular Start Stations")

#Mapview of Most Popular End Stations
popular_end %>% 
  mapview(
    xcol = "end_long", 
    ycol = "end_lat",
    cex = "end_num",
    alpha = 0.9, 
    crs = 4269,
    color = "#8b0000",
    grid = F, 
    legend = T,
    layer.name = "10 Most Popular End Stations")

