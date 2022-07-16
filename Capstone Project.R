# importing csv datasets
jun_2021 <- read_csv("202106-divvy-tripdata.csv")
jul_2021 <- read_csv("202107-divvy-tripdata.csv")
aug_2021 <- read_csv("202108-divvy-tripdata.csv")
sep_2021 <- read_csv("202109-divvy-tripdata.csv")
oct_2021 <- read_csv("202110-divvy-tripdata.csv")
nov_2021 <- read_csv("202111-divvy-tripdata.csv")
dec_2021 <- read_csv("202112-divvy-tripdata.csv")
jan_2022 <- read_csv("202201-divvy-tripdata.csv")
feb_2022 <- read_csv("202202-divvy-tripdata.csv")
mar_2022 <- read_csv("202203-divvy-tripdata.csv")
apr_2022 <- read_csv("202204-divvy-tripdata.csv")
may_2022 <- read_csv("202205-divvy-tripdata.csv")

# checking column names for all csv files for consistency
colnames(jun_2021)
colnames(jul_2021)
colnames(aug_2021)
colnames(sep_2021)
colnames(oct_2021)
colnames(nov_2021)
colnames(dec_2021)
colnames(jan_2022)
colnames(feb_2022)
colnames(mar_2022)
colnames(apr_2022)
colnames(may_2022)

# checking summary for all csv files
str(jun_2021)
str(jul_2021)
str(aug_2021)
str(sep_2021)
str(oct_2021)
str(nov_2021)
str(dec_2021)
str(jan_2022)
str(feb_2022)
str(mar_2022)
str(apr_2022)
str(may_2022)

# converting ride_id and rideable_type into character format
jun_2021 <- mutate(jun_2021, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
jul_2021 <- mutate(jul_2021, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
aug_2021 <- mutate(aug_2021, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
sep_2021 <- mutate(sep_2021, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
oct_2021 <- mutate(oct_2021, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
nov_2021 <- mutate(nov_2021, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
dec_2021 <- mutate(dec_2021, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
jan_2022 <- mutate(jan_2022, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
feb_2022 <- mutate(feb_2022, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
mar_2022 <- mutate(mar_2022, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
apr_2022 <- mutate(apr_2022, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
may_2022 <- mutate(may_2022, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))

# aggregating data from different csv files
all_trips <- bind_rows(jun_2021, jul_2021, aug_2021, sep_2021, oct_2021, nov_2021, dec_2021, jan_2022, 
                       feb_2022, mar_2022, apr_2022, may_2022)

# remove start_lat, start_lng, end_lat, end_lng
all_trips <- all_trips %>% select(-c(start_lat, start_lng, end_lat, end_lng))

# overview of data
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# checking values for member_casual
distinct(all_trips, member_casual)

# splitting ymd date format into separate columns
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# identifying class of ride_length to manipulate it into seconds
str(all_trips)
class(all_trips$ride_length)
is.difftime(all_trips$ride_length)


# converting hms/difftime to factor
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
str(all_trips)
is.factor(all_trips$ride_length)

# convert hms/difftime to numeric 
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# clean up data by removing lengths < 0 and maintenance locations
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

# clean up data from all trips v2 by removing na
all_trips_v2 <- na.omit(all_trips_v2)

# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# order days of the week
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


