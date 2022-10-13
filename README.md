# CASE-STUDY-Capstone-Project
This project is the capstone assignment for the Google Data Analytics Professional Certificate program. The program prepares participants for a career in data analytics with training focused on key analytical skills (data cleaning, analysis, and visualization) and tools (Excel, SQL, R Programming, Tableau)
[The Analysis Preview](https://github.com/claykays/CASE-STUDY-Capstone-Project/blob/d1b0bbd8b482027756a29c71603d3386066081d7/Cyclisti_-Case_Study.html)


## Background

In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are geotracked and locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system anytime.

Until now, Cyclistic' s marketing strategy relied on building general awareness and appealing to broad consumer segments. One approach that helped make these things possible was the flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships. Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members.

Cyclistic' s finance analysts have concluded that annual members are much more profitable than casual riders. Although the pricing flexibility helps Cyclistic attract more customers, Moreno(director of marketing) believes that maximizing the number of annual members will be key to future growth. Rather than creating a marketing campaign that targets all-new customers, Moreno believes there is a very good chance to convert casual riders into members. She notes that casual riders are already aware of the Cyclistic program and have chosen Cyclistic for their mobility needs

## Goals

Design marketing strategies aimed at converting casual riders into annual members. To do that, however, the marketing analyst team needs to better understand how annual members and casual riders differ, why casual riders would buy a membership, and how digital media could affect their marketing tactics. Moreno and her team are interested in analyzing the Cyclistic historical bike trip data to identify trends

# Ask

Three questions will guide the future marketing program: 1. How do annual members and casual riders use Cyclistic bikes differently? 2. Why would casual riders buy Cyclistic annual memberships? 3. How can Cyclistic use digital media to influence casual riders to become members?

### Objectives

This Analysis will focus on the first question. (How do annual members and casual riders use Cyclistic bikes differently?)

# Prepare

Data is obtained [from](https://divvy-tripdata.s3.amazonaws.com/index.html) for period October 2021 to September 2022

### Load Libraries

Load the data and packages. Also setup working directory . All packages already installed.

```{r Load Libraries}
library(dplyr)# we wll use some o its functions for manipulation(helps wrangle data)
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
options(dplyr.summarise.inform = FALSE)# this suppresses the summarise group warnings
#getwd() #displays your working directory
setwd("/Users/clayt/OneDrive/Documents/R Programming Projects/capstone_project")#sets  working directory
options(readr.show_types = FALSE)
```

## STEP 1: COLLECT DATA

This stage we downloaded data and imported it into R.

```{r Collect data}

oct_2021 <- read_csv("202110-divvy-tripdata.csv")
nov_2021 <- read_csv("202111-divvy-tripdata.csv")
dec_2021 <- read_csv("202112-divvy-tripdata.csv")
jan_2022 <- read_csv("202201-divvy-tripdata.csv")
feb_2022 <- read_csv("202202-divvy-tripdata.csv")
mar_2022 <- read_csv("202203-divvy-tripdata.csv")
apr_2022 <- read_csv("202204-divvy-tripdata.csv")
may_2022 <- read_csv("202205-divvy-tripdata.csv")
jun_2022 <- read_csv("202206-divvy-tripdata.csv")
jul_2022 <- read_csv("202207-divvy-tripdata.csv")
aug_2022 <- read_csv("202208-divvy-tripdata.csv")
sep_2022 <- read_csv("202209-divvy-publictripdata.csv")
options(readr.show_col_types  = FALSE)
```

## STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE

Compare column names each of the files.While the names don't have to be in the same order.

```{r chech column names}
#colnames(oct_2021)
#colnames(nov_2021)
#colnames(dec_2021)
#colnames(jan_2022)
#colnames(feb_2022)
#colnames(mar_2022)
#colnames(apr_2022)
#colnames(may_2022)
#colnames(jun_2022)
#colnames(jul_2022)
#colnames(aug_2022)
#colnames(sep_2022)
```

### Rename Columns

This will help make sure columns are all consistant before merging datasets into one

```{r Rename column names }
(oct_2021 <- oct_2021 %>% 
    rename( trip_id = ride_id,
            bike_id = rideable_type  ,
            start_time =  started_at , 
            end_time =  ended_at , 
            from_station_name =  start_station_name,
            from_station_id =  start_station_id,
            to_station_name =  end_station_name,
            to_station_id =  end_station_id,
            usertype =  member_casual))

(nov_2021 <- nov_2021 %>% 
    rename( trip_id = ride_id,
            bike_id = rideable_type  ,
            start_time =  started_at , 
            end_time =  ended_at , 
            from_station_name =  start_station_name,
            from_station_id =  start_station_id,
            to_station_name =  end_station_name,
            to_station_id =  end_station_id,
            usertype =  member_casual))

(dec_2021 <- dec_2021 %>% 
    rename( trip_id = ride_id,
            bike_id = rideable_type  ,
            start_time =  started_at , 
            end_time =  ended_at , 
            from_station_name =  start_station_name,
            from_station_id =  start_station_id,
            to_station_name =  end_station_name,
            to_station_id =  end_station_id,
            usertype =  member_casual))

(jan_2022 <- jan_2022 %>% 
    rename( trip_id = ride_id,
            bike_id = rideable_type  ,
            start_time =  started_at , 
            end_time =  ended_at , 
            from_station_name =  start_station_name,
            from_station_id =  start_station_id,
            to_station_name =  end_station_name,
            to_station_id =  end_station_id,
            usertype =  member_casual))


(feb_2022 <- feb_2022 %>% 
    rename( trip_id = ride_id,
            bike_id = rideable_type  ,
            start_time =  started_at , 
            end_time =  ended_at , 
            from_station_name =  start_station_name,
            from_station_id =  start_station_id,
            to_station_name =  end_station_name,
            to_station_id =  end_station_id,
            usertype =  member_casual))


(mar_2022 <- mar_2022 %>% 
    rename( trip_id = ride_id,
            bike_id = rideable_type  ,
            start_time =  started_at , 
            end_time =  ended_at , 
            from_station_name =  start_station_name,
            from_station_id =  start_station_id,
            to_station_name =  end_station_name,
            to_station_id =  end_station_id,
            usertype =  member_casual))


(apr_2022 <- apr_2022 %>% 
    rename( trip_id = ride_id,
            bike_id = rideable_type  ,
            start_time =  started_at , 
            end_time =  ended_at , 
            from_station_name =  start_station_name,
            from_station_id =  start_station_id,
            to_station_name =  end_station_name,
            to_station_id =  end_station_id,
            usertype =  member_casual))

(may_2022 <- may_2022 %>% 
    rename( trip_id = ride_id,
            bike_id = rideable_type  ,
            start_time =  started_at , 
            end_time =  ended_at , 
            from_station_name =  start_station_name,
            from_station_id =  start_station_id,
            to_station_name =  end_station_name,
            to_station_id =  end_station_id,
            usertype =  member_casual))

(jun_2022 <- jun_2022 %>% 
    rename( trip_id = ride_id,
            bike_id = rideable_type  ,
            start_time =  started_at , 
            end_time =  ended_at , 
            from_station_name =  start_station_name,
            from_station_id =  start_station_id,
            to_station_name =  end_station_name,
            to_station_id =  end_station_id,
            usertype =  member_casual))

(jul_2022 <- jul_2022 %>% 
    rename( trip_id = ride_id,
            bike_id = rideable_type  ,
            start_time =  started_at , 
            end_time =  ended_at , 
            from_station_name =  start_station_name,
            from_station_id =  start_station_id,
            to_station_name =  end_station_name,
            to_station_id =  end_station_id,
            usertype =  member_casual))


(aug_2022 <- aug_2022 %>% 
    rename( trip_id = ride_id,
            bike_id = rideable_type  ,
            start_time =  started_at , 
            end_time =  ended_at , 
            from_station_name =  start_station_name,
            from_station_id =  start_station_id,
            to_station_name =  end_station_name,
            to_station_id =  end_station_id,
            usertype =  member_casual))


(sep_2022 <- sep_2022 %>% 
    rename( trip_id = ride_id,
            bike_id = rideable_type  ,
            start_time =  started_at , 
            end_time =  ended_at , 
            from_station_name =  start_station_name,
            from_station_id =  start_station_id,
            to_station_name =  end_station_name,
            to_station_id =  end_station_id,
            usertype =  member_casual))


                  



                #   ,ride_id = "01 - Rental Details Rental ID"
                 #  ,rideable_type = "01 - Rental Details Bike ID" 
                 #  ,started_at = "01 - Rental Details Local Start Time"  
                  # ,ended_at = "01 - Rental Details Local End Time"  
                 #  ,start_station_name = "03 - Rental Start Station Name" 
                  # ,start_station_id = "03 - Rental Start Station ID"
                  # ,end_station_name = "02 - Rental End Station Name" 
                  # ,end_station_id = "02 - Rental End Station ID"
                  # ,member_casual = "User Type"))


# Inspect the dataframes and look for incosistances

str(nov_2021)#run this for all to check data types
#Do this for all datasets
```

## Combine dataframes

```{r}
# Stack individual Monthly data frames into one big data frame called (bike_share)

bike_share <- bind_rows(oct_2021, nov_2021, jan_2022, feb_2022,oct_2021, mar_2022, apr_2022, may_2022,jun_2022,jul_2022,aug_2022,sep_2022)
```

# Process

## STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS

```{r Inspect Data}
# Inspect the new table that has been created
colnames(bike_share)  #List of column names
nrow(bike_share)  #How many rows are in data frame?has before cleaning(6211921)
#dim(bike_share)  #Dimensions of the data frame?
head(bike_share)  #See the first 6 rows of data frame.  
#tail(bike_share)
#str(bike_share)  #See list of columns and data types (numeric, character, etc)
summary(bike_share)  #Statistical summary of data. Mainly for numerics

# There are a few problems we will need to fix:

# (2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride since the  data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

```

### Observe Data

Begin by seeing how many observations fall under each user type(member or Casual)

```{r}
# Begin by seeing how many observations fall under each usertype

table(bike_share$usertype)
```

### Add additional calculated fields

```{r Add additional calculated fields}

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
bike_share$date <- as.Date(bike_share$start_time) #The default format is yyyy-mm-dd
bike_share$month <- format(as.Date(bike_share$date), "%m")
bike_share$day <- format(as.Date(bike_share$date), "%d")
bike_share$year <- format(as.Date(bike_share$date), "%Y")
bike_share$day_of_week <- format(as.Date(bike_share$date), "%A")

# Add a "ride_length" calculation to bike_share (in seconds)

bike_share$ride_length <- difftime(bike_share$end_time,bike_share$start_time)
```

### Inspect The data

```{r Inspect The data}
# Inspect the structure of the columns
str(bike_share)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(bike_share$ride_length)
bike_share$ride_length <- as.numeric(as.character(bike_share$ride_length))
is.numeric(bike_share$ride_length)
```

## Remove "bad" data

A mini Eye analysis shows there are trips which have duration that is greater than zero yet there is no start station and destination. these rows will be ommited from the analysis and noted during presantation

```{r remove bad data}
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/

bike_share_v3 <- bike_share %>%
  select(from_station_name,from_station_id,to_station_id,    to_station_name,ride_length) %>% 
  filter(ride_length > 0 ) 

slice(bike_share_v3,1:10)
#This analysis shows that there are trips without from_statin or to_stattion, even though the ride length is greater than 0.
# We dont have enough information as to how to deal with these , we will procceed and remove them from our analysis, and will be noted 
#we will note these whn we present findings
```

### Remove Null values and duplicates

```{r Remove NA and duplicates}
bike_share_v2 <- bike_share %>% 
  filter(ride_length > 0 ) %>%   #remove where ride length is negative or no ride 
drop_na(from_station_name) %>% #drop all record with NA (from and to stations) records
drop_na(to_station_name) %>% 
drop_na(from_station_id) %>% 
drop_na(to_station_id) 


nrow(bike_share) #count Original dataset
nrow(bike_share_v2)#count cleaned dataset

#=====================================
slice(bike_share_v2,1:50)

# We are happy with the new data set and we will proceed to Analysis Stage
```

# Analyse

## STEP 4: CONDUCT DESCRIPTIVE ANALYSIS

Descriptive analysis on ride_length (all figures in seconds) and Compare members and casual users

```{r Descriptive Analysis}
# Descriptive analysis on ride_length (all figures in seconds)
mean(bike_share_v2$ride_length) #straight average (total ride length / rides)
median(bike_share_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(bike_share_v2$ride_length) #longest ride
min(bike_share_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(bike_share_v2$ride_length)

# Compare members and casual users
aggregate(bike_share_v2$ride_length ~ bike_share_v2$usertype, FUN = mean)
aggregate(bike_share_v2$ride_length ~ bike_share_v2$usertype, FUN = median)
aggregate(bike_share_v2$ride_length ~ bike_share_v2$usertype, FUN = max)
aggregate(bike_share_v2$ride_length ~ bike_share_v2$usertype, FUN = min)

```

### Further Analysis

```{r average ride time by each day}
# See the average ride time by each day for members vs casual users
aggregate(bike_share_v2$ride_length ~ bike_share_v2$usertype + bike_share_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that by ordering the day_of_week.
bike_share_v2$day_of_week <- ordered(bike_share_v2$day_of_week, levels=c("Saturday","Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(bike_share_v2$ride_length ~ bike_share_v2$usertype + bike_share_v2$day_of_week, FUN = mean)
```

### Analyze ridership data by type and weekday

```{r analyze ridership data by type and weekday}
# analyze ridership data by type and weekday
bike_share_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(usertype, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()                            #calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>%         # calculates the average duration
  arrange(usertype, weekday)
```

## Analysis by Visualization

### Visualize the number of rides by rider type

```{r visualize the number of rides by rider type}
# Let's visualize the number of rides by rider type
bike_share_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge")
```

#### Observations

Members have an almost consistent riding pattern throughout the week that rises mid week. Casual members tend to increase during weekends

### Visualize the average duration by rider type

```{r visualize the average duration by rider type}
# Let's create a visualization for average duration
bike_share_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = usertype)) +
  geom_col(position = "dodge")+
  labs(title ="average duration of Rides by Ridertype Weekly", 
       caption = "Cyclistic’s historical trip data(Case Study)",
       x="Month",
       y="Average Duration of Rides")
```

#### Observations

Casual members tend to have longer ride duration than members and this pattern is consistent any day of the week

### Visualize the number of rides by rider type monthly

```{r Visualize the number of rides by rider type monthly}
# Let's visualize the number of rides by rider type monthly
bike_share_v2 %>% 
  mutate(month_name = month(start_time, label = TRUE)) %>% 
  group_by(usertype,year,month_name) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(usertype,year, month_name)  %>% 
  ggplot(aes(x = number_of_rides, y = month_name, fill = usertype , group = factor(usertype ))) +
  geom_col(position = "dodge")+
  labs(title ="Number of Rides by Rider type Monthly", 
       caption = "Cyclistic’s historical trip data(Case Study)",
       x="Numer of Rides",
       y="Month")
```

#### Observations

general number of rides for both casual and members is lowest in January through to Aprial and peeks during the months of June,July and August.there was a decline in September and a huge peek in October

### A snap Analysis of bike type used by rider type Monthly

```{r snap Analysis of bike type used by rider type Monthly}
#A snap Analys of bike type used by rider type Monthly
bike_share_v2 %>% 
  mutate(month_name = month(start_time, label = TRUE)) %>% 
  group_by(usertype,bike_id,month_name) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(usertype, month_name)  %>% 
  ggplot(aes(x =month_name , y = number_of_rides, fill = bike_id , group = factor(usertype ))) +
  geom_col(position = "dodge")+
  facet_wrap(~usertype)+
  labs(title ="Number of Rides by Rider type by Bike type  Monthly", 
       caption = "Cyclistic’s historical trip data(Case Study)",
       x="Month",
       y="Numer of Rides")
```

#### Observations

Classic and electric bikes are the most used bikes.

### A snap Analysis of bike type used by rider type weekly

```{r A snap Analysis of bike type used by rider type weekly}
#A snap Analysis of bike type used by rider type weekly
bike_share_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>%  
  group_by(usertype,bike_id,weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x =weekday , y = number_of_rides, fill = bike_id , group = factor(usertype ))) +
  geom_col(position = "dodge")+
  facet_wrap(~usertype)+
  labs(title ="Number of Rides by Rider type by Bike type  Weekly", 
       caption = "Cyclistic’s historical trip data(Case Study)",
       x="Day Of Week",
       y="Numer of Rides")
```

### A snap Analysis of bike type used by rider type

```{r}
#A snap Analysis of bike type used by rider type 
bike_share_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>%  
  group_by(usertype,bike_id) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(usertype,bike_id)  %>% 
  ggplot(aes(x =bike_id , y = number_of_rides, fill = bike_id , group = factor(usertype ))) +
  geom_col(position = "dodge")+
  facet_wrap(~usertype)+
  labs(title ="Number of Rides by  by Bike type  ", 
       caption = "Cyclistic’s historical trip data(Case Study)",
       x="Type of Bike",
       y="Numer of Rides")
```

### Total Casual members ride vs member rides

```{r Total Casual members ride vs member rides}
#Total Casual members ride vs member rides
bike_share_v2 %>%
  group_by(usertype) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  mutate(total_number_of_rides = `number_of_rides` / sum(`number_of_rides`)) %>% 
  mutate(labels = scales::percent(total_number_of_rides,accuracy = 1L)) %>% 
  ggplot( aes(x = "", y = total_number_of_rides, fill = usertype)) +
  geom_col(color = "black") +
  geom_label(aes(label =  labels),
             color = "white",
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y")
```

#### Observations

Generally the number of rides is bigger for members, with casual members forming 41% of the riders

# Act: Key Findings & Recommendations

## Key Findings

The following key findings where observed + The number of casual rides increases during the weekend. + The peek Months for casual rides is between June and October.However there is a decline in the month of September + 41 percent of observed riders are casual riders

### Limitations

The following Limitations and or assumptions where made: + All records without start and destination station where omitted. + Information about the gender of participants unknown and was not considered. + Pricing structure was not stated. this could influence a rider choosing a package.

## Recommendations

The following Recommendations are made: + The company may try to run pay per ride kind of a package to encourage casual riders to join and pay mostly for the active days(weekends) + The is encouraged to do more targeted campaigns towards the Months of (June to October ) to capture and register the influx of casual riders who increase in that period + 41 percent of observed riders are casual riders. run campaigns to capture this group. Introducing membership rates that are favorable to summer and weekends riders
