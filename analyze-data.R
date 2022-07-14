############################
# Exploratory Analysis (EDA)
############################

## Task 1: Inspect full_dataset
# print the first six rows of the dataset
print(head(full_dataset))

# Check the structure of the dataframe
# ensure their datetype is how it should be
print(str(full_dataset))

# Convert USERTYPE and RIDEABLE_TYPE from character to factor variable(s)
full_dataset <- full_dataset %>% mutate(USERTYPE = as.factor(USERTYPE),
                                        RIDEABLE_TYPE = as.factor(RIDEABLE_TYPE))

# Confirm that USERTYPE and RIDEABLE_TYPE are now factor variables
print(str(full_dataset)) 
print(skim(full_dataset))
"
Result:
 Both USERTYPE and RIDEABLE_TYPE are now factors.
 
 Data summary:
    5595063 obs of 9 variables
    
    column type: character - 3, RIDE_ID, START_STATION_NAME, END_STATION_NAME
                 Date - 2, START_DATE, END_DATE
                 difftime - 2, START_TIME, END_TIME
                 factor - 2, USERTYPE, RIDEABLE_TYPE
                 numeric - 2, RIDE_LENGTH, WEEK_DAY
"

## Task 2i: Exploring Categorical data: USERTYPE count
# Here, we make a count of the number of Casual riders against Annual members

# Counting the number of Casual riders against annual members
options(digits = 3)
print(sum(table(full_dataset$USERTYPE)))

perc_usertype <- full_dataset %>% 
  summarize(percent_casual = mean(USERTYPE == "casual"),
            percent_annual = mean(USERTYPE == "member"))


## Data Viz
# Here we would go through a series of visualizations

# 1. Comparing the proportions of usertypes by month
perc_usertype_by_month <- full_dataset %>% 
  group_by(month(START_DATE)) %>% 
  summarize(percent_casual = mean(USERTYPE == "casual"),
            percent_annual = mean(USERTYPE == "member"))

print(perc_usertype_by_month %>% arrange(-percent_annual))


### Percent_annual per Month
# Here, we visualize percentage of annual members for each month

# Line plot visualizing percentage of annual members for each month
print(perc_usertype_by_month %>% 
        ggplot(aes(x = `month(START_DATE)`, y = percent_annual * 100)) + 
        geom_point() + 
        geom_smooth() +
        labs(title= 'Percent_annual vs Month', x = "Month") + 
        theme_minimal())

ggsave("figs/Line plot - Percent_annual vs Month.png")


### Percent_casual per Month
# Here, we visualize percentage of casual members for each month

# Line plot visualizing percentage of casual members for each month
print(perc_usertype_by_month %>% 
        ggplot(aes(x = `month(START_DATE)`, y = percent_casual * 100)) + 
        geom_point() + 
        geom_smooth() + 
        labs(title= 'Percent_casual vs Month', x = "Month") +
        theme_minimal())

ggsave("figs/Line plot - Percent_casual vs Month.png")


### Proportion of Annuals and Casuals per Month
# Here, we visualize percentage of annual and casual members for each month

# Bar plot showing proportions of Annuals and Casuals per month
print(full_dataset %>% 
        ggplot(aes(x = month(START_DATE, label= TRUE), 
                   fill = USERTYPE)) + 
        geom_bar(position = "fill") +
        labs(title = "Proportions of Cyclistic Usertype per Month", 
             x = "Month", y = "Prop") +
        theme_minimal())

ggsave("figs/Bar plot - Proportions of Cyclistic Usertype per Month.png")


# Next, let's compare the proportions of Annuals and Casuals by week_day.
# Note: weeks days count from Sunday (1) - Saturday (7)

# 2. Comparing the proportions of usertypes by week day
perc_usertype_by_day <- full_dataset %>% 
  group_by(WEEK_DAY) %>% 
  summarize(percent_casual = mean(USERTYPE == "casual"),
            percent_annual = mean(USERTYPE == "member"))

print(perc_usertype_by_day %>% arrange(-percent_annual))


### Percent_annual per Week day
# Here, we visualize percentage of annual members for each Week Day

# Line plot visualizing percentage of annual members for each week_day}
print(perc_usertype_by_day %>% 
        ggplot(aes(x = WEEK_DAY, y = percent_annual * 100)) + 
        geom_point() + 
        geom_smooth() +
        labs(title= 'Percent_annual vs Week Day', x = "Week Day") + 
        theme_minimal())

ggsave("figs/Line plot - Percent_annual vs Week Day.png")


### Percent_casual per Week day
# Here, we visualize percentage of casual members for each Week Day

# Line plot visualizing percentage of casual members for each week_day}
print(perc_usertype_by_day %>% 
        ggplot(aes(x = WEEK_DAY, y = percent_casual * 100)) + 
        geom_point() + 
        geom_smooth() + 
        labs(title= 'Percent_casual vs Week Day', x = "Week Day") +
        theme_minimal())

ggsave("figs/Line plot - Percent_casual vs Week Day.png")


### Proportion of Annuals and Casuals per Week Day
# Here, we visualize percentage of annual and casual members for each week_day

# Bar plot showing proportions of Annuals and Casuals per week_day
print(full_dataset %>% 
        ggplot(aes(x = wday(WEEK_DAY, label= TRUE), 
                   fill = USERTYPE)) + 
        geom_bar(position = "fill") +
        labs(title = "Proportions of Cyclistic Usertype per Week", 
             x = "Week Day", y = "Prop") +
        theme_minimal())

ggsave("figs/Bar plot - Proportions of Cyclistic Usertype per Week.png")


# Next, let's compare the proportions of Annuals and Casuals by both month and week_day

# 3. Comparing usertypes by both month and week_day
perc_usertype_by_month_day <- full_dataset %>% 
  group_by(month(START_DATE), WEEK_DAY) %>% 
  summarize(percent_casual = mean(USERTYPE == "casual"),
            percent_annual = mean(USERTYPE == "member"))

print(perc_usertype_by_month_day)


### Percent_annual for the Months and Days
# Here, we visualize percentage of annual members for each Month and Day.

# Line plot showing trend for EACH month and week-day for percent_annual
print(perc_usertype_by_month_day %>% 
        ggplot(aes(x = WEEK_DAY, 
                   y = percent_annual)) +
        geom_line() + 
        ggtitle(label = "Percentage of Annual members for each Month and Day") +
        facet_wrap(~ month(`month(START_DATE)`, label = TRUE)) +
        theme_minimal())

ggsave("figs/Line plot - Percentage of Annual members for each Month and Day.png")

### Percent_casual per the Months and Days
# Here, we visualize percentage of casual members for each Month and Day

# Line plot visualizing percentage of casual members for each week_day
print(perc_usertype_by_month_day %>% 
        ggplot(aes(x = WEEK_DAY, 
                   y = percent_casual)) +
        geom_line() + 
        ggtitle(label = "Percentage of Casual members for each Month and Day") +
        facet_wrap(~ month(`month(START_DATE)`, label = TRUE)) +
        theme_minimal())

ggsave("figs/Line plot - Percentage of Casual members for each Month and Day.png")


## Task 2ii: Exploring Categorical data: USERTYPE vs RIDEABLE_TYPE
# Here, we check the types of bikes commonly used by Casual or Annual members

# Comparing categorical variables USERTYPE and RIDEABLE_TYPE using contingency table}
tb_cnt_ut_rt <- table(full_dataset$RIDEABLE_TYPE, full_dataset$USERTYPE)
print(tb_cnt_ut_rt)
print(prop.table(tb_cnt_ut_rt, 1))

## Data Viz
# Here we would go through a series of visualizations

# Checking for common Cyclistic Bike_types used in February}
print(full_dataset %>% 
        filter(month(START_DATE) == 02) %>% 
        ggplot(aes(x = USERTYPE, fill = USERTYPE)) + 
        geom_bar(position = "dodge") + 
        labs(title = "Common Cyclistic Bike_types used in February", x = "usertype") + 
        scale_y_continuous(trans = "log2") +
        facet_wrap(~RIDEABLE_TYPE) +
        theme_minimal())

ggsave("figs/Bar plot - Common Cyclistic Bike_types used in February.png")


### Facet by rideable_type and month 
# Apparently, our analysis holds true for 10 months from December.
# Let's see if that's the case in the eleventh month, by comparing February with November

# Counting common Cyclistic bike_type used in Feb and Nov
print(full_dataset %>% 
        filter(month(START_DATE) %in% c(02, 11)) %>% 
        ggplot(aes(x = USERTYPE, fill = USERTYPE)) + 
        geom_bar(position = "dodge") + 
        labs(title = "Count of Cyclistic Bike_types used in Feb and Nov",
             y = "Count") +
        facet_grid(month(START_DATE, label = TRUE) ~ RIDEABLE_TYPE) + 
        scale_y_continuous(trans = 'log2') +
        theme_minimal())

ggsave("figs/Bar plot - Common Cyclistic Bike_types used in Feb and Nov.png")


# Checking the types of bikes commonly used in each month
bike_used_per_month <- full_dataset %>% 
  group_by(month(START_DATE)) %>% 
  summarize(percent_classic = mean(RIDEABLE_TYPE == "classic_bike"),
            percent_docked = mean(RIDEABLE_TYPE == "docked_bike"),
            percent_electric = mean(RIDEABLE_TYPE == "electric_bike"))

print(bike_used_per_month %>% arrange(-percent_classic))
print(bike_used_per_month %>% arrange(-percent_docked))
print(bike_used_per_month %>% arrange(-percent_electric))


# Line plot showing trend of how classic bikes were used per month
print(bike_used_per_month %>% 
        ggplot(aes(x = `month(START_DATE)`, y = percent_classic * 100)) +
        geom_point() + 
        geom_smooth() +
        labs(title = "Trend of classic bike usage per month", 
             x = "Month", y = "Percent") +
        theme_minimal())

ggsave("figs/Line plot - Trend of classic bike usage per month.png")


# Line plot showing trend of how docked bikes were used per month
print(bike_used_per_month %>% 
        ggplot(aes(x = `month(START_DATE)`, y = percent_docked * 100)) +
        geom_point() + 
        geom_smooth() +
        labs(title = "Trend of docked bike usage per month", 
             x = "Month", y = "Percent") +
        theme_minimal())

ggsave("figs/Line plot - Trend of docked bike usage per month.png")


# Line plot showing trend of how electric bikes were used per month
print(bike_used_per_month %>% 
        ggplot(aes(x = `month(START_DATE)`, y = percent_electric * 100)) +
        geom_point() + 
        geom_smooth() +
        labs(title = "Trend of electric bike usage per month", 
             x = "Month", y = "Percent") +
        theme_minimal())

ggsave("figs/Line plot - Trend of electric bike usage per month.png")


### Scatter plot 
# Here, we visualize a scatter plot showing the linear relationship between classic bike and annual bike usage

# Regression for Electric bikes on Classic Bikes
print(bike_used_per_month %>% 
        ggplot(aes(x = percent_electric, 
                   y = percent_classic)) + 
        geom_point() + 
        geom_smooth(method = lm) +
        labs(title = "Regression for Electric bikes on Classic Bikes") +
        theme_minimal())

ggsave("figs/Scatter plot - Regression for Electric bikes on Classic Bikes.png")


# Now, let's check common bikes used per day_of_week

# Checking the types of bikes commonly used on which week_day
bike_used_per_day <- full_dataset %>% 
  group_by(WEEK_DAY) %>% 
  summarize(percent_classic = mean(RIDEABLE_TYPE == "classic_bike"),
            percent_docked = mean(RIDEABLE_TYPE == "docked_bike"),
            percent_electric = mean(RIDEABLE_TYPE == "electric_bike"))

print(bike_used_per_day)


# Line plot showing trend of how classic bikes were used per week_day
print(bike_used_per_day %>% 
        ggplot(aes(x = WEEK_DAY, y = percent_classic * 100)) +
        geom_line() + 
        labs(title = "Trend of classic bike usage per day", 
             x = "Week Day", y = "Percent") +
        theme_minimal())

ggsave("figs/Line plot - Trend of classic bike usage per day.png")


# Line plot showing trend of how docked bikes were used per week_day}
print(bike_used_per_day %>% 
        ggplot(aes(x = WEEK_DAY, y = percent_docked * 100)) +
        geom_line() +
        labs(title = "Trend of docked bike usage per day", 
             x = "Week Day", y = "Percent") +
        theme_minimal())

ggsave("figs/Line plot - Trend of docked bike usage per day.png")


# Line plot showing trend of how electric bikes were used per week_day
print(bike_used_per_day %>% 
        ggplot(aes(x = WEEK_DAY, y = percent_electric * 100)) +
        geom_line() +
        labs(title = "Trend of electric bike usage per day", 
             x = "Week Day", y = "Percent") +
        theme_minimal())

ggsave("figs/Line plot - Trend of electric bike usage per day.png")



# Now, let check the types of bikes commonly used in each months and week_day

# Checking the types of bikes commonly used in each months and week_day
bike_used_per_month_day <- full_dataset %>% 
  group_by(month(START_DATE), WEEK_DAY) %>% 
  summarize(percent_classic = mean(RIDEABLE_TYPE == "classic_bike"),
            percent_docked = mean(RIDEABLE_TYPE == "docked_bike"),
            percent_electric = mean(RIDEABLE_TYPE == "electric_bike"))

print(bike_used_per_month_day)


# Line plot showing trend of use of classic bikes in each month and week day}
print(bike_used_per_month_day %>% 
        ggplot(aes(x = WEEK_DAY, 
                   y = percent_classic)) +
        geom_line() + 
        ggtitle(label = "Percentage of classic bikes used in each month and week-day") +
        facet_wrap(~ month(`month(START_DATE)`, label = TRUE)) +
        theme_minimal())

ggsave("figs/Line plot - Percentage of classic bikes used in each month and week-day.png")


# Line plot showing trend of use of docked bikes in each month and week day
print(bike_used_per_month_day %>% 
        ggplot(aes(x = WEEK_DAY, 
                   y = percent_docked)) + 
        geom_line() + 
        ggtitle(label = "Percentage of docked bikes used in each month and week-day") +
        facet_wrap(~ month(`month(START_DATE)`, label = TRUE)) +
        theme_minimal())

ggsave("figs/Line plot - Percentage of docked bikes used in each month and week-day.png")


# Line plot showing trend of use of electric bikes in each month and week day
print(bike_used_per_month_day %>% 
        ggplot(aes(x = WEEK_DAY, 
                   y = percent_electric)) +
        geom_line() + 
        ggtitle(label = "Percentage of electric bikes used in each month and week-day") +
        facet_wrap(~ month(`month(START_DATE)`, label = TRUE)) +
        theme_minimal())

ggsave("figs/Line plot - Percentage of electric bikes used in each month and week-day.png")



## Task 3: Exploring Numerical data

### Descriptive Statistical Analysis
# Here, we group by USERTYPE, and calculate the mean, maximum value, 
# minimum_value, median, standard_deviation, mode, and IQR of RIDE_LENGTH 
# feature

# Create the mode function}
mode <- function(v, na.rm = TRUE) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Summary: Relationship between USERTYPE and RIDE_LENGTH
summary_user_ridelengths <- full_dataset %>%
  group_by(USERTYPE) %>%
  summarize(count = n(), 
            mean = mean(RIDE_LENGTH, na.rm = TRUE),
            std_dev = sd(RIDE_LENGTH, na.rm = TRUE), 
            min = min(RIDE_LENGTH, na.rm = TRUE), 
            median = median(RIDE_LENGTH, na.rm=TRUE),
            mode = mode(RIDE_LENGTH, na.rm= TRUE),
            iqr = IQR(RIDE_LENGTH, na.rm = TRUE), 
            max = max(RIDE_LENGTH, na.rm = TRUE))

print(summary_user_ridelengths)



### Average ride_length for members and casual riders
# Here, we visualize the average ride_length for members and casual riders

# Bar plot showing ride length for each usertype
print(summary_user_ridelengths %>% 
        ggplot(aes(x= USERTYPE, y=as.numeric(mean), fill=USERTYPE)) + 
        geom_col() + 
        labs(title = "Average of ride_length vs Usertype", 
             x = "Usertype", 
             y = "Avg of Ride length (mins)") +
        theme_minimal())

ggsave("figs/Bar plot - Average of ride_length vs Usertype.png")


### Average ride_length for users by Month.
# Here, we calculate and visualize the average ride_length for users by Month.

# Calculate the average ride_length for users by Month, include=FALSE
avg_ridelength_by_month <- full_dataset %>% 
  group_by(USERTYPE, month(START_DATE)) %>% 
  summarize(mean = mean(RIDE_LENGTH, na.rm = TRUE)) %>% 
  arrange(USERTYPE, -mean)

print(avg_ridelength_by_month)


# Bar plot showing ride_length for each user by month
print(avg_ridelength_by_month %>% 
        ggplot(aes(x = month(`month(START_DATE)`, label = TRUE), 
                   y = mean, 
                   fill = USERTYPE)) + 
        geom_col() +
        labs(title = "Average Ride Length for Users by Month",
             x = "Month", 
             y = "Average Ride Length") +
        theme_minimal())

ggsave("figs/Bar plot - Average Ride Length for Users by Month.png")


### Average ride_length for users by day_of_week
# Here, we calculate and visualize the average ride_length for users by 
# Day of Week

# Calculate the average ride_length for users by day_of_week
avg_ridelength_by_wday <- full_dataset %>% 
  group_by(USERTYPE, WEEK_DAY) %>% 
  summarize(mean = mean(RIDE_LENGTH, na.rm = TRUE)) %>% 
  arrange(USERTYPE, -mean)

print(avg_ridelength_by_wday)


# Bar plot showing ride length for each user by week_day
print(avg_ridelength_by_wday %>% 
        ggplot(aes(x = wday(WEEK_DAY, label = TRUE), 
                   y = mean, 
                   fill = USERTYPE)) + 
        geom_col() + 
        labs(title = "Average Ride Length for Users by Week_Day",
             x = "Week Day", y = "Average Ride Length") +
        theme_minimal())

ggsave("figs/Bar plot - Average Ride Length for Users by Week_Day.png")



# Now, let's see if there is a relationship between type of rides and 
# ride length

### Relationship between type of rides and ride length
# Here, we calculate and visualize the average ride_length (mins) and observe how they differ between different "Rideable_type"

# Calculating average ride_length by rideable_type
avg_ridelength_by_ridetype <- full_dataset %>% 
  group_by(RIDEABLE_TYPE) %>% 
  summarize(AVG_RIDE_LENGTH = mean(as.numeric(RIDE_LENGTH), na.rm = T))

# Bar plot showing Average Ride-length for each bike type
print(avg_ridelength_by_ridetype %>% 
        ggplot(aes(x = RIDEABLE_TYPE, y = AVG_RIDE_LENGTH)) +
        geom_bar(fill = "#00BFC4", stat = "identity") + 
        ggtitle("Average Ride_length by Ride_type") +
        theme_minimal())

ggsave("figs/Bar plot - Average Ride_length by Ride_type.png")


# Bar Plot showing Average Ride-length for each bike type by Month
avg_ridelength_by_month_ridetype <- full_dataset %>% 
  group_by(RIDEABLE_TYPE, month(START_DATE)) %>% 
  summarize(AVG_RIDE_LENGTH = mean(as.numeric(RIDE_LENGTH), na.rm = T))

print(avg_ridelength_by_month_ridetype %>%  
        ggplot(aes(x = month(`month(START_DATE)`, label = TRUE), 
                   y = AVG_RIDE_LENGTH, 
                   fill = RIDEABLE_TYPE)) +
        geom_col() +
        labs(title = "Ride Length for Ride_type by Month",
             x = "Week Day", y = "Average Ride Length") + 
        theme_minimal())

ggsave("figs/Bar plot - Ride Length for Ride_type by Month.png")

# Notes: Evidently, from our plot, docked_bikes are slower than 
# Classic and Electric bikes.



# Now, let's see if there is a relationship between number of rideable_type 
# and Cyclistic bike stations

### Relationship between Usertype and Rideable_type per Station

"Here, we want to know if there a relationship between count of Rideable_type 
available per station and the use of Rideable_type

For example, We already know that Casual riders patronize docked bikes more 
than Annuals. 
* Why is this the case?

* Could it be there are more docked_bike near-by stations where casuals are 
located (As one is likely to rent a bike with the bike-share station nearest 
to him or her)?

* Are annuals located closely to classic_bike and electric_bike stations;
Does that influence why thy use more of these bikes, besides their speed to 
commute to work faster?
"

#### Determine the start stations with the highest number for each rideable_type

# Create a subset containing the count rideable_type by start_stations
startstationame_by_ridetype <- full_dataset %>% 
  filter(!is.na(START_STATION_NAME)) %>% 
  group_by(RIDEABLE_TYPE, START_STATION_NAME) %>% 
  summarize(count = n()) %>% 
  arrange(-count)

# Filter for first 20 start_stations having the highest count for classic_bikes
print(head(startstationame_by_ridetype %>% 
             filter(RIDEABLE_TYPE == "classic_bike") %>%  
             mutate(START_STATION_NAME = reorder(START_STATION_NAME, count)), 20) %>% 
        ggplot(aes(x = START_STATION_NAME, y = count, fill = RIDEABLE_TYPE)) + 
        geom_col(position = "stack") + 
        coord_flip() + 
        ggtitle(label = "Highest Count of Classic bikes per Start Stations")+ 
        theme_minimal())

ggsave("figs/Column plot - Highest Count of Classic bikes per Start Stations.png")


# Filter for the first 20 start_stations having the highest count for docked_bikes
print(head(startstationame_by_ridetype %>% 
             filter(RIDEABLE_TYPE == "docked_bike") %>%  
             mutate(START_STATION_NAME = reorder(START_STATION_NAME, count)), 20) %>% 
        ggplot(aes(x = START_STATION_NAME, y = count, fill = RIDEABLE_TYPE)) + 
        geom_col(position = "stack") + 
        coord_flip() +
        ggtitle(label = "Highest Count of Docked bikes per Start Stations") + 
        theme_minimal())

ggsave("figs/Column plot - Highest Count of Docked bikes per Start Stations.png")


# Filter for the first 20 start_stations having the highest count for electric_bikes
print(head(startstationame_by_ridetype %>% 
             filter(RIDEABLE_TYPE == "electric_bike") %>%  
             mutate(START_STATION_NAME = reorder(START_STATION_NAME, count)), 20) %>% 
        ggplot(aes(x = START_STATION_NAME, y = count, fill = RIDEABLE_TYPE)) + 
        geom_col(position = "stack") + 
        coord_flip() +
        ggtitle(label = "Highest Count of Electric bikes per Start Stations") + 
        theme_minimal())

ggsave("figs/Column plot - Highest Count of Electric bikes per Start Stations.png")



# Now, let's see the relationship between number of user_type and 
# Cyclistic bike start stations

#### Determine the start stations with the highest number of casual or annual users

# Create a subset containing the count user_type by start_stations
startstationame_by_usertype <- full_dataset %>% 
  filter(!is.na(START_STATION_NAME)) %>% 
  group_by(USERTYPE, START_STATION_NAME) %>% 
  summarize(count = n()) %>% 
  arrange(-count)

# Filter for first 20 start_stations having the highest count for casuals
print(head(startstationame_by_usertype %>% 
             filter(USERTYPE == "casual") %>%  
             mutate(START_STATION_NAME = reorder(START_STATION_NAME, count)), 20) %>% 
        ggplot(aes(x = START_STATION_NAME, y = count, fill = USERTYPE)) + 
        geom_col(position = "stack") + 
        coord_flip() +
        ggtitle(label = "Highest Count of Casuals per Start Stations") + 
        theme_minimal())

ggsave("figs/Column plot - Highest Count of Casuals per Start Stations.png")


# Filter for first 20 start_stations having the highest count for Annuals
print(head(startstationame_by_usertype %>% 
             filter(USERTYPE == "member") %>%  
             mutate(START_STATION_NAME = reorder(START_STATION_NAME, count)), 20) %>% 
        ggplot(aes(x = START_STATION_NAME, y = count, fill = USERTYPE)) + 
        geom_col(position = "stack") + 
        coord_flip() + 
        ggtitle(label = "Highest Count of Annuals per Start Stations") + 
        theme_minimal())

ggsave("figs/Column plot - Highest Count of Annuals per Start Stations.png")



# Now, let's consider end stations
#### Determine the end_stations with the highest number for each rideable_type

# Create a subset containing the count rideable_type by end_stations
endstationame_by_ridetype <- full_dataset %>% 
  filter(!is.na(END_STATION_NAME)) %>% 
  group_by(RIDEABLE_TYPE, END_STATION_NAME) %>% 
  summarize(count = n()) %>% 
  arrange(-count)

# Filter for first 20 end_stations having the highest count for classic_bikes
print(head(endstationame_by_ridetype %>% 
             filter(RIDEABLE_TYPE == "classic_bike") %>%  
             mutate(END_STATION_NAME = reorder(END_STATION_NAME, count)),20) %>% 
        ggplot(aes(x = END_STATION_NAME, y = count, fill = RIDEABLE_TYPE)) + 
        geom_col(position = "stack") + 
        coord_flip() + 
        ggtitle(label = "Highest Count of Classic bikes per End Stations") + 
        theme_minimal())

ggsave("figs/Column plot - Highest Count of Classic bikes per End Stations.png")


# Filter for first 20 end_stations having the highest count for docked_bikes
print(head(endstationame_by_ridetype %>% 
             filter(RIDEABLE_TYPE == "docked_bike") %>%  
             mutate(END_STATION_NAME = reorder(END_STATION_NAME, count)), 20) %>% 
        ggplot(aes(x = END_STATION_NAME, y = count, fill = RIDEABLE_TYPE)) + 
        geom_col(position = "stack") + 
        coord_flip() + 
        ggtitle(label = "Highest Count of Docked bikes per End Stations") + 
        theme_minimal())

ggsave("figs/Column plot - Highest Count of Docked bikes per End Stations.png")


# Filter for first 20 end_stations having the highest count for electric_bikes
print(head(endstationame_by_ridetype %>% 
             filter(RIDEABLE_TYPE == "electric_bike") %>%  
             mutate(END_STATION_NAME = reorder(END_STATION_NAME, count)), 20) %>% 
        ggplot(aes(x = END_STATION_NAME, y = count, fill = RIDEABLE_TYPE)) + 
        geom_col(position = "stack") + 
        coord_flip() + 
        ggtitle(label = "Highest Count of Electric bikes per End Stations") + 
        theme_minimal())

ggsave("figs/Column plot - Highest Count of Electric bikes per End Stations.png")



# Now, let's see the relationship between number of user_type and 
# Cyclistic bike end stations

#### Determine the end stations with the highest number of casual or annual users

# Create a subset containing the count user_type by end_stations
endstationame_by_usertype <- full_dataset %>% 
  filter(!is.na(END_STATION_NAME)) %>% 
  group_by(USERTYPE, END_STATION_NAME) %>% 
  summarize(count = n()) %>% 
  arrange(-count)

# Filter for first 20 end_stations having the highest count for casuals
print(head(endstationame_by_usertype %>% 
             filter(USERTYPE == "casual") %>%  
             mutate(END_STATION_NAME = reorder(END_STATION_NAME, count)), 20) %>% 
        ggplot(aes(x = END_STATION_NAME, y = count, fill = USERTYPE)) + 
        geom_col(position = "stack") + 
        coord_flip() + 
        ggtitle(label = "Highest Count of Casuals per End Stations") + 
        theme_minimal())

ggsave("figs/Column plot - Highest Count of Casuals per End Stations.png")


# Filter for first 20 end_stations having the highest count for annuals
print(head(endstationame_by_usertype %>% 
             filter(USERTYPE == "member") %>%  
             mutate(END_STATION_NAME = reorder(END_STATION_NAME, count)), 20) %>% 
        ggplot(aes(x = END_STATION_NAME, y = count, fill = USERTYPE)) + 
        geom_col(position = "stack") + 
        coord_flip() + 
        ggtitle(label = "Highest Count of Annuals per End Stations") + 
        theme_minimal())

ggsave("figs/Column plot - Highest Count of Annuals per End Stations.png")


# Notes: 
# In general, while Casuals are located closely at areas in Chicago where
# there classic and electric bike stations, a large number of them are found
# patronizing station where there are high counts of docked bikes.

# Annuals are located closely at areas in Chicago where there are more
# classic and electric bikes stations, and little to no docked bikes
# station.

# Hence, this explain one reason why Casuals patronize the use of docked
# bikes than Annuals.