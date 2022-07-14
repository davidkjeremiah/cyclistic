########################## 
# Data Cleaning with dplyr
##########################

#  TASK 1: Standardize column names for all collected datasets
#  Task 2: Range Constraints
#  Task 3: Checking for Duplicate Constraints
#  Task 4: Checking for Membership constraints
#  Task 5: Checking for Completeness: Dealing with Missing Values
#  Task 6: Dealing with partial duplicate found in divvy_tripdata_1
#  Task 7: Format Dataset
#  Task 8: Overwrite divvy_tripdata_1 - divvy_tripdata_12
#  Task 9: Select the columns we are interested in, in this project
#  Task 10: Unite the dataset into one full dataset for further analysis

# Create a list containing sample Cyclist Data from Jan 2021 - Dec 2021
cyclistic_dataset_list <- paste(getwd(), 
                                sep ="", 
                                c("/data/202101-divvy-tripdata.csv",
                                  "/data/202102-divvy-tripdata.csv",
                                  "/data/202103-divvy-tripdata.csv",
                                  "/data/202104-divvy-tripdata.csv",
                                  "/data/202105-divvy-tripdata.csv",
                                  "/data/202106-divvy-tripdata.csv",
                                  "/data/202107-divvy-tripdata.csv",
                                  "/data/202108-divvy-tripdata.csv",
                                  "/data/202109-divvy-tripdata.csv",
                                  "/data/202110-divvy-tripdata.csv",
                                  "/data/202111-divvy-tripdata.csv",
                                  "/data/202112-divvy-tripdata.csv"))

# Task 1: Standardize column names for all collected datasets
# To improve dataset readbility we use the following naming convention:
# -> Column names as UPPERCASE
# -> The word separator needs to be an underscore, such as in `COLUMN_NAME`

# Write a for loop to iterate over the above list of datasets and 
# convert their column names
for (cyclistic_ds_name in cyclistic_dataset_list){
  
  # Read dataset
  dataset <- read_csv(cyclistic_ds_name)
  
  # Convert all columns to uppercase
  names(dataset) <- toupper(names(dataset))
  
  # Save the dataset
  write_csv(dataset, file = cyclistic_ds_name)
}


# Read the resulting datasets back and 
# check whether their column names follow the naming convention
for (cyclistic_ds_name in cyclistic_dataset_list){
  
  #Read dataset
  dataset <- read_csv(cyclistic_ds_name)
  
  # Print a summary for each data set to 
  # check whether the column names were correctly converted
  print(summary(dataset))
}
"
Result:
  All columns, for each dataset, have been standardized to uppercase
"


# Task 2: Range Constraints
# Write a for loop to iterate over the above datasets and 
# check for range constraints for the start_lat and end_lat: -90 to 90, start_lng and end_lng : -180 to 180 
for (cyclistic_ds_name in cyclistic_dataset_list){
  
  # Read dataset
  dataset <- read_csv(cyclistic_ds_name)
  
  # Check for Range Constraints:
  
  # for lat
  print(assert_all_are_in_closed_range(dataset$START_LAT, -90, 90, na_ignore = TRUE))
  print(assert_all_are_in_closed_range(dataset$END_LAT, -90, 90, na_ignore = TRUE))
  
  # for lng
  print(assert_all_are_in_closed_range(dataset$START_LNG, -90, 90, na_ignore = TRUE))
  print(assert_all_are_in_closed_range(dataset$END_LNG, -90, 90, na_ignore = TRUE))
}

"
Result:
  There are no range constraints
"                                    


# Task 3: Checking for Duplicate Constraints:
for (cyclistic_ds_name in cyclistic_dataset_list){
  
  # Read dataset
  dataset <- read_csv(cyclistic_ds_name)
  
  # Check for full duplicates
  print(sum(duplicated(dataset))) #No full duplicates
  
  # Check for partial duplicates
  print(dataset %>% count(RIDE_ID) %>% filter(n > 1)) 
}  

"
Result:
  Every other dataset came out clean with 0 partial and full-duplicates
  
  However, there is partial duplicate in the first dataset: 202101-divvy-tripdata.csv
  RIDE_ID      n
  <chr>    <int>
1 1.44E+15     2
2 4.48E+15     2
3 7.45E+15     2
  We'll need to inspect it.
"


# Task 4: Checking for Membership constraints:
for (cyclistic_ds_name in cyclistic_dataset_list){
  
  # Read dataset
  dataset <- read_csv(cyclistic_ds_name)
  
  # checking for membership constraints in usertype: casual and member
  print(unique(dataset$USERTYPE) %in% c("member", "casual"))
  
  # checking for membership constraint in rideable_type: 
  # "electric_bike" "classic_bike"  "docked_bike"
  print(unique(dataset$RIDEABLE_TYPE) %in% c("electric_bike","classic_bike","docked_bike"))
}

"
Result:
  There are no membership constraints
"

# Task 5: Checking for Completeness
cyclistic_missing_list <- c()
for (cyclistic_ds_name in cyclistic_dataset_list){
  
  # Read dataset
  dataset <- read_csv(cyclistic_ds_name)
  
  # checking for missing data
  # Using anyNA()
  output <- anyNA(dataset)
  
  if (output == TRUE) {
    # add missing data to cyclistic_missing_list
    cyclistic_missing_list[cyclistic_ds_name] <- sum(is.na(dataset))} else{
      print("There is no missing data")
    }
}

# Identifying the number of missing data in each datasets
print(cyclistic_missing_list)
"
Result:
  From the result, there are missing data in each dataset,
  ranging from 10000 to 500000. 
  
  This needs to be handled.
"

# Identifying columns with missing data for each dataset (2021)
# reading each dataset and assigning them to individual variables
divvy_tripdata_1 <- read_csv(paste(getwd(), sep ="", c("/data/202101-divvy-tripdata.csv")))
divvy_tripdata_2 <- read_csv(paste(getwd(), sep ="", c("/data/202102-divvy-tripdata.csv")))
divvy_tripdata_3 <- read_csv(paste(getwd(), sep ="", c("/data/202103-divvy-tripdata.csv")))
divvy_tripdata_4 <- read_csv(paste(getwd(), sep ="", c("/data/202104-divvy-tripdata.csv")))
divvy_tripdata_5 <- read_csv(paste(getwd(), sep ="", c("/data/202105-divvy-tripdata.csv")))
divvy_tripdata_6 <- read_csv(paste(getwd(), sep ="", c("/data/202106-divvy-tripdata.csv")))
divvy_tripdata_7 <- read_csv(paste(getwd(), sep ="", c("/data/202107-divvy-tripdata.csv")))
divvy_tripdata_8 <- read_csv(paste(getwd(), sep ="", c("/data/202108-divvy-tripdata.csv")))
divvy_tripdata_9 <- read_csv(paste(getwd(), sep ="", c("/data/202109-divvy-tripdata.csv")))
divvy_tripdata_10 <- read_csv(paste(getwd(), sep ="", c("/data/202110-divvy-tripdata.csv")))
divvy_tripdata_11 <- read_csv(paste(getwd(), sep ="", c("/data/202111-divvy-tripdata.csv")))
divvy_tripdata_12 <- read_csv(paste(getwd(), sep ="", c("/data/202112-divvy-tripdata.csv")))

# Inspecting each dataset  
# Inspect divvy_tripdata_1 up until divvy_tripdata_12
print(skim_without_charts(divvy_tripdata_1)) 
print(skim_without_charts(divvy_tripdata_2)) 
print(skim_without_charts(divvy_tripdata_3)) 
print(skim_without_charts(divvy_tripdata_4)) 
print(skim_without_charts(divvy_tripdata_5)) 
print(skim_without_charts(divvy_tripdata_6)) 
print(skim_without_charts(divvy_tripdata_7)) 
print(skim_without_charts(divvy_tripdata_8)) 
print(skim_without_charts(divvy_tripdata_9)) 
print(skim_without_charts(divvy_tripdata_10)) 
print(skim_without_charts(divvy_tripdata_11)) 
print(skim_without_charts(divvy_tripdata_12)) 

"
Result:
 For each dataset, the missing values are found with
 start_station_name, start_station_id, end_station_name and 
 end_station_id - categorical variables.
 
 For numeric variables, it's with end_lat and end_lng. 
 All of these issues need to be handled properly.
 
 Now that some basic ideas about how to process this bike-sharing dataset 
 is clear, let's start working on it!
"

# Dealing with Missing Values
"
Notes:
  Given that we won't be using start_station_id, end_station_id, 
  end_lat, and end_lng for our case study, we'll simply ignore their 
  missing values.
  However, if we were using numeric variables (end_lat and end_lng), 
  since they are of similar range (-90, 90), we can replace missing values 
  for each of them by their mean.
  
  For start_station_name, and end_station_name, finding their is likely
  to introduce some form of bias to the dataset, considering there is a lot of
  missing values. So, we'll leave the missing values as it is.
"

# Task 6: Dealing with partial duplicate found in divvy_tripdata_1
# i. Re-affirm we have partial duplicate in the dataset
print(divvy_tripdata_1 %>% count(RIDE_ID) %>% filter(n > 1))
"
Confirmed:
  RIDE_ID      n
  <chr>    <int>
1 1.44E+15     2
2 4.48E+15     2
3 7.45E+15     2
"

# ii. Inspect dataset for duplicates
print(divvy_tripdata_1 %>% 
        filter(RIDE_ID %in% c('1.44E+15', '4.48E+15', '7.45E+15')) %>%  
        arrange(RIDE_ID))
"
Result:
  While the following ride_ids appear more than once, 
  each observation is distinct. Hence, we leave data as it is.
"

# Task 7: Formatting our Dataset
for (cyclistic_ds_name in cyclistic_dataset_list){
  
  # Read dataset
  dataset <- read_csv(cyclistic_ds_name)
  
  # Change the started_at and ended_at column from character field to a Datetime format
  # using the lubridate library.
  dataset <- dataset %>% 
    mutate(STARTED_AT  = mdy_hm(STARTED_AT),
           ENDED_AT = mdy_hm(ENDED_AT))
  
  # Create a column called "RIDE_LENGTH" that Calculates the length of each ride in mins
  dataset <- dataset %>% 
    mutate(RIDE_LENGTH = difftime(ENDED_AT, STARTED_AT, units = "mins"))
  
  # Create a column called "WEEK_DAY," and calculate the day of the week that each ride started
  # Note: 1 = Sunday and 7 = Saturday
  dataset <- dataset %>% 
    mutate(WEEK_DAY = wday(STARTED_AT))
  
  # Parse started_at and ended_at column into start_date, start_time, end_date and end_time
  dataset <- dataset %>% 
    separate(STARTED_AT, into = c('START_DATE', 'START_TIME'), sep = " ") %>%
    separate(ENDED_AT, into = c('END_DATE', 'END_TIME'), sep = " ") %>% 
    mutate(START_DATE = parse_date(START_DATE), 
           END_DATE = parse_date(END_DATE),
           START_TIME = parse_time(START_TIME), 
           END_TIME = parse_time(END_TIME))
  
  # Save the dataset
  write_csv(dataset, file = cyclistic_ds_name)
}  


# Task 8: overwrite divvy_tripdata_1 - divvy_tripdata_12
divvy_tripdata_1 <- read_csv(paste(getwd(), sep ="", c("/data/202101-divvy-tripdata.csv")))
divvy_tripdata_2 <- read_csv(paste(getwd(), sep ="", c("/data/202102-divvy-tripdata.csv")))
divvy_tripdata_3 <- read_csv(paste(getwd(), sep ="", c("/data/202103-divvy-tripdata.csv")))
divvy_tripdata_4 <- read_csv(paste(getwd(), sep ="", c("/data/202104-divvy-tripdata.csv")))
divvy_tripdata_5 <- read_csv(paste(getwd(), sep ="", c("/data/202105-divvy-tripdata.csv")))
divvy_tripdata_6 <- read_csv(paste(getwd(), sep ="", c("/data/202106-divvy-tripdata.csv")))
divvy_tripdata_7 <- read_csv(paste(getwd(), sep ="", c("/data/202107-divvy-tripdata.csv")))
divvy_tripdata_8 <- read_csv(paste(getwd(), sep ="", c("/data/202108-divvy-tripdata.csv")))
divvy_tripdata_9 <- read_csv(paste(getwd(), sep ="", c("/data/202109-divvy-tripdata.csv")))
divvy_tripdata_10 <- read_csv(paste(getwd(), sep ="", c("/data/202110-divvy-tripdata.csv")))
divvy_tripdata_11 <- read_csv(paste(getwd(), sep ="", c("/data/202111-divvy-tripdata.csv")))
divvy_tripdata_12 <- read_csv(paste(getwd(), sep ="", c("/data/202112-divvy-tripdata.csv")))

# Task 9: Select the columns we are interested in, in this project
"
The following are the variables we need for our casestudy:
RIDE_ID
RIDEABLE_TYPE
USERTYPE
START_DATE
END_DATE
sTART_TIME
END_TIME
START_STATION_NAME
END_STATION_NAME
RIDE_LENGTH
WEEK_DAY
"

# For divvy_tripdata_1
divvy_tripdata_1 <- divvy_tripdata_1 %>% select(RIDE_ID, 
                                                USERTYPE,
                                                RIDEABLE_TYPE,
                                                START_DATE,
                                                END_DATE,
                                                START_TIME,
                                                END_TIME,
                                                START_STATION_NAME,
                                                END_STATION_NAME,
                                                RIDE_LENGTH,
                                                WEEK_DAY)

# For divvy_tripdata_2
divvy_tripdata_2 <- divvy_tripdata_2 %>% select(RIDE_ID, 
                                                USERTYPE,
                                                RIDEABLE_TYPE,
                                                START_DATE,
                                                END_DATE,
                                                START_TIME,
                                                END_TIME,
                                                START_STATION_NAME,
                                                END_STATION_NAME,
                                                RIDE_LENGTH,
                                                WEEK_DAY)

# For divvy_tripdata_3
divvy_tripdata_3 <- divvy_tripdata_3 %>% select(RIDE_ID, 
                                                USERTYPE,
                                                RIDEABLE_TYPE,
                                                START_DATE,
                                                END_DATE,
                                                START_TIME,
                                                END_TIME,
                                                START_STATION_NAME,
                                                END_STATION_NAME,
                                                RIDE_LENGTH,
                                                WEEK_DAY)

# For divvy_tripdata_4
divvy_tripdata_4 <- divvy_tripdata_4 %>% select(RIDE_ID, 
                                                USERTYPE,
                                                RIDEABLE_TYPE,
                                                START_DATE,
                                                END_DATE,
                                                START_TIME,
                                                END_TIME,
                                                START_STATION_NAME,
                                                END_STATION_NAME,
                                                RIDE_LENGTH,
                                                WEEK_DAY)

# For divvy_tripdata_5
divvy_tripdata_5 <- divvy_tripdata_5 %>% select(RIDE_ID, 
                                                USERTYPE,
                                                RIDEABLE_TYPE,
                                                START_DATE,
                                                END_DATE,
                                                START_TIME,
                                                END_TIME,
                                                START_STATION_NAME,
                                                END_STATION_NAME,
                                                RIDE_LENGTH,
                                                WEEK_DAY)

# For divvy_tripdata_6
divvy_tripdata_6 <- divvy_tripdata_6 %>% select(RIDE_ID, 
                                                USERTYPE,
                                                RIDEABLE_TYPE,
                                                START_DATE,
                                                END_DATE,
                                                START_TIME,
                                                END_TIME,
                                                START_STATION_NAME,
                                                END_STATION_NAME,
                                                RIDE_LENGTH,
                                                WEEK_DAY)

# For divvy_tripdata_7
divvy_tripdata_7 <- divvy_tripdata_7 %>% select(RIDE_ID, 
                                                USERTYPE,
                                                RIDEABLE_TYPE,
                                                START_DATE,
                                                END_DATE,
                                                START_TIME,
                                                END_TIME,
                                                START_STATION_NAME,
                                                END_STATION_NAME,
                                                RIDE_LENGTH,
                                                WEEK_DAY)

# For divvy_tripdata_8
divvy_tripdata_8 <- divvy_tripdata_8 %>% select(RIDE_ID, 
                                                USERTYPE,
                                                RIDEABLE_TYPE,
                                                START_DATE,
                                                END_DATE,
                                                START_TIME,
                                                END_TIME,
                                                START_STATION_NAME,
                                                END_STATION_NAME,
                                                RIDE_LENGTH,
                                                WEEK_DAY)

# For divvy_tripdata_9
divvy_tripdata_9 <- divvy_tripdata_9 %>% select(RIDE_ID, 
                                                USERTYPE,
                                                RIDEABLE_TYPE,
                                                START_DATE,
                                                END_DATE,
                                                START_TIME,
                                                END_TIME,
                                                START_STATION_NAME,
                                                END_STATION_NAME,
                                                RIDE_LENGTH,
                                                WEEK_DAY)

# For divvy_tripdata_10
divvy_tripdata_10 <- divvy_tripdata_10 %>% select(RIDE_ID, 
                                                  USERTYPE,
                                                  RIDEABLE_TYPE,
                                                  START_DATE,
                                                  END_DATE,
                                                  START_TIME,
                                                  END_TIME,
                                                  START_STATION_NAME,
                                                  END_STATION_NAME,
                                                  RIDE_LENGTH,
                                                  WEEK_DAY)

# For divvy_tripdata_11
divvy_tripdata_11 <- divvy_tripdata_11 %>% select(RIDE_ID, 
                                                  USERTYPE,
                                                  RIDEABLE_TYPE,
                                                  START_DATE,
                                                  END_DATE,
                                                  START_TIME,
                                                  END_TIME,
                                                  START_STATION_NAME,
                                                  END_STATION_NAME,
                                                  RIDE_LENGTH,
                                                  WEEK_DAY)

# For divvy_tripdata_12
divvy_tripdata_12 <- divvy_tripdata_12 %>% select(RIDE_ID, 
                                                  USERTYPE,
                                                  RIDEABLE_TYPE,
                                                  START_DATE,
                                                  END_DATE,
                                                  START_TIME,
                                                  END_TIME,
                                                  START_STATION_NAME,
                                                  END_STATION_NAME,
                                                  RIDE_LENGTH,
                                                  WEEK_DAY)


# Task 10: Unite the dataset into one full dataset for further analysis
full_dataset <- rbind(divvy_tripdata_1, divvy_tripdata_2, divvy_tripdata_3, 
                      divvy_tripdata_4, divvy_tripdata_5, divvy_tripdata_6,
                      divvy_tripdata_7, divvy_tripdata_8, divvy_tripdata_9,
                      divvy_tripdata_10, divvy_tripdata_11, divvy_tripdata_12)

# Task 11: Save derived dataset as cyclistic_clean.rda in the rda directory
save(full_dataset, file = "rda/cyclistic_clean.rda")
