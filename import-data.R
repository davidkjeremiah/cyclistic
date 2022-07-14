#########################################
# Importing and Basic Exploration of data
#########################################

# Load the needed libraries
library(tidyverse)
library(skimr)
library(ggplot2)
library(assertive)
library(ggthemes)
library(lubridate)

# Importing file
# Sample Cyclistic Data: 202101
file_path <- paste(getwd(), sep ="", c("/data/202101-divvy-tripdata.csv"))

# Confirm that file exist in current working directory
print(file.exists(file_path))

# inspect the first 3 lines
print(read_lines(file_path, n_max = 3))

# read file in CSV format
sub_cylistic_data <- read_csv(file_path)

## Inspect data
head(sub_cylistic_data)
tail(sub_cylistic_data)

# Get information about it's structure and organization
# Check the structure of the data
str(sub_cylistic_data) 
glimpse(sub_cylistic_data) #data.frame: 96,834 obs. of 13 variables

# Get a comprehensive summary of the data frame
skim_without_charts(sub_cylistic_data)


# Inspecting levels for rideable_type and usertype
# Check levels for rideable_type
print(levels(factor(sub_cylistic_data$rideable_type)))

# Check levels for usertype
print(levels(factor(sub_cylistic_data$usertype)))