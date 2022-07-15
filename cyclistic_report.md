Cyclistic Bikeshare Project
================
David Jeremiah
2022-07-12

## Background for Use Case

This is a report on how casual riders and annual members use Cyclistic
bikes differently - a fictional bike-share company in Chicago. The
director of marketing, Moreno, believes the company’s future success
depends on maximizing the number of annual memberships.

From understanding how casual riders and annual members vary in their
use of Cyclistic bikes, the marketing team can design a new marketing
strategy to convert casual riders into annual members.

The original data was obtained from:
[divvy-tripdata](https://divvy-tripdata.s3.amazonaws.com/index.html)

## Step 1: Setting up my environment

Notes: setting up my R environment by loading the needed packages

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.8
    ## v tidyr   1.2.0     v stringr 1.4.0
    ## v readr   2.1.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(skimr)
library(ggplot2)
library(assertive)
```

    ## 
    ## Attaching package: 'assertive'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     is_atomic, is_character, is_double, is_empty, is_formula,
    ##     is_function, is_integer, is_list, is_logical, is_null, is_numeric,
    ##     is_vector

    ## The following object is masked from 'package:tibble':
    ## 
    ##     has_rownames

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

## Step 2: Importing and Viewing file

Notes: Using a sample from the entire file: *202101-divvy-tripdata*, for
inspection

``` r
print(head(sub_cylistic_data))
```

    ## # A tibble: 6 x 13
    ##   ride_id rideable_type started_at          ended_at            start_station_n~
    ##   <chr>   <chr>         <dttm>              <dttm>              <chr>           
    ## 1 E19E6F~ electric_bike 2021-01-23 16:14:19 2021-01-23 16:24:44 California Ave ~
    ## 2 DC88F2~ electric_bike 2021-01-27 18:43:08 2021-01-27 18:47:12 California Ave ~
    ## 3 EC45C9~ electric_bike 2021-01-21 22:35:54 2021-01-21 22:37:14 California Ave ~
    ## 4 4FA453~ electric_bike 2021-01-07 13:31:13 2021-01-07 13:42:55 California Ave ~
    ## 5 BE5E8E~ electric_bike 2021-01-23 02:24:02 2021-01-23 02:24:45 California Ave ~
    ## 6 5D8969~ electric_bike 2021-01-09 14:24:07 2021-01-09 15:17:54 California Ave ~
    ## # ... with 8 more variables: start_station_id <chr>, end_station_name <chr>,
    ## #   end_station_id <chr>, start_lat <dbl>, start_lng <dbl>, end_lat <dbl>,
    ## #   end_lng <dbl>, member_casual <chr>

``` r
print(skim_without_charts(sub_cylistic_data))
```

    ## -- Data Summary ------------------------
    ##                            Values           
    ## Name                       sub_cylistic_data
    ## Number of rows             96834            
    ## Number of columns          13               
    ## _______________________                     
    ## Column type frequency:                      
    ##   character                7                
    ##   numeric                  4                
    ##   POSIXct                  2                
    ## ________________________                    
    ## Group variables            None             
    ## 
    ## -- Variable type: character ----------------------------------------------------
    ##   skim_variable      n_missing complete_rate min max empty n_unique whitespace
    ## 1 ride_id                    0         1      16  16     0    96834          0
    ## 2 rideable_type              0         1      11  13     0        3          0
    ## 3 start_station_name      8625         0.911  10  51     0      640          0
    ## 4 start_station_id        8625         0.911   3  35     0      638          0
    ## 5 end_station_name       10277         0.894  10  53     0      632          0
    ## 6 end_station_id         10277         0.894   3  35     0      629          0
    ## 7 member_casual              0         1       6   6     0        2          0
    ## 
    ## -- Variable type: numeric ------------------------------------------------------
    ##   skim_variable n_missing complete_rate  mean     sd    p0   p25   p50   p75
    ## 1 start_lat             0         1      41.9 0.0471  41.6  41.9  41.9  41.9
    ## 2 start_lng             0         1     -87.6 0.0271 -87.8 -87.7 -87.6 -87.6
    ## 3 end_lat             103         0.999  41.9 0.0471  41.6  41.9  41.9  41.9
    ## 4 end_lng             103         0.999 -87.6 0.0272 -87.8 -87.7 -87.6 -87.6
    ##    p100
    ## 1  42.1
    ## 2 -87.5
    ## 3  42.1
    ## 4 -87.5
    ## 
    ## -- Variable type: POSIXct ------------------------------------------------------
    ##   skim_variable n_missing complete_rate min                 max                
    ## 1 started_at            0             1 2021-01-01 00:02:05 2021-01-31 23:57:00
    ## 2 ended_at              0             1 2021-01-01 00:08:39 2021-02-01 15:33:15
    ##   median              n_unique
    ## 1 2021-01-15 06:05:04    93736
    ## 2 2021-01-15 06:19:58    93582

    ## $character
    ## 
    ## -- Variable type: character ----------------------------------------------------
    ##   skim_variable      n_missing complete_rate min max empty n_unique whitespace
    ## 1 ride_id                    0         1      16  16     0    96834          0
    ## 2 rideable_type              0         1      11  13     0        3          0
    ## 3 start_station_name      8625         0.911  10  51     0      640          0
    ## 4 start_station_id        8625         0.911   3  35     0      638          0
    ## 5 end_station_name       10277         0.894  10  53     0      632          0
    ## 6 end_station_id         10277         0.894   3  35     0      629          0
    ## 7 member_casual              0         1       6   6     0        2          0
    ## 
    ## $numeric
    ## 
    ## -- Variable type: numeric ------------------------------------------------------
    ##   skim_variable n_missing complete_rate  mean     sd    p0   p25   p50   p75
    ## 1 start_lat             0         1      41.9 0.0471  41.6  41.9  41.9  41.9
    ## 2 start_lng             0         1     -87.6 0.0271 -87.8 -87.7 -87.6 -87.6
    ## 3 end_lat             103         0.999  41.9 0.0471  41.6  41.9  41.9  41.9
    ## 4 end_lng             103         0.999 -87.6 0.0272 -87.8 -87.7 -87.6 -87.6
    ## # ... with 1 more variable: p100 <dbl>
    ## 
    ## $POSIXct
    ## 
    ## -- Variable type: POSIXct ------------------------------------------------------
    ##   skim_variable n_missing complete_rate min                 max                
    ## 1 started_at            0             1 2021-01-01 00:02:05 2021-01-31 23:57:00
    ## 2 ended_at              0             1 2021-01-01 00:08:39 2021-02-01 15:33:15
    ## # ... with 2 more variables: median <dttm>, n_unique <int>

Notes:

1.  Our data is made up majorly of two variable types: *character* and
    *numeric*.

2.  Considering the n_unique summary of the character variables, it
    seems rideable_type and user_type are best as factors.

3.  We learn that there are infact, three possible rideable_types:
    ‘classic_bike’, ‘docked_bike’, and ‘electric_bike’. And for
    usertype, there are infact two possible Cyclistic user type:
    ‘casual’, and ‘member’.

4.  There are missing values in our sample dataset. This is likely to be
    the same for the remain years: *202102 - 202112 of divvy_trip_data*

5.  started_at and ended_at variables are declared as ‘character’
    variable, instead of DATETIME.

So, there are few issues in our data in need of wrangling.

## Step 3: Import complete wrangled dataset

Notes: spent some time working with and carrying out Data wrangling
techniques on the individual spreadsheets. Then, imported each dataset
and assigned them to individual variables: *divvy_tripdata_1 -
divvy_tripdata_12*

## Step 4: Select needed columns for project

Here, we select the needed features for our project.

Notes: The following are the features we need for our casestudy:

-   RIDE_ID
-   RIDEABLE_TYPE
-   USERTYPE
-   START_DATE
-   END_DATE
-   START_TIME
-   END_TIME
-   START_STATION_NAME
-   END_STATION_NAME
-   RIDE_LENGTH
-   WEEK_DAY

## Step 5: Merge Individual Spreadsheets into Full-year view

Here, we merge the wrangled individual spreadsheets into a full-year
view for further analysis.

``` r
full_dataset <- rbind(divvy_tripdata_1, divvy_tripdata_2, divvy_tripdata_3, 
                      divvy_tripdata_4, divvy_tripdata_5, divvy_tripdata_6,
                      divvy_tripdata_7, divvy_tripdata_8, divvy_tripdata_9,
                      divvy_tripdata_10, divvy_tripdata_11, divvy_tripdata_12)
```

## Step 6i: Exploring Categorical data: USERTYPE count

Here, we make a count of the number of Casual riders against Annual
members

``` r
options(digits = 3)
perc_usertype <- full_dataset %>% 
  summarize(percent_casual = mean(USERTYPE == "casual"),
            percent_annual = mean(USERTYPE == "member"))

print(sum(table(full_dataset$USERTYPE)))
```

    ## [1] 5595063

``` r
print(perc_usertype)
```

    ## # A tibble: 1 x 2
    ##   percent_casual percent_annual
    ##            <dbl>          <dbl>
    ## 1          0.452          0.548

Notes: From the table, we see that Cyclistic bike_share company have a
total of 5,595,063 users for the year, 2021, with more of annual members
than casuals. Where 54.8 % are Annuals, 45.2% are Casuals.

The goal of Moreno, the director of marketing at Cyclistic, is to
convert these **45.2%** to annuals.

## Visualizations

Here we would go through a series of visualizations

### Percent_annual per Month

Here, we visualize percentage of annual members for each month

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](cyclistic_report_files/figure-gfm/Line%20plot%20visualizing%20percentage%20of%20annual%20members%20for%20each%20month-1.png)<!-- -->

### Percent_casual per Month

Here, we visualize percentage of casual members for each month

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](cyclistic_report_files/figure-gfm/Line%20plot%20visualizing%20percentage%20of%20casual%20members%20for%20each%20month-1.png)<!-- -->

### Proportion of Annuals and Casuals per Month

Here, we visualize percentage of annual and casual members for each
month

![](cyclistic_report_files/figure-gfm/Bar%20plot%20showing%20proportions%20of%20Annuals%20and%20Casuals%20per%20month-1.png)<!-- -->

Notes:

-   From our result, we see that between September and May (i.e during
    autumn, winter, and spring seasons), Cyclistic bike-share system has
    highest percentage (\> 50%) - i.e. highest number - of Annual
    members than Casuals.

-   We also see that the highest number of annuals, are in the month of
    January. With the lowest being in the month of July. For Casuals, it
    was vice versa.

Next, let’s compare the proportions of Annuals and Casuals by week_day.

Note: weeks days count from Sunday (1) - Saturday (7)

### Percent_annual per Week day

Here, we visualize percentage of annual members for each Week Day

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](cyclistic_report_files/figure-gfm/Line%20plot%20visualizing%20percentage%20of%20annual%20members%20for%20each%20week_day-1.png)<!-- -->

### Percent_casual per Week day

Here, we visualize percentage of casual members for each Week Day

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](cyclistic_report_files/figure-gfm/Line%20plot%20visualizing%20percentage%20of%20casual%20members%20for%20each%20week_day-1.png)<!-- -->

### Proportion of Annuals and Casuals per Week Day

Here, we visualize percentage of annual and casual members for each
week_day

![](cyclistic_report_files/figure-gfm/Bar%20plot%20showing%20proportions%20of%20Annuals%20and%20Casuals%20per%20week_day-1.png)<!-- -->

Notes:

From our result, we see that for each month, during the weekends
(Sundays and Saturdays), Casuals patronize Cyclistic more (\> 50%).
Annuals, on the other hand, patronize more during the business days:
Monday - Friday (\> 50%).

    This suggests that most annual members use Cylistic bikes to commute to
    work. Their peak patronage days being Tuesdays and Wednesdays. While most
    Casual use Cylistic bikes mostly for leisure or recreational activities,
    with peak patronage occurring in the weekends.

    Furthermore, this goes on to highlight the reason why Annuals are highest
    in January, as shown by our earliest plot of "Proportions of Cyclistic
    Usertype per Month." This is possibly due to the start of a New year, with
    majority of workers back from the holiday vacation, plus new hiring by
    companies taking place.

    For Casuals high number in July. This is probably due to the summer
    vacation (as one factor), with students typically finishing the school
    year between late-May and late-June, and starting the new year between
    early-August and early-September. 

    Perhaps, the number of Casual patronage of Cyclistic bikes during summer
    seasons are by students on break and mostly for leisure.

Next, let’s compare the proportions of Annuals and Casuals by both month
and week_day

### Percent_annual for the Months and Days

Here, we visualize percentage of annual members for each Month and Day.

![](cyclistic_report_files/figure-gfm/Line%20plot%20showing%20trend%20for%20EACH%20month%20and%20week-day%20for%20percent_annual-1.png)<!-- -->

### Percent_casual per the Months and Days

Here, we visualize percentage of casual members for each Month and Day

![](cyclistic_report_files/figure-gfm/Line%20plot%20visualizing%20percentage%20of%20casual%20members%20for%20each%20Month%20and%20week_day-1.png)<!-- -->

Notes:

-   Generally, for casuals, they patronize Cyclistic bikes more, in all
    the months of the year, during weekends than on working days. And
    this is at its peak as they approach Summer: June, July, and August.

-   For Annuals, they patronize Cyclistic bikes more in all the months
    during working days than on weekends. And this dwindles as they
    approach summer but picks up during Winter, Spring and Autumn
    seasons.

## Step 6ii: Exploring Categorical data: USERTYPE vs RIDEABLE_TYPE

Here, we check the types of bikes commonly used by Casual or Annual
members

``` r
tb_cnt_ut_rt <- table(full_dataset$RIDEABLE_TYPE, full_dataset$USERTYPE)
print(prop.table(tb_cnt_ut_rt, 1))
```

    ##                
    ##                   casual   member
    ##   classic_bike  3.90e-01 6.10e-01
    ##   docked_bike   1.00e+00 3.20e-06
    ##   electric_bike 4.68e-01 5.32e-01

## Visualizations

Here we would go through a series of visualizations

![](cyclistic_report_files/figure-gfm/Checking%20for%20common%20Cyclistic%20Bike_types%20used%20in%20February-1.png)<!-- -->

Notes: We already know that there are infact three possible rideable
types: **‘classic_bike’**, **‘docked_bike’**, and **‘electric_bike’** in
our dataset.

So, first, the analysis tells us that the most common bike used by both
types of riders - Annuals and Casuals - are classic bikes.

However, when comparing by proportions, classic_bikes are used more by
annual members than by casual members: **61% vs 39%.**

The next bike commonly used are electric bikes, with annual member using
more of it than casual members: **53.2% vs 46.8%**

Interestingly, more casual users than annual members opt for docked
bikes (**100% vs \< 0.05%**). It seems as though hardly do annuals used
these types of bikes. However, note that, Casuals use docked bikes less,
compared to the other types of Cyclistic bikes.

    Note: With this summary, since a large proportion of Annuals use cyclistic
    bikes to commute to work, it translates to mean a large percentage of
    classic and electric bikes used by Annuals, are for work purposes, than
    for recreational purposes.

    In the same way, since a large proportion of Casuals use cyclistic bikes
    for leisure, it translates to mean large percentage of classic, electric,
    and docked bikes used by Casuals, are for recreational purposes, than for
    work purposes.

Questions to consider:

-   Which bikes is preferred more by Casuals for recreational purpose?
-   Which bikes is preferred more by Annuals for recreational purpose?

### Facet by rideable_type and month

Apparently, our analysis holds true for 10 months from December. Let’s
see if that’s the case in the eleventh month, by comparing February with
November

![](cyclistic_report_files/figure-gfm/Counting%20common%20Cyclistic%20bike_type%20used%20in%20Feb%20and%20Nov-1.png)<!-- -->

Notes: From our plot, we find that while majority of Casuals have
increased in their use of classic and electric bikes over the months
from February. Yet, Annuals subscribe for it more.

We also see that Casuals have maintained an increase use of docked bikes
over the months.

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](cyclistic_report_files/figure-gfm/Line%20plot%20showing%20trend%20of%20how%20classic%20bikes%20were%20used%20per%20month-1.png)<!-- -->

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](cyclistic_report_files/figure-gfm/Line%20plot%20showing%20trend%20of%20how%20docked%20bikes%20were%20used%20per%20month-1.png)<!-- -->

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](cyclistic_report_files/figure-gfm/Line%20plot%20showing%20trend%20of%20how%20electric%20bikes%20were%20used%20per%20month-1.png)<!-- -->

Notes: According to our plot, in the beginning of the year (during the
first 8 months, which includes the summer vacation months), classic
bikes are used more than any other type of Cyclistic bikes. And towards
the end of the year (after the first 8 months), electric bikes are the
ones being used more.

    A. This shows that there is a linear relationship between the use 
    of classic bikes and electric bikes across the months of the year.
    (Check out the scatter plot below to confirm this)

    B. This also shows that both Annuals and Casuals prefer to use Classic
    bikes in the first 8 months of the year than electric bikes.
    However, there are some casuals that opt for docked bikes instead.

Questions to consider:

1.  What factors influence this trend for Casuals - their choice of
    bikes in these months: Time Availability of Bikes, Stations of
    Bikes, Speed of Bikes, Seasons??

Docked bikes are used more, together with classic bikes somewhere in the
beginning and middle of the year - between March and August.

    c. This shows that Casual riders prefer to use more of Classic bikes
    and Docked bikes for recreational purposes during the winter, spring, and
    summer seasosn. And giving that their use of classic bikes is greater than
    docked bikes, we can say they seem to prefer to ride classic bikes, during
    these periods for leisure.

### Scatter plot

Here, we visualize a scatter plot showing the linear relationship
between classic bike and annual bike usage

    ## `geom_smooth()` using formula 'y ~ x'

![](cyclistic_report_files/figure-gfm/Regression%20for%20Electric%20bikes%20on%20Classic%20Bikes-1.png)<!-- -->

Notes: From our plot, we see a negative linear relationship between the
use of classic bikes and electric bikes across the year.

We see that as the use of classic bikes decreases in their patronage
across the year, the use of electric bikes increases.

Question to consider:

-   WHY is there a negative linear relationship between classic bikes
    and electric bikes across the months of the year?

Now, let’s check common bikes used per day_of_week

![](cyclistic_report_files/figure-gfm/Line%20plot%20showing%20trend%20of%20how%20classic%20bikes%20were%20used%20per%20week_day-1.png)<!-- -->
![](cyclistic_report_files/figure-gfm/Line%20plot%20showing%20trend%20of%20how%20docked%20bikes%20were%20used%20per%20week_day-1.png)<!-- -->
![](cyclistic_report_files/figure-gfm/Line%20plot%20showing%20trend%20of%20how%20electric%20bikes%20were%20used%20per%20week_day-1.png)<!-- -->

Now, let check the types of bikes commonly used in each months and
week_day

![](cyclistic_report_files/figure-gfm/Line%20plot%20showing%20trend%20of%20use%20of%20classic%20bikes%20in%20each%20month%20and%20week%20day-1.png)<!-- -->
![](cyclistic_report_files/figure-gfm/Line%20plot%20showing%20trend%20of%20use%20of%20docked%20bikes%20in%20each%20month%20and%20week%20day-1.png)<!-- -->
![](cyclistic_report_files/figure-gfm/Line%20plot%20showing%20trend%20of%20use%20of%20electric%20bikes%20in%20each%20month%20and%20week%20day-1.png)<!-- -->

Notes:

In General, considering the first and last days, classic_bikes are used
more on weekends than business days. Although, when compared to other
bike types, it is used during the business days more. And they are
patronized more from the Winter to Summer Season.

Docked bikes, for all the months, are also used more during weekends
than business days. However, they are patronized in high proportion,
from the Spring entering into the Summer season.

Electric bikes, however are used less during the weekends, but more
during business days. And they are being sort after and use as we
approach the holidays.

## Step 7: Exploring Numerical data

### Descriptive Statistical Analysis

Here, we group by USERTYPE, and calculate the mean, maximum value,
minimum_value, median, standard_deviation, mode, and IQR of RIDE_LENGTH
feature

``` r
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
```

    ## # A tibble: 2 x 9
    ##   USERTYPE   count  mean std_dev   min median  mode   iqr   max
    ##   <chr>      <int> <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
    ## 1 casual   2529005  32.0   263.    -58     16     8    20 55944
    ## 2 member   3066058  13.6    27.7   -54     10     5    11  1560

### Average ride_length for members and casual riders

Here, we visualize the average ride_length for members and casual riders

![](cyclistic_report_files/figure-gfm/Bar%20plot%20showing%20average%20ride_length%20for%20each%20usertype-1.png)<!-- -->

Notes:

1.  From the table, we reaffirm that we have more annual members than
    casual members.

2.  However, on average, casual members ride for longer hours/mins than
    annual members: 32 mins vs 13.6 mins

3.  In summary the highest ride_lengths (in mins for 2021) are from
    casual members (55,944 mins) compared to annual members (1,560
    mins).

4.  Also, the most common ride_lengths for casual members per day is 8
    mins on average. For annuals, it’s 5 mins.

Questions to consider:

-   What could be the reason casuals ride for long mins/hours and
    Annuals ride for less?

-   What factors influence the long rides for Casuals and short rides
    for Annuals?

-   Could it be as a result of Season/Weather of the year, far distance
    to cover, type of activity they do, type of ride?

-   Does the day of the week impact the length of ride?

-   Does the month impact the length of ride?

-   Does the same factor affect annual members?

### Average ride_length for users by Month.

Here, we calculate and visualize the average ride_length for users by
Month.

![](cyclistic_report_files/figure-gfm/Bar%20plot%20showing%20average%20ride%20length%20for%20users%20by%20month-1.png)<!-- -->

Notes:

1.  From the graph, it’s clear that casuals ride longer hours/mins than
    Annuals all through the year. Annuals tend to have shorter and
    steady rides.

2.  We also notice that Casuals and Annuals highest rides between
    February up until October, where it begins to decline as we enter
    the holiday periods: November - January. This is predominantly the
    winter seasons.

    So, evidently, the weather condition influences the length of rides
    for both users. Giving that the weather during the winter season is
    general windy, cold, and frozen, many users would choose to use
    alternate transit than bikes. Hence, the reduction in ride_lengths
    during these months.

    But as the winter ends (somewhere in February), and we approach the
    spring, summer, and autumn seasons, which have more pleasant weather
    conditions (in comparison to the Winter) for bike riding.

In general, many people are inspired to use cyclistic bike share system
under pleasant expected weather conditions.

### Average ride_length for users by day_of_week

Here, we calculate and visualize the average ride_length for users by
Day of Week

![](cyclistic_report_files/figure-gfm/Bar%20plot%20showing%20ride_length%20for%20each%20user%20by%20week_day-1.png)<!-- -->

Notes:

1.  For casual members, they ride more during the weekends: Saturday’s
    and Sunday’s. And this appears to be the same for annuals members.
    In other words, there are more rides during the weekends than the
    business days.

2.  The day with the lowest ride_length for each usertype are the
    business days. However, when you compare Casual ride_lengths on
    working days, they are still higher than Annuals.

    Note: Considering that majority of Casuals use Cyclistic bikes for
    leisure or recreational purposes than simply commuting to work as
    the Annuals, hence it explains why they have longer ride_lengths
    than Annuals - whose ride-length are steady and shorter - during the
    working days.

In general, bikes used for recreational activities have lesser time
constraints, than when used simply to punch a timeline as it is with
those commuting strictly to work.

Now let’s see if there is a relationship between type of rides and ride
length

### Relationship between type of rides and ride length

Here, we calculate and visualize the average ride_length (mins) and
observe how they differ between different “Rideable_type”

![](cyclistic_report_files/figure-gfm/Bar%20plot%20showing%20Average%20Ride-length%20for%20each%20bike%20type-1.png)<!-- -->

Notes:

1.  From the table and plot, docked bikes have longer ride_lengths (81.5
    mins) on average, compared to classic_bikes (19.9 mins), and
    electric_bikes (16 mins).

    One would have assumed that docked_bike longer ride lengths are due
    to it being used often by Casuals. But, giving that Docked bikes are
    used the least by Casuals in comparison to Classic bikes based on
    our analysis above, the result in the graph thus implies that this
    deals with the speed of the bikes.

From our plot, the fastest bike are electric bikes. Following closely
behind it are classic bikes. And the slowest bikes are docked bikes.
(Perhaps, docked bikes used by Cyclistic bike-share company are the
traditional bikes, hence its low speed.)

    In general, one of factors influencing longer ride_lengths with Casuals
    and fewer with Annuals is: the bike_type speed.

    We already know that Annuals use more of Classic and Electric bikes.
    Hence, giving that these two are fast-paced bikes, they commute to their
    destination for less hour/min.

    On the other hand, giving that Casuals use more of classic_bikes and
    docked_bikes, with much of their use during the first eight months of the
    year, it explains why our plot shows long_bike lengths (for casuals)
    during the first 8 months as well, compared to the last four months.

    ## `summarise()` has grouped output by 'RIDEABLE_TYPE'. You can override using the
    ## `.groups` argument.

![](cyclistic_report_files/figure-gfm/Bar%20Plot%20showing%20Average%20Ride-length%20for%20each%20bike%20type%20by%20Month-1.png)<!-- -->

Notes: Evidently, from our plot, docked_bikes are slower than Classic
and Electric bikes.

Now, let’s see if there is a relationship between count of usertype and
rideable_type by Cyclistic bike stations

### Relationship between Usertype and Rideable_type by Station

Here, we want to know if there a relationship between the count of
Rideable_type available per station and the use of Rideable_type

For example, We already know that Casual riders patronize docked bikes
more than Annuals.

-   Why is this the case?

-   Could it be there are more docked_bike near-by stations where
    casuals are located (as one is likely to rent a bike with the
    bike-share station nearest to him or her)?

-   Are annuals located closely to classic_bike and electric_bike
    stations. Does that influence why thy use more of these bikes,
    besides their speed, to commute to work faster?

#### Determine the start stations with the highest number for each rideable_type

![](cyclistic_report_files/figure-gfm/Filter%20for%20the%20first%2020%20start_stations%20having%20the%20highest%20count%20for%20classic_bikes-1.png)<!-- -->

![](cyclistic_report_files/figure-gfm/Filter%20for%20the%20first%2020%20start_stations%20having%20the%20highest%20count%20for%20docked_bikes-1.png)<!-- -->

![](cyclistic_report_files/figure-gfm/Filter%20for%20the%20first%2020%20start_stations%20having%20the%20highest%20count%20for%20electric_bikes-1.png)<!-- -->

Now, let’s see if there is a relationship between number of user_type
and Cyclistic bike start stations

#### Determine the start stations with the highest number of casual or annual users

![](cyclistic_report_files/figure-gfm/Filter%20for%20the%20first%2020%20start_stations%20having%20the%20highest%20count%20for%20casuals-1.png)<!-- -->

![](cyclistic_report_files/figure-gfm/Filter%20for%20the%20first%2020%20start_stations%20having%20the%20highest%20count%20for%20Annuals-1.png)<!-- -->

Notes:

1.  From the result, the start_station with the highest count of
    classic_bikes, electric_bikes, and docked_bikes is **‘Streeter Dr &
    Grand Ave’**

2.  We also observe, from the plots above, that by comparing the counts
    of Casuals per start stations, we see that majority of casuals (**\>
    20,000**) are located closely to start stations where they have high
    counts of docked bikes. For example, *Streeter Dr & Grand Ave*,
    *Millennium Park*, *Shedd Aquarium*, *Theather on the Lake*, etc.

3.  Similarly, we observe, from the plots above, that by comparing the
    counts of Annuals per start stations, we see that majority of
    Annuals (**\> 20,000**) are located away from docked_bike start
    stations, but are found close to start stations with high counts of
    electric and classic bikes. For example, *Clark St & Elm St.*,
    *Wells St & Concord Ln*, *Kingsbury St & Kinzie St*, and *Wells St &
    Elm St*

Now, let’s consider end stations

#### Determine the end stations with the highest number for each rideable_type

![](cyclistic_report_files/figure-gfm/Filter%20for%20the%20first%2020%20end_stations%20having%20the%20highest%20count%20for%20classic_bikes-1.png)<!-- -->

![](cyclistic_report_files/figure-gfm/Filter%20for%20the%20first%2020%20end_stations%20having%20the%20highest%20count%20for%20docked_bikes-1.png)<!-- -->

![](cyclistic_report_files/figure-gfm/Filter%20for%20the%20first%2020%20end_stations%20having%20the%20highest%20count%20for%20electric_bikes-1.png)<!-- -->

Now, let’s see the relationship between number of user_type and
Cyclistic bike end stations

#### Determine the end stations with the highest number of casual or annual users

![](cyclistic_report_files/figure-gfm/Filter%20for%20the%20first%2020%20end_stations%20having%20the%20highest%20count%20for%20casuals-1.png)<!-- -->

![](cyclistic_report_files/figure-gfm/Filter%20for%20the%20first%2020%20end_stations%20having%20the%20highest%20count%20for%20annuals-1.png)<!-- -->

Notes:

1.  From the result, we also observe same output with the end stations,
    having the highest count of classic_bikes, electric_bikes, and
    docked_bikes with **‘Streeter Dr & Grand Ave’**.

2.  We also observe, from the plots above, that by comparing the counts
    of Casuals per end stations, we see that majority of casuals (**\>
    20,000**) are located closely to end stations where they have high
    counts of docked bikes. For example, *Streeter Dr & Grand Ave*,
    *Millennium Park*, *Shedd Aquarium*, *Theather on the Lake*,
    *Michigan Ave & Oak St*, etc.

3.  Similarly, just as with start stations, we observe that by comparing
    the counts of Annuals per end stations, majority of Annuals (**\>
    20,000**) are located away from docked_bike end stations, but are
    found close to end stations with high counts of electric and classic
    bikes. For example, *Clark St & Elm St.*, *Wells St & Concord Ln*,
    *Kingsbury St & Kinzie St*, and *Wells St & Elm St*, etc.

    In general, while Casuals are located closely at areas in Chicago
    where there classic and electric bike stations, a large number of
    them are found patronizing station where there are high counts of
    docked bikes.

    Annuals are located closely at areas in Chicago where there are more
    classic and electric bikes stations, and little to no docked bikes
    station.

    Hence, this explain one reason why Casuals patronize the use of
    docked bikes than Annuals.
