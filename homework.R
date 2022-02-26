# 259 Homework - integrating skills
# For full credit, answer at least 8/10 questions
# List students working with below:

library(tidyverse)
library(lubridate)
library(DataExplorer)
library(readr)
library(dplyr)
#> These data are drawn from the fivethirtyeight article:
#> http://fivethirtyeight.com/features/what-12-months-of-record-setting-temperatures-looks-like-across-the-u-s/
#> The directory us-weather-history contains a data file for each of 10 cities, labelled by their station name
#> Each data file contains:
#> `date` | The date of the weather record, formatted YYYY-M-D
#> `actual_mean_temp` | The measured average temperature for that day
#> `actual_min_temp` | The measured minimum temperature for that day
#> `actual_max_temp` | The measured maximum temperature for that day
#> `average_min_temp` | The average minimum temperature on that day since 1880
#> `average_max_temp` | The average maximum temperature on that day since 1880
#> `record_min_temp` | The lowest ever temperature on that day since 1880
#> `record_max_temp` | The highest ever temperature on that day since 1880
#> `record_min_temp_year` | The year that the lowest ever temperature occurred
#> `record_max_temp_year` | The year that the highest ever temperature occurred
#> `actual_precipitation` | The measured amount of rain or snow for that day
#> `average_precipitation` | The average amount of rain or snow on that day since 1880
#> `record_precipitation` | The highest amount of rain or snow on that day since 1880

stations <- c("KCLT", "KCQT", "KHOU", "KIND", "KJAX", "KMDW", "KNYC", "KPHL", "KPHX", "KSEA")
cities <- c("Charlotte", "Los Angeles", "Houston", "Indianapolis", "Jacksonville", 
            "Chicago", "New York City", "Philadelphia", "Phoenix", "Seattle")


# QUESTION 1
#> The data files are in the directory 'us-weather-history'
#> Write a function that takes each station abbreviation and reads
#> the data file and adds the station name in a column
#> Make sure the date column is a date
#> The function should return a tibble
#> Call the function "read_weather" 
#> Check by reading/glimpsing a single station's file

directory <- "C:/Users/User/Documents/259-integrating-skills-hw"
setwd(directory)
list  <- dir('us-weather-history',pattern = "*.csv", full.names = T )

file <-  read_csv(list)
  
 read_weather <- function(st){
   output <- read_csv(paste0("us-weather-history/",st,".csv")) %>% 
     mutate(station_name = st, date=as.Date(date))
   return(output)
}
 read_weather("KCLT")

# QUESTION 2
#> Use map_dfr() and your new function to read in all 10 stations
#> map_dfr() will take each dataframe and automatically bind them.
#> Save the resulting dataset to "ds"

ds <-  map_dfr(stations,read_weather)

# QUESTION 3
#> Make a factor called "city" based on the station variable
#> (station should be the level and city should be the label)
#> Use fct_count to check that there are 365 days of data for each city 

city <- factor(ds$station_name,levels = stations,labels = cities)
fct_count(city)

# I think that this is supposed to be added to the data frame ds?? I will do that
# below just in case. 

ds <- mutate(ds,City = factor(ds$station_name,levels = stations,labels = cities))

# QUESTION 4
#> Since we're scientists, let's convert all the temperatures to C
#> Write a function to convert F to C, and then use mutate across to 
#> convert all of the temperatures, rounded to a tenth of a degree

F_to_C <- function(fahrenhiet){
  C <- (fahrenhiet - 32)* 5/9 
  C_round <- round(C, digits = 1)
  return(C_round)
}
x <- ds %>%  mutate(across(actual_mean_temp:record_max_temp, F_to_C(.x)))
x <- ds %>%  mutate(across(2:8, F_to_C(.x)))
# I do not know why this isn't working. Uhhh

### CHECK YOUR WORK
#> At this point, your data should look like the "compiled_data.csv" file
#> in data-clean. If it isn't, read in that file to use for the remaining
#> questions so that you have the right data to work with.
setwd("C:/Users/User/Documents/259-integrating-skills-hw")
ds_clean <- read.csv( "data-clean/compiled_data.csv")


# QUESTION 5
#> Write a function that counts the number of extreme temperature days,
#> where the actual min or max was equal to the (i.e., set the) record min/max
#> A piped function starting with '.' is a good strategy here.
#> Group the dataset by city to see how many extreme days each city experienced,
#> and sort in descending order to show which city had the most:
#> (Seattle, 20, Charlotte 12, Phoenix 12, etc...)
#> Don't save this summary over the original dataset!
#> 
#> 
  
  x_temp <- . %>% ds_clean %>%  filter(actual_min_temp == record_min_temp|
                                         actual_max_temp==record_max_temp) %>% 
    group_by(city) 
  output <- (count(x_temp$city))
  output <- arrange(output,desc(freq))
  print(output)

# QUESTION 6
#> Pull out the month from the date and make "month" a factor
#> Split the tibble by month into a list of tibbles 
#> Month is the middle number ymd of the values in date
#> 
month_levels <- c(
   "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
 )
level <- c(1,2,3,4,5,6,7,8,9,10,11,12)
ds_clean <- ds_clean %>% mutate(Month = factor(month(date),levels = level,  labels = month_levels))
tbl_list <- ds_clean %>% group_split(Month)


# QUESTION 7
#> For each month, determine the correlation between the actual_precipitation
#> and the average_precipitation (across all cities), and between the actual and average mins/maxes
#> Use a for loop, and print the month along with the resulting correlation
#> Look at the documentation for the ?cor function if you've never used it before
ds_clean %>% group_by(Month,city)
cor(ds_clean$actual_precipitation,ds_clean$average_precipitation)

for (mi in month_levels){
for (i in length(ds_clean)){
  if (ds_clean[[i,16]]== ds_clean$Month[mi])
  cor_data <- ds_clean[i,1:17]
  #select(ds_clean,Month == i )
  #cor(ds_clean$actual_precipitation,ds_clean$average_precipitation)
  
}}



# QUESTION 8
#> Use the Data Explorer package to plot boxplots of all of the numeric variables in the dataset
#> grouped by city, then do the same thing grouped by month. 
#> Finally, use plot_correlation to investigate correlations between the continuous variables only
#> Check the documentation for plot_correlation for an easy way to do this
#> 
 
for (i in 2:13){
  boxplot(ds_clean[,i]~ city)
  boxplot(ds_clean[,i]~ ds_clean$Month)}
# ^^^^ I know this is probably hard coding but I think it is a clean method.
plot_correlation(ds_clean,type = 'continuous')

# QUESTION 9
#> Create a scatterplot of actual_mean_temp (y axis) by date (x axis)
#> Use facet_wrap to make a separate plot for each city (3 columns)
#> Make the points different colors according to month

  
 ggplot(ds_clean)+
   geom_point(aes(x=date, y = actual_mean_temp,colour= Month))+facet_wrap(city)


# QUESTION 10
#> Write a function that takes the dataset and the abbreviate month as arguments
#> and creates a scatter and line plot of actual temperature (y axis) by date (x axis)
#> Note, just add geom_line() to your ggplot call to get the lines
#> use the ggtitle() function to add the month as a title
#> The function should save the plot as "eda/month_name.png"
#> The eda folder has an example of what each plot should look like
#> Call the function in a map or loop to generate graphs for each month

many_plots <- function(ds,AbM){
  return_output <- ggplot(ds)+ geom_point(aes(x= date, y = actual_mean_temp)) + 
    geom_line(aes(x= date, y = actual_mean_temp))+
    labs(title = AbM)
savePlot("eda/",AbM,"actual_temps_plot",type = "png")
  return(return_output)
}

  for (i in month_levels){
    many_plots(ds_clean,i)
    }
# So I don't understand why this function wont take my second input. 
# so I got the second input issue but now I am testing the for loop. 
