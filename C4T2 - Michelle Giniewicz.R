# Title: C4T2 - Visualize and Analyze Energy Data

#Updated:  9/25/2022


###############
# Project Notes
###############


# Clear console: CTRL + L


###############
# Housekeeping
###############

# Clear objects if necessary
rm(list = ls())

# get working directory
getwd()

# set working directory 
setwd("C:/Users/giniewic/OneDrive - HP Inc/Documents/Personal/UT Data Analytics Cert/Course 4/C4T2")

# see files in working directory
dir()



###############
# Load packages
###############
install.packages("Rtools")
install.packages("caret")
install.packages("corrplot")
install.packages("readr")
install.packages("mlbench")
install.packages("doParallel")
install.packages("reshape2")
install.packages("dplyr")
install.packages("arules")
install.packages("arulesViz")
install.packages("RMariaDB")
install.packages("lubridate")
install.packages("plotly")
install.packages("ggfortify")
install.packages("forecast")
library(caret)
library(corrplot)
library(readr)
library(mlbench)
library(doParallel)
library(e1071)
library(gbm)
library(ggplot2)
library(writexl)
library(reshape2)
library(dplyr)
library(arules)
library(arulesViz)
library(RMariaDB)
library(lubridate)
library(plotly)
library(ggfortify)
library(forecast)



#####################
# Parallel Processing
#####################

#detectCores()         #detect number of cores
#cl <- makeCluster(2)  # select number of cores
#registerDoParallel(cl) # register cluster
#getDoParWorkers()      # confirm number of cores being used by RStudio
#  Stop Cluster -- After performing tasks, make sure to stop cluster
#stopCluster(cl)
#detectCores()


####################
# Import data
####################


# Create DB connection
con = dbConnect(MariaDB(), user='deepAnalytics',password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# List tables in DB
dbListTables(con)

#[1] "iris"    "yr_2006" "yr_2007" "yr_2008" "yr_2009" "yr_2010"

######################
# Save datasets
######################




##################
# Evaluate data
##################


# List attributes in yr_2006 table
dbListFields(con, 'yr_2006')
#[1] "id"                    "Date"                 
#[3] "Time"                  "Global_active_power"  
#[5] "Global_reactive_power" "Global_intensity"     
#[7] "Voltage"               "Sub_metering_1"       
#[9] "Sub_metering_2"        "Sub_metering_3" 

dbListFields(con, 'yr_2007')

#*** THE DATA WE WILL PULL FOR EACH YEAR IS Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3




##################
# Preprocess data
##################

# Download tables with specified attributes
yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")

yr_2007 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")

yr_2008 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")

yr_2009 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")

yr_2010 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")


### Analyze tables

str(yr_2006)
summary(yr_2006)
head(yr_2006)
tail(yr_2006)

### Date & Time are chr data
### All 3 sub_metering attributes are numerical data 

### yr_2006:
#### sub_metering_1: goes from 0-77
#### sub_metering_2: goes from 0-74
#### sub_metering_3: goes from 0-20
#### data from 12/16/2006 to 12/31/2006

str(yr_2007)
summary(yr_2007)
head(yr_2007)
tail(yr_2007)

### yr_2007:
#### sub_metering_1: goes from 0-78
#### sub_metering_2: goes from 0-78
#### sub_metering_3: goes from 0-20
#### data goes from 1/1/2007 to 12/31/2007

str(yr_2008)
summary(yr_2008)
head(yr_2008)
tail(yr_2008)

### yr_2008:
#### sub_metering_1: goes from 0-80
#### sub_metering_2: goes from 0-76
#### sub_metering_3: goes from 0-31
#### data goes from 1/1/2008 to 12/31/2008

str(yr_2009)
summary(yr_2009)
head(yr_2009)
tail(yr_2009)

### yr_2009:
#### sub_metering_1: goes from 0-82
#### sub_metering_2: goes from 0-77
#### sub_metering_3: goes from 0-31
#### data goes from 1/1/2009 to 12/31/2009

str(yr_2010)
summary(yr_2010)
head(yr_2010)
tail(yr_2010)

### yr_2010:
#### sub_metering_1: goes from 0-88
#### sub_metering_2: goes from 0-80
#### sub_metering_3: goes from 0-31
#### data goes from 1/1/2010 to 11/26/2010

# Create Primary Data Frame with multi-year dataset
# Remove 2006 and 2010 since they do not contain an entire year
data <- bind_rows(yr_2007, yr_2008, yr_2009)

str(data)
summary(data)
head(data)
tail(data)

### Date & Time are chr data
### All 3 sub_metering attributes are numerical data 
#### sub_metering_1: goes from 0-82;  mean: 1.159
#### sub_metering_2: goes from 0-78;  mean: 1.343
#### sub_metering_3: goes from 0-31;  mean: 6.217
#### data goes from 01/01/2007 to 12/31/2009


# Combine Date & Time into a new column

data <-cbind(data,paste(data$Date,data$Time), stringsAsFactors=FALSE)

colnames(data)

# Rename the new DateTime column
colnames(data)[6] <- "DateTime"

colnames(data)

summary(data)

# Move the DateTime attribute within the dataset
data <- data[,c(ncol(data),1:(ncol(data)-1))]

# Check columns
head(data)

# Convert DateTime from chr to POSIXct
data$DateTime <- as.POSIXct(data$DateTime, "%Y/%m/%d %H:%M:%S")

# Add the time zone
attr(data$DateTime, "tzone") <- "UTC"

# check columns again
str(data)
summary(data)
head(data)
tail(data)

# Create "year" attribute with lubridate
data$year <- year(data$DateTime)

# Check data with new attribute
str(data)
summary(data)
head(data)
tail(data)

# Create month attribute with lubridate
data$month <- month(data$DateTime)
summary(data)

# Create quarter attribute with lubridate
data$quarter <- quarter(data$DateTime)
summary(data)
head(data)
tail(data)

# Create week attribute with lubridate
data$week <- week(data$DateTime)
summary(data)

# Create day attribute with lubridate
data$day <- day(data$DateTime)
summary(data)

#Create minute attribute with lubridate
data$minute <- minute(data$DateTime)
summary(data)

#Create hour attribute with lubridate
data$hour <- hour(data$DateTime)
summary(data)

# Create weekday attribute with lubridate
data$weekDay <- wday(data$DateTime)
summary(data)

#####################
# EDA/Visualizations
#####################

#--- Statistics ---#
summary(data)

summary(data$Sub_metering_1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   1.159   0.000  82.000 

summary(data$Sub_metering_2)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   1.343   1.000  78.000 

summary(data$Sub_metering_3)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   1.000   6.216  17.000  31.000

## sub_metering 3 uses the most power on average; 
##  sub_metering 1 and 2 seem to have some outliers, since their max is much  higher than sub_metering 3


#--- Plots ---#
barplot_Submeter1_year<-ggplot(data=data, aes(x=year, y=Sub_metering_1)) +
  geom_bar(stat="identity") +
  ggtitle("Sub_metering_1 by year") + 
  theme(plot.title = element_text(hjust = 0.5))

barplot_Submeter1_year

# Create smaller dataset with just meters + year 
year_data <- data[, c(4,5,6,7)]

summary(year_data)

# move year to first column 
year_data <- year_data[,c(ncol(year_data),1:(ncol(year_data)-1))]

summary(year_data)


# bar plot 
gg <- melt(year_data,id="year")

summary(gg)
head(gg)
tail(gg)
tail(data)

ggplot(gg, aes(x=variable, y=value, fill=factor(year))) +
  stat_summary(fun=mean, geom="bar", position=position_dodge(1)) +
  scale_color_discrete("year") +
  labs(title="Average Sub-Metering by Year", x="Metering", y= "Average") +
  theme(plot.title = element_text(hjust = 0.5))


# Create smaller dataset with just meters + month 
month_data <- data[, c(4,5,6,8)]

summary(month_data)

# move month to first column 
month_data <- month_data[,c(ncol(month_data),1:(ncol(month_data)-1))]

summary(month_data)


# bar plot 
gg1 <- melt(month_data,id="month")

summary(gg1)
head(gg1)
tail(gg1)

ggplot(gg1, aes(x=variable, y=value, fill=factor(month))) +
  stat_summary(fun=mean, geom="bar", position=position_dodge(1)) +
  scale_color_discrete("month") +
  labs(title="Average Sub-Metering by Month", x="Metering", y= "Average") +
  theme(plot.title = element_text(hjust = 0.5))


# Create smaller dataset with just meters + quarter 
quarter_data <- data[, c(4,5,6,9)]

summary(quarter_data)

# move year to first column 
quarter_data <- quarter_data[,c(ncol(quarter_data),1:(ncol(quarter_data)-1))]

summary(quarter_data)


# bar plot 
gg2 <- melt(quarter_data,id="quarter")

summary(gg2)
head(gg2)
tail(gg2)

ggplot(gg2, aes(x=variable, y=value, fill=factor(quarter))) +
  stat_summary(fun=mean, geom="bar", position=position_dodge(1)) +
  scale_color_discrete("quarter") +
  labs(title="Average Sub-Metering by Quarter", x="Metering", y= "Average") +
  theme(plot.title = element_text(hjust = 0.5))


# Create box plots to look for outliers

ggplot(gg, aes(x=variable, y=value)) +
  geom_boxplot()

### Submetering 1 & 2 have a lot of outliers




#################
# TASK 2
#################

# Plot all of sub-meter 1
plot(data$Sub_metering_1)


#####################
# Filtering pipeline
#####################

##----Process Steps from Sub-setting to Graphing------#
# 1. dplyr::mutate(): add column with desired time interval (e.g., Yr/Mo/Day) using lubridate 
# 2. dplyr::filter(): select cols to filter by; full ds + added col from mutate are avail to filter at this stage
# 3. dplyr::group_by(): select which time intervals to subset and their order 
# 4. dplyr::summarize(): select the vars and any calculations for these vars
# 5. dplyr::first(): add DateTime col to end summary() string using first() (not needed for forecasting)
# 6. dplyr::filter() to remove any NA or narrow data ranges 


#############
## Subsets
#############

#######Subset1#######

# Subset second week of 2008 - All observations
houseWeek <- filter(data, year== 2008 & week == 2)

# Plot subset houseWeek
plot(houseWeek$Sub_metering_1)


#######Subset2#######

# Subset 9th day of Jan 2008 - All observations
houseDay <- filter(data, year==2008 & month==1 & day==9)

#Plot subset houseDay
plot_ly(houseDay, x= ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type='scatter', mode='lines')

# Plot all 3 submeters - All observations for houseDay
plot_ly(houseDay, x= ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name='Kitchen', type='scatter', mode='lines') %>%
  add_trace(y=~houseDay$Sub_metering_2, name='Laundry Room', mode='lines') %>%
  add_trace(y=~houseDay$Sub_metering_3, name='Water Heater & AC', mode='lines') %>%
  layout(title="Power Consumption January 9th, 2008",
         xaxis=list(title="Time"),
         yaxis=list(title= "Power (watt-hours)"))


### Increase granularity

# Subset Jan 9, 2008 - 10 min frequency
houseDay10 <- filter(data, year == 2008 & month == 1 & day == 9
                     & (minute ==0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

# Plot all 3 submeters - 10 min frequency
plot_ly(houseDay10, x=~houseDay10$DateTime, y=~houseDay10$Sub_metering_1, name="Kitchen", type='scatter', mode='lines') %>%
  add_trace(y=~houseDay10$Sub_metering_2, name='Laundry Room', mode='lines') %>%
  add_trace(y=~houseDay10$Sub_metering_3, name='Water Heater & AC', mode='lines') %>%
  layout(title="Power Consumption January 9th, 2008",
         xaxis=list(title="Time"),
         yaxis=list(title= "Power (watt-hours)"))

###NOTES ABOUT EXAMPLE SUBSETS###
# The "Water Heater & AC" peak from 7:50 - 13:10 is the hot water heater. Based on my research, hot water heaters run from 3-5 hours/day.
# The multiple short peaks that last around 30 minutes are when the AC is running.
# The peaks in the laundry room are from the refrigerator.
# Kitchen appliances were used twice during this day, between 16:30-17:00.


#######Subset3#######

# Subset the 27th week of 2008 - 10 min frequency
houseWeek10 <- filter(data, year== 2008 & week == 27 &
                      (minute ==0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

# Plot all 3 submeters - 10 min frequency for 27th week of 2008
plot_ly(houseWeek10, x=~houseWeek10$DateTime, y=~houseWeek10$Sub_metering_1, name="Kitchen", type='scatter', mode='lines') %>%
  add_trace(y=~houseWeek10$Sub_metering_2, name='Laundry Room', mode='lines') %>%
  add_trace(y=~houseWeek10$Sub_metering_3, name='Water Heater & AC', mode='lines') %>%
  layout(title="Power Consumption 27th week of 2008",
         xaxis=list(title="Time"),
         yaxis=list(title= "Power (watt-hours)"))


#increase granularity to every hour
houseWeekHour <- filter(data, year== 2008 & week == 27 &
                        (minute ==0))

# Plot all 3 submeters - hourly frequency for 27th week of 2008
plot_ly(houseWeekHour, x=~houseWeekHour$DateTime, y=~houseWeekHour$Sub_metering_1, name="Kitchen", type='scatter', mode='lines') %>%
  add_trace(y=~houseWeekHour$Sub_metering_2, name='Laundry Room', mode='lines') %>%
  add_trace(y=~houseWeekHour$Sub_metering_3, name='Water Heater & AC', mode='lines') %>%
  layout(title="Power Consumption 27th week of 2008",
         xaxis=list(title="Time"),
         yaxis=list(title= "Power (watt-hours)"))


#increase granularity to every 4 hours
houseWeek4Hour <- filter(data, year== 2008 & week == 27 &
                         (hour==0 | hour==4 | hour==8 | hour==12 | hour==16 | hour==20) &
                           (minute==0))

# Plot all 3 submeters - 4 hour frequency for 27th week of 2008
plot_ly(houseWeek4Hour, x=~houseWeek4Hour$DateTime, y=~houseWeek4Hour$Sub_metering_1, name="Kitchen", type='scatter', mode='lines') %>%
  add_trace(y=~houseWeek4Hour$Sub_metering_2, name='Laundry Room', mode='lines') %>%
  add_trace(y=~houseWeek4Hour$Sub_metering_3, name='Water Heater & AC', mode='lines') %>%
  layout(title="Power Consumption 27th week of 2008",
         xaxis=list(title="Time"),
         yaxis=list(title= "Power (watt-hours)"))


#######Subset4#######

# Subset 12-1pm on 9th day of Jan 2008
houseDayHour <- filter(data, year == 2008 & month == 1 & day == 9 & hour ==12
                     & (minute ==0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

# Plot all 3 submeters - 12-1pm on 9th day of Jan 2008
plot_ly(houseDayHour, x=~houseDayHour$DateTime, y=~houseDayHour$Sub_metering_1, name="Kitchen", type='scatter', mode='lines') %>%
  add_trace(y=~houseDayHour$Sub_metering_2, name='Laundry Room', mode='lines') %>%
  add_trace(y=~houseDayHour$Sub_metering_3, name='Water Heater & AC', mode='lines') %>%
  layout(title="Power Consumption January 9th, 2008",
         xaxis=list(title="Time"),
         yaxis=list(title= "Power (watt-hours)"))

####SUBSET4 IS NOT HELPFUL####


#######Subset5#######

# Subset month of Jan 2008 - 6 hour increments
houseDayMonth <- filter(data, year == 2008 & month == 1 &
                          (hour==0 | hour==6 | hour==12 | hour==18 ) &
                          (minute==0))

# Plot all 3 submeters - month Jan 2008
plot_ly(houseDayMonth, x=~houseDayMonth$DateTime, y=~houseDayMonth$Sub_metering_1, name="Kitchen", type='scatter', mode='lines') %>%
  add_trace(y=~houseDayMonth$Sub_metering_2, name='Laundry Room', mode='lines') %>%
  add_trace(y=~houseDayMonth$Sub_metering_3, name='Water Heater & AC', mode='lines') %>%
  layout(title="Power Consumption January 2008",
         xaxis=list(title="Time"),
         yaxis=list(title= "Power (watt-hours)"))


#######Subset6#######

# Subset all of 2008 - every 4 days
houseDayYear <- filter(data, year == 2008 &
                         (day==4| day==8| day==12| day==16| day==20| day==24| day==28) &
                          (hour==12) &
                          (minute==0))

# Plot all 3 submeters - month Jan 2008
plot_ly(houseDayYear, x=~houseDayYear$DateTime, y=~houseDayYear$Sub_metering_1, name="Kitchen", type='scatter', mode='lines') %>%
  add_trace(y=~houseDayYear$Sub_metering_2, name='Laundry Room', mode='lines') %>%
  add_trace(y=~houseDayYear$Sub_metering_3, name='Water Heater & AC', mode='lines') %>%
  layout(title="Power Consumption 2008",
         xaxis=list(title="Time"),
         yaxis=list(title= "Power (watt-hours)"))


####### Subsets from Logan's pipeline ##########
########
# 1) Annual subset
########

# Example.

# Create a subset that shows the total annual consumption (kWh) for each submeter over the 
# Jan-07 thru Dec-09 period. 

#--- Create annual aggregate dataset ---#
# Total kWh per SM by year 

Yr.sum <- filter(data, year==2007 | year==2008 | year==2009) %>%
  group_by(year) %>% 
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm=TRUE),3), #Total kWh per hour
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm=TRUE),3),
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm=TRUE),3),
            DateTime = first(DateTime)) # To verify date of first instance

Yr.sum
# A tibble: 3 × 5
#year   SM1   SM2   SM3 DateTime           
#<dbl> <dbl> <dbl> <dbl> <dttm>             
#  1  2007  643.  854. 3023. 2007-01-01 00:00:00
#  2  2008  585.  662. 3179. 2008-01-01 00:00:00
#  3  2009  593.  592. 3557. 2009-01-01 00:00:00

# Plot sub-meter 1, 2 and 3 with title, legend and labels - Year frequency
plot_ly(Yr.sum, x = ~Yr.sum$year, y = ~Yr.sum$SM1, name = 'SM1-Kitchen', type = 'bar') %>%
  add_trace(y = ~Yr.sum$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~Yr.sum$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Annual consumption (kWh) 2007~2009",
         xaxis = list(title = "Year"),
         yaxis = list (title = "Power (kWh)"))  

#--- Create average annual dataset ---#

Yr.mean <- Yr.sum %>%
  summarize(SM1 = mean(SM1), # Total kWh per hour
            SM2 = mean(SM2), 
            SM3 = mean(SM3),
            DateTime = first(DateTime))   # To verify date of first instance
Yr.mean
# A tibble: 1 × 4
#SM1   SM2   SM3 DateTime           
#<dbl> <dbl> <dbl> <dttm>             
#  1  607.  703. 3253. 2007-01-01 00:00:00

#--- Plot ---#
plot_ly(Yr.mean, x = "", y = ~Yr.mean$SM1, name = 'SM1-Kitchen', type = 'bar') %>%
  add_trace(y = ~Yr.mean$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~Yr.mean$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Average Annual Consumption: 2007~2009",
         xaxis = list(title = "Submeter"),
         yaxis = list (title = "Power (kWh)"))  


#####
# 2) Weekday subset
#####

# Another example.

# Create a subset that shows the average daily consumption (kWh) for each submeter 
# by weekday for the winter months (Dec-Feb) over Jan-07 thru Oct-10 period. 
# This subset will have 7 values (one for each weekday - it reflects the 
# typical usage per weekday during the winter season). 

# Create aggregate daily subset

day.sum <- filter(data, Date >= "2007-01-01" & Date <= "2010-11-25") %>%
  group_by(year, month, day) %>%
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm=TRUE),3), #Total kWh per hour
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm=TRUE),3),
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm=TRUE),3),
            DateTime = first(DateTime)) %>% # for filters in Tableau
  filter(!is.na(day)) # Remove the last row that has NA

head(day.sum)
# A tibble: 6 × 7
# Groups:   year, month [1]
#year month   day   SM1   SM2   SM3 DateTime           
#<dbl> <dbl> <int> <dbl> <dbl> <dbl> <dttm>             
#  1  2007     1     1  0    0.352  5.88 2007-01-01 00:00:00
#  2  2007     1     2  0    0.348  6.56 2007-01-02 00:00:00
#  3  2007     1     3  0    0.344  4.76 2007-01-03 00:00:00
#  4  2007     1     4  1.05 7.60  10.9  2007-01-04 00:00:00
#  5  2007     1     5  1.48 0.379  7.60 2007-01-05 00:00:00
#  6  2007     1     6  1.34 0.402  5.68 2007-01-06 00:00:00

#--- Plot ---#
plot_ly(day.sum, x = "", y = ~day.sum$SM1, name = 'SM1-Kitchen', type = 'bar') %>%
  add_trace(y = ~day.sum$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~day.sum$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Aggregate Daily Consumption Consumption: 2010",
         xaxis = list(title = "Submeter"),
         yaxis = list (title = "Power (kWh)"))  

# Create weekday subset for winter 

# Subset for weekday in winter
wday.avg.winter <- day.sum %>%
  mutate(wDay = lubridate::wday(DateTime,1)) %>%  # Add col wDay by adding "1"
  filter(month==12 | month==1 | month==2) %>%   # Filter after mutate, but before group_by
  group_by(wDay) %>%  # Group data by wDay
  summarize(SM1 = round(mean(SM1),3), 
            SM2 = round(mean(SM2),3), 
            SM3 = round(mean(SM3),3),
            DateTime = first(DateTime))
wday.avg.winter
any(is.na(wday.avg.winter))  # FALSE

# A tibble: 7 × 5
#wDay    SM1   SM2   SM3 DateTime           
#<ord> <dbl> <dbl> <dbl> <dttm>             
#  1 Sun    3.26 3.38   9.95 2007-01-07 00:00:00
#  2 Mon    1.17 0.865 10.4  2007-01-01 00:00:00
#  3 Tue    1.11 2.21  10.2  2007-01-02 00:00:00
#  4 Wed    1.75 2.67  10.0  2007-01-03 00:00:00
#  5 Thu    1.26 0.994 10.2  2007-01-04 00:00:00
#  6 Fri    1.30 1.53  10.9  2007-01-05 00:00:00
#  7 Sat    3.30 2.94  12.0  2007-01-06 00:00:00

# Plot 

plot_ly(wday.avg.winter, x = ~wday.avg.winter$wDay, y = ~wday.avg.winter$SM1, 
        name = 'SM1-Kitchen', type = 'scatter', mode='lines') %>%
  add_trace(y = ~wday.avg.winter$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~wday.avg.winter$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Winter Consumption (kWh) 2007~2010",
         xaxis = list(title = "Weekday"),
         yaxis = list (title = "Power (kWh)"))  


##########
# 3) Hourly subset
##########

#####CHANGING TO 2008 SINCE MY DATASET REMOVED 2010, PER THE POA FOR PART 1#####

# Create a subset that shows the average hourly kWh used for each hour of the day during 
# January 2009. This subset should only have 24 values (it reflects the typical usage per 
# hour of day during Jan-09). Plot this subset using a line plot. 

Hr.sum <- filter(data, year==2008 & month==1) %>%
  group_by(hour) %>%
   summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm=TRUE),3), #Total kWh per hour
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm=TRUE),3),
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm=TRUE),3),
            DateTime = first(DateTime)) # To verify date of first instance

head(Hr.sum)
# A tibble: 6 × 5
#hour   SM1   SM2   SM3 DateTime           
#<int> <dbl> <dbl> <dbl> <dttm>             
#  1     0 1.61  2.69  3.79  2009-01-01 00:00:00
#  2     1 0.783 0.494 0.67  2009-01-01 01:00:00
#  3     2 0.424 0.424 0.662 2009-01-01 02:00:00
#  4     3 0     0.369 1.34  2009-01-01 03:00:00
#  5     4 0     0.406 5.43  2009-01-01 04:00:00
#  6     5 0     0.601 2.34  2009-01-01 05:00:00

#--- Plot ---#
plot_ly(Hr.sum, x = ~Hr.sum$DateTime, y = ~Hr.sum$SM1, name = 'SM1-Kitchen', type = 'scatter', mode='lines') %>%
  add_trace(y = ~Hr.sum$SM2, name = 'SM2-Laundry Room', mode='lines') %>%
  add_trace(y = ~Hr.sum$SM3, name = 'SM3-Water Heater & AC', mode='lines') %>%
  layout(title = "Average Hourly Consumption - Jan 2008",
         xaxis = list(title = "Submeter"),
         yaxis = list (title = "Power (kWh)"))  




##################
# Pie Charts
##################

summary(data)

# Pie chart - percentage of total use at various times of day by each sub-meter 



# Pie chart - percentage of total power use over a day by each sub-meter
day_data_Jan9 <-houseDay[,c(4,5,6)]

summary(day_data_Jan9)

Sum_day_data_Jan9 <- data.frame(Kitchen = c(sum(day_data_Jan9$Sub_metering_1)),
                                'Laundry Room' = c(sum(day_data_Jan9$Sub_metering_2)),
                                'AC & Hot Water Heater' = c(sum(day_data_Jan9$Sub_metering_3)))

# Transpose data

m2 <- t(Sum_day_data_Jan9)
Sum_day_data_Jan9_Pie <- data.frame(r1=row.names(m2), m2, row.names=NULL)

head(Sum_day_data_Jan9_Pie)

pie(Sum_day_data_Jan9_Pie$m2, labels=c(Sum_day_data_Jan9_Pie$r1), main="Jan 9, 2008 - by Submeter")



# Pie chart - percentage of total power user over an entire year by each sub-meter 
#year_data <- data[, c(4,5,6,7)]

# Subset year_data for only 2008
year_data_2008 <- filter(year_data, year==2008)

summary(year_data_2008)

year_2008_sub1 <- sum(year_data_2008$Sub_metering_1)
year_2008_sub2 <- sum(year_data_2008$Sub_metering_2)
year_2008_sub3 <- sum(year_data_2008$Sub_metering_3)

Sum_year_2008 <- data.frame(Kitchen = c(year_2008_sub1),
                            Laundry = c(year_2008_sub2),
                            ACWaterHeater = c(year_2008_sub3))

head(Sum_year_2008)

# Transpose data
m1 <- t(Sum_year_2008)
Sum_year_2008_Pie <- data.frame(r1= row.names(m1), m1, row.names=NULL)

head(Sum_year_2008_Pie)

pie(Sum_year_2008_Pie$m1, labels= c(Sum_year_2008_Pie$r1), main="2008 - by Submeter")


#########################
# Prepare to Analyze Data
#########################

## Time Series Analysis

# Subset to one observation per week on Mondays at 8:00pm for 2007, 2008, and 2009
house070809weekly <- filter(data, weekDay ==2 & hour==20 & minute==1)

# Create Time Series object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))

# Produce Time Series plots
autoplot(tsSM3_070809weekly)

# Plot SubMeter3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour='red', xlab="Time", ylab="Watt Hours", main="Sub-meter 3")

# Plot with plot.ts
plot.ts(tsSM3_070809weekly, xlab="Time", ylab="Watt Hours", main= "Sub-meter 3")

# Create Time Series object with SubMeter2
tsSM2_070809weekly <- ts(house070809weekly$Sub_metering_2, frequency=52, start=c(2007,1))

# Plot with plot.ts
plot.ts(tsSM2_070809weekly, xlab="Time", ylab="Watt Hours", main= "Sub-meter 2")

# Create Time Series object with SubMeter1
tsSM1_070809weekly <- ts(house070809weekly$Sub_metering_1, frequency=52, start=c(2007,1))

# Plot with plot.ts
plot.ts(tsSM1_070809weekly, xlab="Time", ylab="Watt Hours", main= "Sub-meter 1")



##############
# Forecasting
##############

## SubMeter3

# Apply time series linear regression to SubMeter 3 ts object and use summary to obtain R2 and RMSE
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season)
summary(fitSM3)

#Residual standard error: 9.046 on 104 degrees of freedom
#Multiple R-squared:  0.263,	Adjusted R-squared:  -0.1055 
#F-statistic: 0.7138 on 52 and 104 DF,  p-value: 0.9105

# Create forecast for SubMeter3 - forecast ahead 20 time periods, with confidence levels 80 and 90
forecastfitSM3 <- forecast(fitSM3, h=20, level=c(80,90))
plot(forecastfitSM3, ylim=c(0,20), ylab="Watt-Hours", xlab="Time", main="Forecast of SubMeter3 - Linear Regression Model")


## SubMeter2
fitSM2 <- tslm(tsSM2_070809weekly ~ trend + season)
summary(fitSM2)

#Residual standard error: 5.98 on 104 degrees of freedom
#Multiple R-squared:  0.3152,	Adjusted R-squared:  -0.02725 
#F-statistic: 0.9204 on 52 and 104 DF,  p-value: 0.6239

# Create forecast for SubMeter2 - forecast ahead 20 time periods, with confidence levels 80 and 90
forecastfitSM2 <- forecast(fitSM2, h=20, level=c(80,90))
plot(forecastfitSM2, ylim=c(0,40), ylab="Watt-Hours", xlab="Time", main="Forecast of SubMeter2 - Linear Regression Model")


##SubMeter1
fitSM1 <- tslm(tsSM1_070809weekly ~ trend + season)
summary(fitSM1)

#Residual standard error: 3.55 on 104 degrees of freedom
#Multiple R-squared:  0.3248,	Adjusted R-squared:  -0.01278 
#F-statistic: 0.9622 on 52 and 104 DF,  p-value: 0.5529

# Create forecast for SubMeter1 - forecast ahead 20 time periods, with confidence levels 80 and 90
forecastfitSM1 <- forecast(fitSM1, h=20, level=c(80,90))
plot(forecastfitSM1, ylim=c(0,40), ylab="Watt-Hours", xlab="Time", main="Forecast of SubMeter1 - Linear Regression Model")


##################################
# Decomposing Seasonal Time Series
##################################

# Decompose SubMeter3 into trend, seasonal, and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)

plot(components070809SM3weekly, col.main="white")
title("Decomposition of Time Series - SubMeter3")


summary(components070809SM3weekly$seasonal)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-6.96934 -6.11837  1.67970  0.01015  2.69893 11.59797 

summary(components070809SM3weekly$trend)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#4.837   5.913   6.404   6.456   6.981   8.231      52 

summary(components070809SM3weekly$random)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-13.3576  -1.4009   0.2289   0.2241   1.8491  13.8059       52 


# Decompose SubMeter2 into trend, seasonal, and remainder
components070809SM2weekly <- decompose(tsSM2_070809weekly)

plot(components070809SM2weekly, col.main="white")
title("Decomposition of Time Series - SubMeter2")

summary(components070809SM2weekly$seasonal)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-1.633937 -0.907976 -0.778168 -0.005783 -0.407976 17.592024 

summary(components070809SM2weekly$trend)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.3269  0.8846  1.0289  1.0292  1.0769  2.3269      52 

summary(components070809SM2weekly$random)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#-18.48625  -0.33722  -0.06318  -0.05837   0.22048  18.36951        52 


# Decompose SubMeter1 into trend, seasonal, and remainder
components070809SM1weekly <- decompose(tsSM1_070809weekly)

plot(components070809SM1weekly, col.main="white")
title("Decomposition of Time Series - SubMeter1")

summary(components070809SM1weekly$seasonal)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.603427 -0.488042 -0.478427 -0.003047 -0.459196 18.550419 


summary(components070809SM1weekly$trend)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.03846 0.03846 0.55769 0.48315 0.88462 0.92308      52 

summary(components070809SM1weekly$random)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#-18.58888  -0.42542  -0.09209  -0.00234   0.42073  18.58420        52 


##########################
# Holt-Winters Forecasting
##########################

##### SubMeter 3

# Remove seasonal compoent for SubMeter 3
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal

autoplot(tsSM3_070809Adjusted, main="SubMeter3 with Seasonal Removed")

# Test seasonal adjustment by running decompose again
plot(decompose(tsSM3_070809Adjusted))

### Seasonal has a very small scale, so it was removed for all practical purposes

# Holt-Winters Expoential smoothing & plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim=c(0,25))

### red line = smooth fitted line; helps see outliers 

# HoltWinters forecast & plot 
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim=c(0,20), ylab="Watt-Hours", xlab="Time - SubMeter 3", col.main="white")
title("Forecasts from HoltWinters - SubMeter3")

# HoltWinters forecast & plot - diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
plot(tsSM3_HW070809forC, ylim=c(0,20), ylab="Watt-Hours", xlab="Time - SubMeter 3", col.main="White")
title("Forecasts from HoltWinters - SubMeter3")

# HoltWinters forecast & plot - diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
plot(tsSM3_HW070809forC, ylim=c(0,20), ylab="Watt-Hours", xlab="Time - SubMeter 3", start(2010), col.main="White")
title("Forecasts from HoltWinters - SubMeter3 - Forecast only")

##### SubMeter 2

# Remove seasonal component for SubMeter2
tsSM2_070809Adjusted <- tsSM2_070809weekly - components070809SM2weekly$seasonal

autoplot(tsSM2_070809Adjusted, main="SubMeter2 with Seasonal Removed")

# Test seasonal adjustment by running decompose again
plot(decompose(tsSM2_070809Adjusted))
### Seasonal has a very small scale, so it was removed for all practical purposes

# Holt-Winters Expoential smoothing & plot
tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM2_HW070809, ylim=c(0,20))
### red line = smooth fitted line; helps see outliers 

# HoltWinters forecast & plot - diminished confidence levels
tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=25, level=c(10,25))
plot(tsSM2_HW070809forC, ylim=c(0,10), ylab="Watt-Hours", xlab="Time - SubMeter 2", col.main="White")
title("Forecasts from HoltWinters - SubMeter2")

# HoltWinters forecast & plot - diminished confidence levels
tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=25, level=c(10,25))
plot(tsSM2_HW070809forC, ylim=c(0,10), ylab="Watt-Hours", xlab="Time - SubMeter 2", start(2010), col.main="White")
title("Forecasts from HoltWinters - SubMeter2 - Forecast only")


##### SubMeter 1

# Remove seasonal component for SubMeter1
tsSM1_070809Adjusted <- tsSM1_070809weekly - components070809SM1weekly$seasonal

autoplot(tsSM1_070809Adjusted, main="SubMeter1 with Seasonal Removed")

# Test seasonal adjustment by running decompose again
plot(decompose(tsSM1_070809Adjusted))
### Seasonal has a very small scale, so it was removed for all practical purposes

# Holt-Winters Expoential smoothing & plot
tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM1_HW070809, ylim=c(0,20))
### red line = smooth fitted line; helps see outliers 

# HoltWinters forecast & plot - diminished confidence levels
tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=25, level=c(10,25))
plot(tsSM1_HW070809forC, ylim=c(0,10), ylab="Watt-Hours", xlab="Time - SubMeter 1", col.main="White")
title("Forecasts from HoltWinters - SubMeter1")


# HoltWinters forecast & plot - diminished confidence levels
tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=25, level=c(10,25))
plot(tsSM1_HW070809forC, ylim=c(0,10), ylab="Watt-Hours", xlab="Time - SubMeter 1", start(2010), col.main="White")
title("Forecasts from HoltWinters - SubMeter1 - Forecast only")

