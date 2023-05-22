#install required packages -----
install.packages("stringr", "httr", "tidyverse")
install.packages('lubridate')
install.packages("dplyr")
install.packages("hydroTSM")
install.packages("geosphere")
install.packages("rmarkdown")
install.packages("ggplot2")
install.packages("nnet")
install.packages("caret")
install.packages("tensorflow")
install.packages("keras")
install.packages("randomForest")
install.packages("shinydashboard")
install.packages("deepnet")

# load required packages -----
library(stringr)
library(httr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(hydroTSM)
library(geosphere)
library(rmarkdown)
library(ggplot2)
library(nnet)
library(caret)
library(tensorflow)
library(keras)
library(randomForest)
library(shinydashboard)
library(deepnet)

# download and import data ------


# downlaod and import 202104
dataset <- httr::GET("https://divvy-tripdata.s3.amazonaws.com/202104-divvy-tripdata.zip")
temp <- tempfile()
download.file(dataset$url, temp)

data_202104 <- read.csv(unz(temp, '202104-divvy-tripdata.csv'))
unlink(temp)



# downlaod and import 202105
dataset <- httr::GET("https://divvy-tripdata.s3.amazonaws.com/202105-divvy-tripdata.zip")
temp <- tempfile()
download.file(dataset$url, temp)

data_202105 <- read.csv(unz(temp, '202105-divvy-tripdata.csv'))
unlink(temp)



# downlaod and import 202106
dataset <- httr::GET("https://divvy-tripdata.s3.amazonaws.com/202106-divvy-tripdata.zip")
temp <- tempfile()
download.file(dataset$url, temp)

data_202106 <- read.csv(unz(temp, '202106-divvy-tripdata.csv'))
unlink(temp)



# downlaod and import 202107
dataset <- httr::GET("https://divvy-tripdata.s3.amazonaws.com/202107-divvy-tripdata.zip")
temp <- tempfile()
download.file(dataset$url, temp)

data_202107 <- read.csv(unz(temp, '202107-divvy-tripdata.csv'))
unlink(temp)



# downlaod and import 202108
dataset <- httr::GET("https://divvy-tripdata.s3.amazonaws.com/202108-divvy-tripdata.zip")
temp <- tempfile()
download.file(dataset$url, temp)

data_202108 <- read.csv(unz(temp, '202108-divvy-tripdata.csv'))
unlink(temp)



# downlaod and import 202109
dataset <- httr::GET("https://divvy-tripdata.s3.amazonaws.com/202109-divvy-tripdata.zip")
temp <- tempfile()
download.file(dataset$url, temp)

data_202109 <- read.csv(unz(temp, '202109-divvy-tripdata.csv'))
unlink(temp)



# downlaod and import 202110
dataset <- httr::GET("https://divvy-tripdata.s3.amazonaws.com/202110-divvy-tripdata.zip")
temp <- tempfile()
download.file(dataset$url, temp)

data_202110 <- read.csv(unz(temp, '202110-divvy-tripdata.csv'))
unlink(temp)



# downlaod and import 202111
dataset <- httr::GET("https://divvy-tripdata.s3.amazonaws.com/202111-divvy-tripdata.zip")
temp <- tempfile()
download.file(dataset$url, temp)

data_202111 <- read.csv(unz(temp, '202111-divvy-tripdata.csv'))
unlink(temp)



# downlaod and import 202112
dataset <- httr::GET("https://divvy-tripdata.s3.amazonaws.com/202112-divvy-tripdata.zip")
temp <- tempfile()
download.file(dataset$url, temp)

data_202112 <- read.csv(unz(temp, '202112-divvy-tripdata.csv'))
unlink(temp)



# downlaod and import 202201
dataset <- httr::GET("https://divvy-tripdata.s3.amazonaws.com/202201-divvy-tripdata.zip")
temp <- tempfile()
download.file(dataset$url, temp)

data_202201 <- read.csv(unz(temp, '202201-divvy-tripdata.csv'))
unlink(temp)



# downlaod and import 202202
dataset <- httr::GET("https://divvy-tripdata.s3.amazonaws.com/202202-divvy-tripdata.zip")
temp <- tempfile()
download.file(dataset$url, temp)

data_202202 <- read.csv(unz(temp, '202202-divvy-tripdata.csv'))
unlink(temp)



# downlaod and import 202203
dataset <- httr::GET("https://divvy-tripdata.s3.amazonaws.com/202203-divvy-tripdata.zip")
temp <- tempfile()
download.file(dataset$url, temp)

data_202203 <- read.csv(unz(temp, '202203-divvy-tripdata.csv'))
unlink(temp)



# merge files -----
bicycle_data <- bind_rows(data_202104, data_202105, data_202106, data_202107, data_202108, data_202109, data_202110, data_202111, data_202112, data_202201, data_202202, data_202203)


# str() to display the structure of bicycle_data
str(bicycle_data)



# display column names and summary
colnames(bicycle_data)
summary(bicycle_data)

#gives type of bicycle_data
class(bicycle_data)


# dropping columns containing ride and station ids
bicycle_data = bicycle_data[, c(-1,-6,-8)] # - is to exclude the specific columns
colnames(bicycle_data)



#omit NA data when the bike was not moved from the station/ Converting 0 values to NA
bicycle_data[bicycle_data==0] <- NA
bicycle_data

# Removing NA / NULL values from the dataset 
bicycle_data <- na.omit(bicycle_data)

#seeing the number of rows removed
str(bicycle_data)



# deriving the weekday of the ride start time from started_at column 
bicycle_data <- cbind(bicycle_data, weekdays(as.Date(bicycle_data$started_at)))
colnames(bicycle_data)



# exploring each column
ridable_type <- unique(bicycle_data$rideable_type)
ridable_type



station_names <- unique(c(bicycle_data$start_station_name, bicycle_data$end_station_name))
station_names



member_categories <- unique(bicycle_data$member_casual)
member_categories



start_lat <- unique(bicycle_data$start_lat)   # we can add range for each lat and long and add stations on map
start_lat



#Adding duration column in the data frame
bicycle_data$ride_length <- difftime(bicycle_data$ended_at, bicycle_data$started_at)

#Observe new column ride_length in the bicycle_data
colnames(bicycle_data)



# conversion of date variable into POSIXct format 
bicycle_data$started_at = as_datetime(bicycle_data$started_at)
class(bicycle_data$started_at)

date <- as.POSIXct(bicycle_data$started_at, format = "%Y-%m-%d %H:%M:%S")
bicycle_data$started_at[1]




# extract date from time stamp
datemonth <- as.Date(date, format="%Y-%m-%d")
bicycle_data <- cbind(bicycle_data, datemonth)

#adding seasons column
bicycle_data$season <- time2season(datemonth, out.fmt = "seasons", type="default")
colnames(bicycle_data)



#Adding ride distance using distGeo function and converting in miles
bicycle_data$distance <- distGeo(matrix(c(bicycle_data$start_lng, bicycle_data$start_lat), ncol=2), matrix (c(bicycle_data$end_lng, bicycle_data$end_lat), ncol=2))
bicycle_data$distance <- bicycle_data$distance/1609 
bicycle_data$distance
bicycle_data$ride_length



#Adding days of the week column : weekday or weekend
bicycle_data$week <- ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
str(bicycle_data)

bicycle_data$week 



# has to remove an extra column : bicycle_data$daytype <- NULL
#creating a new variable for analysis and removing all negative values
cleaned_data <- bicycle_data[!(bicycle_data$ride_length <= 0),]

cleaned_data <- cleaned_data %>% select(-start_lat, -start_lng, -end_lat, -end_lng)


# display column names and summary after cleaning the code
colnames(cleaned_data)
summary(cleaned_data)


#Descriptive Analysis ----


#Analyzing Ride Length
cleaned_data %>% group_by(member_casual) %>%
  summarise(average_ride_length = mean(ride_length), median_length = median(ride_length), 
            max_ride_length = max(ride_length), min_ride_length = min(ride_length)) %>%
  ggplot(aes(x = member_casual, y = average_ride_length)) +
  labs(title ="Average Ride length of member vs casual user ") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))



#Analyzing the rides taken by Members vs casual users.
cleaned_data %>% group_by(member_casual) %>% summarise(total_rides = length(started_at)) %>%
  ggplot(aes(x = member_casual, y = total_rides)) +
  labs(title ="Total rides of Members vs Casual riders ") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))



#Analyzing seasonal usage of members vs casual users.
cleaned_data %>% group_by(member_casual, season) %>%
  summarise(number_of_rides = n(),average_ride_length = mean(ride_length),.groups="drop") %>% 
  arrange(member_casual, season)%>% 
  ggplot(aes(x = season, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides of Members and Casual riders Vs. Season") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))



# Analyzing the duration of usage of member or casual user vs the season
cleaned_data %>%  
  group_by(member_casual, season) %>% 
  summarise(average_ride_length = mean(ride_length), .groups="drop") %>%
  ggplot(aes(x = season, y = average_ride_length, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average ride time of Members and Casual riders Vs. Day of the week")



#Analyzing the mean distance travelled by both type of users.
cleaned_data %>%  
  group_by(member_casual) %>% 
  summarise(average_ride_distance = mean(distance), .groups="drop") %>%
  ggplot(aes(x = member_casual, y = average_ride_distance, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average ride distance of Members and Casual riders")



#Analyzing the mean distance travelled by both type of users Vs Season.
cleaned_data %>%  
  group_by(member_casual,season) %>% 
  summarise(average_ride_distance = mean(distance), .groups="drop") %>%
  ggplot(aes(x = season, y = average_ride_distance, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average ride distance of Members and Casual riders Vs Season in miles")



#Analysing weekday vs weekend
cleaned_data %>%  
  group_by(member_casual, week) %>% 
  summarise(average_ride_length = mean(ride_length), .groups="drop") %>%
  ggplot(aes(x = week, y = average_ride_length, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average ride time of Members and Casual riders Vs. Day of the week")




#Analysing number of rides during weekday vs weekend
cleaned_data %>% group_by(member_casual, week) %>%
  summarise(number_of_rides = n(),average_ride_length = mean(ride_length),.groups="drop") %>% 
  arrange(member_casual, week)%>% 
  ggplot(aes(x = week, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides of Members and Casual riders Vs. Season") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


#Predictive Analysis ----

#Model1: Multinomial Logistic Regression to predict the type of byke for the given station name
pred1_data <- cleaned_data %>% select(rideable_type, start_station_name)
pred1_data <- pred1_data[1:70000,]

pred1_data = pred1_data[sample(1:nrow(pred1_data)), ]

#Use 80% of dataset as training set and remaining 20% as testing set
sample <- sample(c(TRUE, FALSE), nrow(pred1_data), replace=TRUE, prob=c(0.8,0.2))
train  <- pred1_data[sample, ]
test   <- pred1_data[!sample, ]

ytest = test$rideable_type
xtest = test %>% select(-rideable_type)

# Setting the baseline to "classic_bike"
train$rideable_type <- relevel(factor(train$rideable_type), ref = "classic_bike")

# Training the multinomial model
multinom_model <- multinom(rideable_type ~ start_station_name, data = pred1_data)

# Checking the model
#summary(multinom_model)

# Predicting the values for test dataset
test$ClassPredicted <- predict(multinom_model, xtest, "class")

# Building classification table
tab <- table(test$rideable_type, test$ClassPredicted)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)








# Model2: Random forest regression to predict no. of checkouts
cleaned_data <- cleaned_data %>% rename('week_day' = 'weekdays(as.Date(bicycle_data$started_at))')
pred2_data <- cleaned_data %>% group_by(rideable_type, week_day, season, member_casual) %>% summarize(no_of_rides = n())

rideable_type_vect <- unique(pred2_data$rideable_type)
member_casual_vect <- unique(pred2_data$member_casual)
week_day_vect <- unique(pred2_data$week_day)
season_vect <- unique(pred2_data$season)

pred2_data$rideable_type = factor(pred2_data$rideable_type, 
                                  levels = rideable_type_vect, 
                                  labels = c(1:length(rideable_type_vect)))

pred2_data$week_day = factor(pred2_data$week_day, 
                             levels = week_day_vect, 
                             labels = c(1:length(week_day_vect)))

pred2_data$season = factor(pred2_data$season, 
                           levels = season_vect, 
                           labels = c(1:length(season_vect)))

pred2_data$member_casual = factor(pred2_data$member_casual, 
                                  levels = member_casual_vect, 
                                  labels = c(1:length(member_casual_vect)))

pred2_data = pred2_data[sample(1:nrow(pred2_data)), ]

#make this example reproducible
set.seed(42)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(pred2_data), replace=TRUE, prob=c(0.9,0.1))
train  <- pred2_data[sample, ]
test   <- pred2_data[!sample, ]

ytest = test$no_of_rides
xtest = test %>% select(-no_of_rides)

#Random forest regression model
set.seed(42)
rf_model <- randomForest(formula = no_of_rides ~ rideable_type + week_day + season + member_casual, data=train, ntree=1000,
                         keep.forest=TRUE, importance=TRUE,  xtest = xtest,
                         ytest = ytest)
summary(rf_model)
#find number of trees that produce lowest test MSE
which.min(rf_model$mse)

#find RMSE of best model
sqrt(rf_model$mse[which.min(rf_model$mse)]) 

plot(rf_model)

#use fitted bagged model to predict no_of_rides of new observation
ypredict <- predict(rf_model, newdata=xtest)

df <- data.frame(ytest, ypredict)

ggplot(df, aes(ytest, ypredict, size=ytest)) + geom_point(color='skyblue') + 
  xlab("Actual Checkout Count") + ylab("Predicted Checkout Count") + ggtitle("Random Forest Error Analysis") + 
  theme_classic()






# Model2: Linear Regression to predict no. of checkouts
# Training the linear model
lin_model <- lm(no_of_rides ~ rideable_type + week_day + season + member_casual, data = train)

# Checking the model
summary(lin_model)

# Predicting the values for test dataset
ypredict <- predict(lin_model, xtest)

df <- data.frame(ytest, ypredict)

ggplot(df, aes(ytest, ypredict, size=ytest)) + geom_point(color='skyblue') + 
  xlab("Actual no. of rides") + ylab("Predicted no. of rides") + ggtitle("Linear Regression Error Analysis") + 
  theme_classic()

# Building confusion matrix
tab <- table(ytest, ypredict)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

