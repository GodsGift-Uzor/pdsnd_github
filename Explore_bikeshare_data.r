
library(tidyverse) 
library(knitr)
library(repr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

#Top 10 End Stations for Newyork
#What is the gender distribution of trips and the user type in NewYork
#Data preparation done by filtering to get only observations where the gender is either male or female and where
#user.type is either customer or subscriber, this is because there are missing values in the usertype and gender variables
#After filtering there are 49244 observations left instead of the 54770 initial observations.

#head(ny)
#Data Preparation filtering for only observations where gender is male or female and UserType is Subscriber or Customer
newyork_data <- ny %>%
                filter(Gender %in% c("Male", "Female"), User.Type %in% c("Subscriber", "Customer")) 

summary(newyork_data)

total_trip_duration_mins <- newyork_data %>% summarise(new_york_trip_duration = sum(Trip.Duration), average_trip = mean(Trip.Duration))
total_trip_duration_mins

popular_end_station <- newyork_data %>% group_by(End.Station) %>% count(sort = TRUE)

popular_start_stationi <- newyork_data %>% group_by(Start.Station) %>% count(sort = TRUE)

gender_distribution <- newyork_data %>% group_by(Gender) %>% count() %>%
                     ggplot(., aes(Gender, n, color = Gender)) + geom_col() + labs(y="Gender Count", x = "Gender") + 
                    ggtitle("Gender Distribution")
ggplotly(gender_distribution)
 
user_type_count <- newyork_data %>% group_by(User.Type) %>% count() %>%
                     ggplot(., aes(User.Type, n, color = User.Type)) + geom_col() + labs(y="UserType Count", x = "User Type") + 
                    ggtitle("User Distribution")                       

user_type_count
                
user_distribution_gender <- newyork_data %>% group_by(User.Type, Gender) %>% count() %>%
                     ggplot(., aes(User.Type, n, color = Gender)) + geom_col() + facet_wrap(~Gender) + labs(y="UserType Count", x = "User Type") + 
                    ggtitle("User Distribution by Gender")   
user_distribution_gender


#top_n(newyork_data,20, n)
#str(NewYork)
#str(ny)
#NewYork

# #Average Trip Duration NYC
# summary(ny$Trip.Duration)
# ny_travel_time = ny %>% drop_na() %>% summarise(sum(Trip.Duration/60))
# wash_travel_time = wash %>% drop_na() %>% summarise(sum(Trip.Duration/60))
# chi_traval_time = chi %>% drop_na() %>% summarise(sum(Trip.Duration/60))


bike_data_gender <- ny %>% drop_na() %>%
                  group_by(Gender) %>%
                  count(sort = TRUE)
                    
bike_data_gender 

ggplot(bike_data_gender, aes(Gender, n)) + geom_boxplot()

bike_data_SE <- ny %>%
                  group_by(Start.Station, End.Station) %>%
                  count(sort = TRUE)
bike_data_SE 

head(wash)

head(chi)

# Data Preparation is done by filtering for observations that contains only Male or Female under the Gender variable
#Filter for observations that has only subscriber or customer as usertype
newyork_data <- ny %>%
                filter(Gender %in% c("Male", "Female"), User.Type %in% c("Subscriber", "Customer"))
str(newyork_data)
summary(newyork_data)

#Group by Gender and count to obtain the gender distribution

gender_distribution <- newyork_data %>% group_by(Gender) %>% count()
gender_distribution

#plot the gender distribution

ggplot(gender_distribution, aes(Gender, n, color = Gender)) + geom_col() + labs(y="Gender Count", x = "Gender") + 
ggtitle("Gender Distribution")


#Grouping by Usertype
user_type_count <- newyork_data %>% group_by(User.Type) %>% count() 
user_type_count

#plot user type vs count obtained from grouping
ggplot(user_type_count, aes(User.Type, n, color = User.Type)) + geom_col() + labs(y="UserType Count", x = "User Type") + 
ggtitle("User Distribution")                       



# Grouping by usertype and gender to obtain the count of each gender in association with their usertypes

user_distribution_gender <- newyork_data %>% group_by(User.Type, Gender) %>% count()
user_distribution_gender

#plot gender and user type distribution with faceting

ggplot(user_distribution_gender, aes(User.Type, n, color = Gender)) + geom_col() + facet_wrap(~Gender) + labs(y="UserType Count", x = "User Type") + 
ggtitle("User Distribution by Gender")   


system('python -m nbconvert Explore_bikeshare_data.ipynb')
