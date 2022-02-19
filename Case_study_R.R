# loading packages 

install.packages("tidyverse")
install.packages("ggmap")

library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(ggmap)

# merging data located in .csv files

csv_files = list.files(path = "C:/Users/andre/Desktop/CASE STUDY/DATASETS/DIRTY DATA/UNZIPPED", pattern = "csv$",
                       full.names = TRUE)

consolidated_2021 <- map_dfr(csv_files, read_csv)

# using functions to better understand the data frame's structure

str(consolidated_2021)

glimpse(consolidated_2021) 

head(consolidated_2021)

summary(consolidated_2021)

# checking if there are any duplicates in the "ride_id" column 

length(unique(consolidated_2021$ride_id)) == nrow(consolidated_2021)

# checking types of bikes 

unique(consolidated_2021[c("rideable_type")])

# adding a "time_riding" column (in minutes)

consolidated_2021 <- cbind(consolidated_2021, time_riding = as.numeric(consolidated_2021$ended_at - consolidated_2021$started_at)/60)

# checking for NA values

sum(is.na(consolidated_2021$ride_id))
sum(is.na(consolidated_2021$rideable_type))
sum(is.na(consolidated_2021$member_casual))
sum(is.na(consolidated_2021$time_riding))

# filtering 

casual_members=filter(consolidated_2021,member_casual=='casual'& time_riding < 600 & time_riding > 0)
annual_members=filter(consolidated_2021,member_casual=='member'& time_riding < 600 & time_riding > 0)

# casual_member rides vs annual_member rides

perc_1 <- count(annual_members)/count(consolidated_2021)*100
print(perc_1)

perc_2 <- 100-perc_1
print(perc_2)

ggplot(data=consolidated_2021) +
  geom_bar(mapp=aes(x=member_casual , fill=member_casual)) +
  labs(title="Casual member rides Vs. Annual member rides") +
  annotate("text", x=1, y=1500000, label="45.2%", size=10, fontface= "bold") +
  annotate("text", x=2, y=1500000, label="54.8%", size=10, fontface= "bold") +
  ylab("Number of rides") +
  xlab("Member type")
  
# Casual member time riding vs annual member time riding

mean(casual_members$time_riding)

ggplot(data=casual_members)+
  geom_histogram(bins=30,color= "black", fill="green")+
  aes(x=time_riding)+
  xlim(0,60)+
  theme_bw(base_size = 18)+
  xlab("Time of the ride (in minutes)")+
  ylab("Number of rides")+
  ggtitle("Histogram for casual members")+
  geom_vline(aes(xintercept = mean(time_riding)), color= "red") +
  annotate("text",x=40, y=150000, label= "The mean is 25.64 min. \n per ride", size =10)
  
mean(annual_members$time_riding)

ggplot(data=annual_members)+
  geom_histogram(bins=30,color= "black", fill="blue")+
  aes(x=time_riding)+
  xlim(0,60)+
  theme_bw(base_size = 18)+
  xlab("Time of the ride (in minutes)")+
  ylab("Number of rides")+
  ggtitle("Histogram for annual members")+
  geom_vline(aes(xintercept = mean(time_riding)), color= "red")+
  annotate("text",x=40, y=200000, label= "The mean is 13.19 min. \n per ride", size =10)

# bike type vs member 

ggplot(data=consolidated_2021)+
  geom_bar(aes(x=member_casual, fill=rideable_type))+
  theme_bw(base_size = 18)+
  ggtitle("Different type of bike usage according to member type")+
  xlab("Member type")+
  ylab("Count")

# map for stations
                  
myvars <- c("start_lat","start_lng")
start_casual <- casual_members[myvars]
start_casual <- drop_na(start_casual)

mean.longitude <- mean(start_casual$start_lng)
mean.latitude <- mean(start_casual$start_lat)

chicago_map<- get_map(location = c(mean.longitude, mean.latitude), zoom = 13, scale = 2)
chicago_map <- ggmap(chicago_map, extent="device", legend="none")
chicago_map <- chicago_map + stat_density2d(data=start_casual,
                                        aes(x=start_lng, y=start_lat, fill=..level.., alpha=10), geom="polygon")

chicago_map <- chicago_map + scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")))
