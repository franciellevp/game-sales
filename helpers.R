getwd()
library(ggplot2)
library(shiny)
library(shinydashboard)
library(dplyr)
library(reshape2)
library(DT)

data <- read.csv("data/video_games.csv")

# Check for NA
#names(which(colSums(is.na(data))>0))

# Remove NA
data <- na.omit(data)
# Remove games without release data
data <- subset(data, Year_of_Release != "N/A")
# Remove empty strings in rating
data <- subset(data, Rating != "")

# Check for classification of platforms
#unique(data$Platform)

# Create a column for general category based on the platform
data$Platform_Category[data$Platform == 'Wii'] <- 'Nintendo'
data$Platform_Category[data$Platform == 'DS'] <- 'Nintendo'
data$Platform_Category[data$Platform == 'GBA'] <- 'Nintendo'
data$Platform_Category[data$Platform == '3DS'] <- 'Nintendo'
data$Platform_Category[data$Platform == 'WiiU'] <- 'Nintendo'
data$Platform_Category[data$Platform == 'GC'] <- 'Nintendo'
data$Platform_Category[data$Platform == 'X360'] <- 'Xbox'
data$Platform_Category[data$Platform == 'XB'] <- 'Xbox'
data$Platform_Category[data$Platform == 'XOne'] <- 'Xbox'
data$Platform_Category[data$Platform == 'PS3'] <- 'Playstation'
data$Platform_Category[data$Platform == 'PS2'] <- 'Playstation'
data$Platform_Category[data$Platform == 'PS4'] <- 'Playstation'
data$Platform_Category[data$Platform == 'PS'] <- 'Playstation'
data$Platform_Category[data$Platform == 'PSP'] <- 'Playstation'
data$Platform_Category[data$Platform == 'PSV'] <- 'Playstation'
data$Platform_Category[data$Platform == 'DC'] <- 'Sega'
data$Platform_Category[data$Platform == 'PC'] <- 'PC'

# command to check unique values for possible categories and invalid values
#unique(data$Genre)

# Make Critic Score the same decimal as User Score
data$Critic_Score <- as.numeric(as.character(data$Critic_Score)) / 10  
data$User_Score <- as.numeric(as.character(data$User_Score))
data$Critic_Count <- as.numeric(data$Critic_Count)
data$User_Count <- as.numeric(data$User_Count)

# Transform Year of realease and category variables as factor
data$Year_of_Release <- as.numeric(as.character(data$Year_of_Release))
data$Name <- as.factor(data$Name)
data$Platform <- as.factor(data$Platform)
data$Platform_Category <- as.factor(data$Platform_Category)
data$Genre <- as.factor(data$Genre)
data$Publisher <- as.factor(data$Publisher)
data$Developer <- as.factor(data$Developer)
data$Rating <- as.factor(data$Rating)

# Show statistical details
#dim(data)
#summary(data)

colnames(data) <- c("Name", "Platform", "Year_Release", "Genre", "Publisher", "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales", "Critic_Score", "Critic_Count", "User_Score", "User_Count", "Developer", "Rating", "Platform_Category")
# Based on `summary` the numeric variables arent distributed, so the log is used for transformation
NA_Sales.Log <- log(data$NA_Sales * 1000000 + 1)   
EU_Sales.Log <- log(data$EU_Sales * 1000000 + 1)  
JP_Sales.Log <- log(data$JP_Sales * 1000000 + 1)   
Other_Sales.Log <- log(data$Other_Sales * 1000000 + 1)  
Global_Sales.Log <- log(data$Global_Sales * 1000000 + 1) 
Critic_Count.Log <- log(data$Critic_Count * 1000000 + 1)
User_Count.Log <- log(data$User_Count * 1000000 + 1)

data_log <- cbind.data.frame(NA_Sales.Log, EU_Sales.Log, JP_Sales.Log, Other_Sales.Log, Global_Sales.Log, Critic_Count.Log, User_Count.Log)

# Final dataset
games <- cbind.data.frame(data, data_log)

genres <- unique(games$Genre)


