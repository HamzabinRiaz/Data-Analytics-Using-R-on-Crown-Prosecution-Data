library(rlang)
library(tidyverse)
library(dplyr)
library(corrplot)
library(visdat)
library(ggplot2)
library(reshape2)
library(DataExplorer)
library(gridExtra)
library(forecast)
library(factoextra)
library(cluster)
library(randomForest)
library(glmnet)
library(nnet)



version <- R.version.string
version

# Defining path to the directory containing the csv files
data_dir <- "C:\\Users\\L380\\OneDrive\\Desktop\\DA-Assignment\\Dataset"

# Merging all files into 1 dataframe and adding 2 new columns of Year and Month
dfs <- list()
for (year in dir(data_dir)) {
  year_dir <- file.path(data_dir, year)
  for (csv_file in dir(year_dir, pattern = ".csv")) {
    month <- str_split(csv_file, "_")[[1]][4]
    year <- as.character(str_split(csv_file, "_")[[1]][5])
    year <- sub(".csv$", "", year)
    print(year)
    df <- read_csv(file.path(year_dir, csv_file))
   df <- df %>% mutate(Year = year, Month = month)
   dfs[[length(dfs) + 1]] <- df
  }
}


# ********************** STEP 1 - DATA CLEANING ***************************


# Sorting All the rows of merged dataframe into ascending order as per Years and Months

final_df <- bind_rows(dfs)

final_df <- final_df %>% arrange(Year, factor(Month, levels = c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")))

final_df

View(final_df)

#verifying the number of rows and columns

nrow(final_df)
ncol(final_df)


#Now rearranging the columns by shifting the columns of Year and Month at the beginning

final_df <- final_df %>%
  select(1, 52, 53, 2:51)

View(final_df)

#verifying the number of rows and columns

nrow(final_df)
ncol(final_df)

#removing the columns with percentages

final_df <- final_df %>%
  select(-matches("Percentage"))

View(final_df)


#shortening names of the columns

#replacing spaces with dots in column names + trimming any extra spaces at start or end of col names

colnames(final_df) <- gsub("Number of", "", colnames(final_df))
colnames(final_df) <- gsub(" ", ".", trimws(colnames(final_df)))

View(final_df)

#changing name of first column to COunty

names(final_df)[names(final_df) == "...1"] <- "County"
colnames(final_df)

#checking unique values of each variable

colnames(final_df)
unique(final_df$Year)
unique(final_df$Month)
unique(final_df$Homicide.Convictions)
unique(final_df$Homicide.Unsuccessful)
unique(final_df$Offences.Against.The.Person.Convictions)
unique(final_df$Offences.Against.The.Person.Unsuccessful)
unique(final_df$Sexual.Offences.Convictions)
unique(final_df$Sexual.Offences.Unsuccessful)
unique(final_df$Burglary.Convictions)
unique(final_df$Burglary.Unsuccessful)
unique(final_df$Robbery.Convictions)
unique(final_df$Theft.And.Handling.Convictions)
unique(final_df$Theft.And.Handling.Unsuccessful)
unique(final_df$Fraud.And.Forgery.Convictions)
unique(final_df$Fraud.And.Forgery.Unsuccessful)
unique(final_df$Criminal.Damage.Unsuccessful)
unique(final_df$Criminal.Damage.Convictions)
unique(final_df$Drugs.Offences.Convictions)
unique(final_df$Drugs.Offences.Unsuccessful)
unique(final_df$Public.Order.Offences.Convictions)
unique(final_df$Public.Order.Offences.Unsuccessful)
unique(final_df$`All.Other.Offences.(excluding.Motoring).Convictions`)
unique(final_df$`All.Other.Offences.(excluding.Motoring).Unsuccessful`)
unique(final_df$Motoring.Offences.Convictions)
unique(final_df$Motoring.Offences.Unsuccessful)
unique(final_df$Admin.Finalised.Unsuccessful)


# standardizing values from Months variable

month_mapping <- c(
  "january" = "January",
  "february" = "February",
  "march" = "March",
  "april" = "April",
  "may" = "May",
  "june" = "June",
  "july" = "July",
  "august" = "August",
  "september" = "September",
  "october" = "October",
  "november" = "November",
  "december" = "December",
  "Aug" = "August",
  "Dec" = "December",
  "Jul" = "July",
  "Nov" = "November",
  "Oct" = "October",
  "Sep" = "September"
)

# cleaning and standardizing the month values
final_df <- final_df %>% mutate(Month = recode(Month, !!!month_mapping))

unique(final_df$Month)



#removing rows with the data of national crimes
final_df <- subset(final_df, !grepl("National", County))

nrow(final_df)
ncol(final_df)

View(final_df)


# defining the list of rural counties
rural_counties <- c("Cumbria", "Dorset", "Durham", "Dyfed Powys", "Gloucestershire",
                    "Lincolnshire", "Norfolk", "North Wales", "North Yorkshire",
                    "South Wales", "Suffolk", "Warwickshire", "Wiltshire")

# adding a column named "Region" indicating rural or urban areas based on the county
final_df$Region <- ifelse(final_df$County %in% rural_counties, "Rural", "Urban")

View(final_df)

#shifting "Region" column to second index after "County" 

final_df <- final_df %>% 
  select(1, 29, 2:28)

View(final_df)

colnames(final_df)


#Checking if there are any missing values in the dataframe

missing_indexes <- which(rowSums(is.na(final_df)) > 0)
print(missing_indexes)



# ********************** STEP 2 - DATA VISUALISATION AND SUMMARY *******************


DataExplorer::create_report(final_df)


summary(final_df)

glimpse(final_df)

str(final_df)


#checking if data set has any missing values / null values

vis_miss(final_df)


colnames(final_df)


#finding thr correlation for all the crime columns of the dataframe

cor_matrix <- cor(final_df[5:29])
  
  corrplot(cor_matrix, type = "upper", order = "hclust", tl.cex = 0.7, tl.col = "black", 
           is.corr = TRUE, mar = c(0, 0, 0, 0))
  heatmap(cor_matrix,
          col = colorRampPalette(c("blue", "white", "red"))(100),
          main = "Correlation Heatmap")



# Showing proportion of convictions by Offense Type
convictions_by_offense <- final_df[, c(5:29)]
convictions_total <- colSums(convictions_by_offense)
pie(convictions_total, labels = names(convictions_total),
    main = "Proportion of Convictions by Offense Type")



# successful crimes' graph over the years

# selecting the relevant variables for the years 2014, 2015, 2017, and 2018
selected_years <- c("2014", "2015", "2016", "2017")
s_crimes_df <- c("Homicide.Convictions", "Offences.Against.The.Person.Convictions",
                   "Burglary.Convictions", "Robbery.Convictions", "Theft.And.Handling.Convictions",
                   "Fraud.And.Forgery.Convictions", "Criminal.Damage.Convictions",
                   "Drugs.Offences.Convictions", "Public.Order.Offences.Convictions",
                   "All.Other.Offences.(excluding.Motoring).Convictions", "Motoring.Offences.Convictions")

# cresting subset of dataframe for selected years and variables
subset_df <- final_df[final_df$Year %in% selected_years, c("Year", s_crimes_df)]

# reshaping the data from wide to long format for plotting
subset_df_long <- reshape2::melt(subset_df, id.vars = "Year", variable.name = "Crime",
                                 value.name = "Convictions")

# creating a grouped bar chart
ggplot(subset_df_long, aes(x = Year, y = Convictions, fill = Crime)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Crime Convictions by Year",
       x = "Year", y = "Convictions", fill = "Crime") +
  theme(legend.position = "right")


# unsuccessful crimes over the years

us_crimes_df <- c("Homicide.Unsuccessful", "Offences.Against.The.Person.Unsuccessful",
                   "Burglary.Unsuccessful", "Robbery.Unsuccessful", "Theft.And.Handling.Unsuccessful",
                   "Fraud.And.Forgery.Unsuccessful", "Criminal.Damage.Unsuccessful",
                   "Drugs.Offences.Unsuccessful", "Public.Order.Offences.Unsuccessful",
                   "All.Other.Offences.(excluding.Motoring).Unsuccessful", "Motoring.Offences.Unsuccessful")

subset_df <- final_df[, c("Year", us_crimes_df)]

subset_df_long <- reshape2::melt(subset_df, id.vars = "Year", variable.name = "Crime", value.name = "Unsuccessful")

ggplot(subset_df_long, aes(x = Year, y = Unsuccessful, fill = Crime)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Unsuccessful Crimes by Year",
       x = "Year", y = "Unsuccessful", fill = "Crime") +
  theme(legend.position = "right")


# Creating a dataframe for successful crimes
successful_crimes_df <- final_df[, c("County", "Year", "Month", "Homicide.Convictions", "Offences.Against.The.Person.Convictions", 
                                     "Sexual.Offences.Convictions","Burglary.Convictions", "Robbery.Convictions", 
                                     "Theft.And.Handling.Convictions", "Fraud.And.Forgery.Convictions", "Criminal.Damage.Convictions", 
                                     "Drugs.Offences.Convictions", "Public.Order.Offences.Convictions", 
                                     "All.Other.Offences.(excluding.Motoring).Convictions",
                                     "Motoring.Offences.Convictions")]
View(successful_crimes_df)

colnames(successful_crimes_df)

# Creating a dataframe for unsuccessful crimes
unsuccessful_crimes_df <- final_df[, c("County", "Year", "Month", "Homicide.Unsuccessful", "Offences.Against.The.Person.Unsuccessful", 
                                       "Sexual.Offences.Unsuccessful",
                                       "Burglary.Unsuccessful", "Robbery.Unsuccessful", "Theft.And.Handling.Unsuccessful",
                                       "Fraud.And.Forgery.Unsuccessful", "Criminal.Damage.Unsuccessful", "Drugs.Offences.Unsuccessful",
                                       "Public.Order.Offences.Unsuccessful", "All.Other.Offences.(excluding.Motoring).Unsuccessful",
                                       "Motoring.Offences.Unsuccessful")]


View(unsuccessful_crimes_df)
colnames(unsuccessful_crimes_df)

#computing correlation matrix for successful crimes
successful_corr <- cor(successful_crimes_df[, 4:15])
print("Correlation Between Successful Crimes: ")
print(successful_corr)

#computing covariance matrix for successful crimes
successful_cov <- cov(successful_crimes_df[, 4:15])
print("Corvariance Between Successful Crimes: ")
print(successful_cov)


#computing correlation matrix for unsuccessful crimes
unsuccessful_corr <- cor(unsuccessful_crimes_df[, 4:15])
print("Correlation Between Unsuccessful Crimes: ")
print(unsuccessful_corr)


#computing covariance matrix for unsuccessful crimes
unsuccessful_cov <- cov(unsuccessful_crimes_df[, 4:15])
print("Correlation Between Unsuccessful Crimes: ")
print(unsuccessful_cov)

# converting correlation and covariance matrices to long format
successful_corr_long <- melt(successful_corr)
unsuccessful_corr_long <- melt(unsuccessful_corr)
successful_cov_long <- melt(successful_cov)
unsuccessful_cov_long <- melt(unsuccessful_cov)

#plotting correlation heatmap for successful crimes
ggplot(successful_corr_long, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Correlation Heatmap - Successful Crimes")

#plotting correlation heatmap for unsuccessful crimes
ggplot(unsuccessful_corr_long, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Correlation Heatmap - Unsuccessful Crimes")

#plotting covariance heatmap for successful crimes
ggplot(successful_cov_long, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Covariance Heatmap - Successful Crimes")

#plotting covariance heatmap for unsuccessful crimes
ggplot(unsuccessful_cov_long, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Covariance Heatmap - Unsuccessful Crimes")

colnames(final_df)
  

# Calculate total number of crimes for each row
total_crimes <- rowSums(final_df[, 5:ncol(final_df)])

# Add the TotalCrimes column to the dataframe
final_df$TotalCrimes <- total_crimes


#PERFECTLY showing the total numbr of crimes took place so far 

# Select the crime variables from your dataset
crime_variables <- c("Homicide.Convictions", "Offences.Against.The.Person.Convictions", "Sexual.Offences.Convictions",
                     "Burglary.Convictions", "Robbery.Convictions", "Theft.And.Handling.Convictions",
                     "Fraud.And.Forgery.Convictions", "Criminal.Damage.Convictions", "Drugs.Offences.Convictions",
                     "Public.Order.Offences.Convictions", "All.Other.Offences.(excluding.Motoring).Convictions",
                     "Motoring.Offences.Convictions")

# Calculate total number of crimes for each offense
offense_totals <- colSums(final_df[, crime_variables])

# Create a data frame with offense names and their total frequencies
offense_data <- data.frame(Offense = names(offense_totals), TotalCrimes = offense_totals)

# Rank offenses based on total frequencies
ranked_offenses <- offense_data[order(offense_data$TotalCrimes, decreasing = TRUE), ]

# Display ranked offenses and their total frequencies
print(ranked_offenses)

#cities wise crime 

print(ranked_data[, c("County", "TotalCrimes")], n = Inf)





#HYPOTHESIS 1 - If the crime rate is increasing or decreasing in the rural areas

# Subsetting the relevant variables
data_subset <- final_df[, c("Year", "TotalCrimes", "Region")]

# Grouping the data by year and region
data_grouped <- aggregate(TotalCrimes ~ Year + Region, data_subset, mean)

# Creating a new variable for rural regions
data_grouped$Rural <- ifelse(data_grouped$Region == "Rural", 1, 0)

# Performing regression analysis
model <- lm(TotalCrimes ~ Year + Rural, data = data_grouped)
summary(model)

final_df$TotalCrimes

# Creating a line plot
ggplot(data_grouped, aes(x = Year, y = TotalCrimes, color = Region, group = Region)) +
  geom_line() +
  labs(x = "Year", y = "Average Crime Rate", color = "Region") +
  theme_minimal()


#-----------------------


#HYPOTHESIS 2:whether the number of Homicide Convictions
#is significantly influenced by the number of Burglary Convictions.


crime_data <- final_df[, c("Homicide.Convictions", "Burglary.Convictions")]

crime_data <- na.omit(crime_data)

# Spliting the data into training and test sets (e.g., 70% training and 30% test)
set.seed(123)  
train_index <- sample(1:nrow(crime_data), 0.7 * nrow(crime_data))
train_data <- crime_data[train_index, ]
test_data <- crime_data[-train_index, ]

model <- lm(Homicide.Convictions ~ Burglary.Convictions, data = train_data)

predictions <- predict(model, newdata = test_data)

mse <- mean((predictions - test_data$Homicide.Convictions)^2)

summary(model) 

#visualising the findings

ggplot(train_data, aes(x = Burglary.Convictions, y = Homicide.Convictions)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Burglary Convictions", y = "Homicide Convictions") +
  ggtitle("Linear Regression: Homicide Convictions vs Burglary Convictions")



#CLUSTERING

# Choosing the relevant variables for clustering
clustering_data <- final_df[, c("Homicide.Convictions", "Offences.Against.The.Person.Convictions",
                                "Burglary.Convictions", "Robbery.Convictions",
                                "Theft.And.Handling.Convictions", "Drugs.Offences.Convictions")]

# standardizing the data
standardized_data <- scale(clustering_data)

#choosing no of clusters using elbow method
wcss <- numeric(length = 10)  

for (k in 1:10) {
  kmeans_model <- kmeans(standardized_data, centers = k)
  wcss[k] <- kmeans_model$tot.withinss
}

plot(1:10, wcss, type = "b", xlab = "Number of Clusters", ylab = "WCSS")

chosen_number_of_clusters <- 4

#applying k-means clustering with the chosen number of clusters
kmeans_model <- kmeans(standardized_data, centers = chosen_number_of_clusters)
cluster_labels <- kmeans_model$cluster

#adding labels with clusters
final_df$Cluster <- as.factor(cluster_labels)

#visualizing the clusters using a scatter plot
ggplot(final_df, aes(x = Homicide.Convictions, y = Burglary.Convictions, color = Cluster)) +
  geom_point() +
  labs(x = "Homicide Convictions", y = "Burglary Convictions", color = "Cluster") +
  theme_minimal()





#PREDICTIVE ANALYSIS - Crimes in Gloucestershire 


#choosing required variables from the dataframe
gloucestershire_data <- subset(final_df, County == "Gloucestershire" & Year >= 2014 & Year <= 2017)

#converting "Year" variable to a proper date format
gloucestershire_data$Year <- as.Date(gloucestershire_data$Year, format = "%Y")

#creating a time series object
crime_ts <- ts(gloucestershire_data$TotalCrimes, frequency = 1, start = c(2014, 1))

#splitting the data into training and testing
train_data <- window(crime_ts, end = c(2016, 12))
test_data <- window(crime_ts, start = c(2017, 1))

# training the data using the ARIMA model
arima_model <- auto.arima(train_data)

#Predicting future values
forecast_values <- forecast(arima_model, h = length(test_data))

#extracting the forecasted values
forecasted_crime <- forecast_values$mean

#calculating accuracy of predictions
accuracy(forecast_values, test_data)

#viewing the forecasted values
forecasted_crime

# plotting the actual and forecasted values
plot(forecast_values, main = "Crime Rate Forecast for Gloucestershire")
lines(test_data, col = "red")
legend("topleft", legend = c("Actual", "Forecast"), col = c("red", "blue"), lty = 1)

colnames(final_df)

colnames(train_data)




#CLASSIFICATION


# Convert the response variable to a factor
classification_df <- final_df[c("Homicide.Convictions", "Burglary.Convictions")]

target_variable <- final_df$Region

# Split the data into training and testing sets (adjust the ratio as needed)
set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(classification_data), 0.7 * nrow(classification_data))
train_data <- classification_data[train_indices, ]
train_target <- target_variable[train_indices]
test_data <- classification_data[-train_indices, ] 
test_target <- target_variable [-train_indices]


multinom_model <- multinom (train_target ~ ., data = train_data)

# predictions on the test data
predictions <- predict(multinom_model, newdata = test_data, type = "class")

# model performance evaluation
accuracy <- sum (predictions == test_target) / length(test_target) 
print (paste("Accuracy: ", accuracy))
                        
