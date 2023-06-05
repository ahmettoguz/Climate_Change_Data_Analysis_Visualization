#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("dplyr")
#install.packages("e1071")
#install.packages("moments")
# clear plot
graphics.off()

library("moments")
library("e1071")
library("ggplot2")
library("gridExtra")
library("grid")

rm(list = ls())
cat("\f")

# Read the CSV file with one column
data <- read.csv("coLevels.csv", header = FALSE)

# remove rows
data <- subset(data, select = -V3)
data <- subset(data, select = -V5)
data <- subset(data, select = -V6)
data <- subset(data, select = -V7)

colnames(data)[colnames(data) == "V4"] <- "V3"

# Remove rows with NA values
data <- na.omit(data)

#remove na
data <- data[complete.cases(data$V1), ]

# Remove rows with empty values
data <- data[!is.na(data$V3) & data$V3 != "", ]
data <- data[!is.na(data$V1) & data$V1 != "", ]

# Group data by year and find the minimum and maximum value for each year
data$min_value <- ave(data$V3, data$V1, FUN = min)
data$max_value <- ave(data$V3, data$V1, FUN = max)


data <- subset(data, V1 %in% c(1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015))


#---------------
# Convert V3 column to numeric
data$V3 <- as.numeric(as.character(data$V3))

# Calculate the mean, mode, min, and max of V3 grouped by V1
mean_by_group <- aggregate(V3 ~ V1, data = data, FUN = mean, na.rm = TRUE)
min_by_group <- aggregate(V3 ~ V1, data = data, FUN = min, na.rm = TRUE)
max_by_group <- aggregate(V3 ~ V1, data = data, FUN = max, na.rm = TRUE)
mode_by_group <- aggregate(V3 ~ V1, data = data, FUN = function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
})
sd_by_group <- aggregate(V3 ~ V1, data = data, FUN = sd, na.rm = TRUE)
median_by_group <- aggregate(V3 ~ V1, data = data, FUN = median)
Q1_by_group <- aggregate(V3 ~ V1, data = data, FUN = function(x) {
  quantile(x, probs = 0.25)
})
Q3_by_group <- aggregate(V3 ~ V1, data = data, FUN = function(x) {
  quantile(x, probs = 0.75)
})
skewness_by_group <- aggregate(V3 ~ V1, data = data, FUN = function(x) {
  skewness(x)
})
kurtosis_by_group <- aggregate(V3 ~ V1, data = data, FUN = function(x) {
  kurtosis(x) + 3
})

# Rename the columns in the resulting dataframes
colnames(mean_by_group) <- c("year", "mean")
colnames(min_by_group) <- c("year", "min")
colnames(max_by_group) <- c("year", "max")
colnames(mode_by_group) <- c("year", "mode")
colnames(sd_by_group) <- c("year", "sd")
colnames(median_by_group) <- c("year", "median")
colnames(Q1_by_group) <- c("year", "Q1")
colnames(Q3_by_group) <- c("year", "Q3")
colnames(skewness_by_group) <- c("year", "skewness")
colnames(kurtosis_by_group) <- c("year", "kurtosis")

# Calculate the count of observations per year
count_table <- table(data$V1)

# Convert the table to a dataframe
count_df <- data.frame(year = as.numeric(names(count_table)),
                       n = as.numeric(count_table))

# Remove NA rows
count_df <- count_df[complete.cases(count_df$year), ]

# Merge the mean_by_group, min_by_group, max_by_group, and count_df dataframes
result_df <- merge(count_df, mean_by_group, by = "year")
result_df <- merge(result_df, min_by_group, by = "year")
result_df <- merge(result_df, max_by_group, by = "year")
result_df <- merge(result_df, mode_by_group, by = "year")
result_df <- merge(result_df, sd_by_group, by = "year")
result_df <- merge(result_df, median_by_group, by = "year")
result_df <- merge(result_df, Q1_by_group, by = "year")
result_df <- merge(result_df, Q3_by_group, by = "year")
result_df <- merge(result_df, skewness_by_group, by = "year")
result_df <- merge(result_df, kurtosis_by_group, by = "year")


# Reorder the columns
#result_df <- result_df[, c("year", "n", "mean", "median", "mode", "min", "max", "sd" ,"Q1", "Q3" , "skewness", "kurtosis")]
result_df <- result_df[, c("year", "n", "Q1", "min", "mean", "median", "mode", "Q3" ,"max", "sd" , "skewness", "kurtosis")]
result_df <- round(result_df, 1)

# Display the resulting dataframe
#print(result_df)

# Combine the first 5 rows and last 5 rows
combined_df <- rbind(head(result_df, 5), tail(result_df, 5))

# Display the combined dataframe
#print(combined_df)


#PLOTS


grid.table(combined_df)

