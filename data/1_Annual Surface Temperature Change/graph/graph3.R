#The temperature change with respect to a baseline climatology years between 1961-2009 is measured in degrees Celsius.

remove(list = ls())
cat("\014")

data <- read.csv("Annual_Surface_Temperature_Change.csv")
data <- na.omit(data)

data <- subset(data, Country %in% c("United Kingdom", "United States"))
data <- subset(data, select = -c(`ObjectId`,`Unit`, `ISO2`, `ISO3`,`Indicator`,`Source`,`CTS_Code`,`CTS_Name`,`CTS_Full_Descriptor`))
#data <- subset(data, select = c(`Country` , `F1970`, `F1971`))
head(data)

#------------------
library(reshape2)
library(ggplot2)
library(tidyr)
graphics.off()
#------------------

# Create a vector of column names for the years F1961 to F2009
years <- paste("F", 1961:2009, sep = "")

# Select the columns Country and the years F1961 to F2009
selected_columns <- c("Country", years)

# Create a new dataframe with the selected columns
selected_data <- data[selected_columns]

# Pivot the dataframe from wide to long format
transformed_data <- pivot_longer(selected_data, cols = -Country, names_to = "date", values_to = "value")

# Extract the year from the "date" column
transformed_data$date <- as.Date(substring(transformed_data$date, 2), format = "%Y")

# Extract the temperature type from the "Country" column
transformed_data$variable <- ifelse(grepl("United Kingdom", transformed_data$Country), "United Kingdom", "United States")

# Remove unnecessary columns
transformed_data <- transformed_data[, c("date", "Country", "value")]

# Print the transformed dataframe
print(transformed_data)

df <- transformed_data
print(df, n = 5)
str(df)

# Multiple line plot
plot1 <- ggplot(df, aes(x = date, y = value)) + 
  geom_line(aes(color = Country), size = 1) +
  theme(legend.title = element_text(size = 18), legend.position = "top", legend.text = element_text(size = 16)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  ylab("Value") +
  xlab("Date") +
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'dashed', colour = "grey"), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 16))

plot1


# Area plot
plot2 <- ggplot(df, aes(x = date, y = value)) + 
  geom_area(aes(color = Country, fill = Country), alpha = 0.2, position = position_dodge(0.8)) +
  theme(legend.title = element_text(size=18), legend.position = "top", legend.text = element_text(size = 16)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  #ggtitle(label = "Area chart for economics data") +
  ylab("Value") +
  xlab("Date") +
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'dashed', colour = "grey"), 
        panel.grid.minor =  element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 16))

plot2

