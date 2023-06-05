# Precipitation ratio in Istanbul between the years 2009 - 2019

remove(list = ls())
cat("\014")

df <- read.csv("Istanbul Weather Data.csv")
df <- na.omit(df)

df <- subset(df, select = c(`DateTime`,`Rain`))

colnames(df) <- c("date", "value")


# Convert the date column to Date format
df$date <- as.Date(df$date, format = "%d.%m.%Y")

# Extract the year from the date
df$year <- format(df$date, "%Y")

# Group the data by year and calculate the mean value
df <- df %>%
  group_by(year) %>%
  summarize(mean_value = mean(value, na.rm = TRUE))


head(df)

df <- data.frame(df)
colnames(df) <- c("Date", "Precipitation")
df
#------------
# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)
data$date <- as.Date(data$date)

data <- df

head(data)

# Plot
data %>%
  head(10) %>%
  ggplot(aes(x = Date, y = Precipitation)) +
  geom_line(color = "grey") +
  geom_point(shape = 21, color = "black", fill = "#69b3a2", size = 9) +
  geom_text(aes(label = round(Precipitation, 1)), vjust = -2, size = 3) +
  labs(x = "Date", y = "Precipitation") +
  theme(
    text = element_text(size = 18),
    panel.background = element_rect(fill = "transparent"),
    plot.margin = margin(50, 50, 50, 50, "pt"),
    axis.title.x = element_text(margin = margin(t = 30, unit = "pt")),
    axis.title.y = element_text(margin = margin(r = 30, unit = "pt"))
  )