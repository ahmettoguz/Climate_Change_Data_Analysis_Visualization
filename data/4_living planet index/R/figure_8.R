# National Oceanic and Atmospheric Administration (NOAA). 2020. Sea Level Rise.
# Change in mean sea level in the Atlantic Ocean from 1992 to 2022.

library(stringr)
library(dplyr)


remove(list = ls())
cat("\014")

df <- read.csv("sea_Levels.csv")
#df <- na.omit(df)

df <- subset(df, select = c(`Date` , `Measure`, `Value`))

df$Year <- str_extract(df$Date, "\\d{4}")
df$Date <- NULL
colnames(df) <- c("Place", "Value", "Date")

df <- df %>%
  filter(Place == "Atlantic Ocean")
df <- aggregate(Value ~ Date, data = df, FUN = mean)

df

# Group the data by the Measure column and summarize by selecting the first value in each group




#------------
library("ggplot2")
graphics.off()

data <- data.frame(x = as.numeric(df$Date), y = df$Value)

ggp <- ggplot(data, aes(x, y)) +
  geom_point(size = 5, color = "lightblue") +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray", linetype = "dashed", size = 0.1),
        panel.grid.major.y = element_line(color = "gray", linetype = "dashed", size = 0.1),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16), 
        plot.margin = margin(30, 30, 30, 30, "pt"),
        axis.title.x = element_text(margin = margin(t = 20, unit = "pt")),
        axis.title.y = element_text(margin = margin(r = 20, unit = "pt"))) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), color = "orangered", alpha = 0.2) +
  labs(x = "Year", y = "Sea level change (mm)")

ggp