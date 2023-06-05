# The weather conditions in Istanbul from 2009 to 2019

remove(list = ls())
cat("\014")

df <- read.csv("Istanbul Weather Data.csv")
df <- na.omit(df)

df <- subset(df, select = c(`DateTime` , `Condition`, `Rain`,`AvgHumidity`,`AvgWind`, `AvgPressure`))
head(df)


#------------

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

count.data <- data.frame(
  classs = c("1st", "2nd", "3rd", "Crwew"),
  prop = c(14.8, 12.9, 32.1, 45.2)
)

count.data <- count.data %>%
  arrange(desc(classs)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)

mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")

ggplot(count.data, aes(x = 2, y = prop, fill = classs)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)


count.data

count.data
