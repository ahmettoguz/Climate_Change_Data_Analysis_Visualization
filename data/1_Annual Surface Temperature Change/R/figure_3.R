#The temperature change with respect to a baseline climatology years between 1961-2009 is measured in degrees Celsius.

remove(list = ls())
cat("\014")

data <- read.csv("Annual_Surface_Temperature_Change.csv")
data <- na.omit(data)

data <- subset(data, Country %in% c("United Kingdom", "United States","Austria", "Canada", "Finland","Sweden"))
data <- subset(data, select = c(`Country` , `F2009`))
head(data)

#------------------


graphics.off()
library(ggplot2)
library(gridExtra)


# create a dataset
Country = data$Country

Temperature_Change = data$F2009
data = data.frame(Country, Temperature_Change, Temperature_Change)


# We want to define the order of plotting
data$Country <- factor(data$Country , levels = Country)
data
str(data)

cbp1 <- c("cadetblue3", "firebrick2", "darkseagreen3", "darkgoldenrod2", "darkorange2", "orchid4")

plot1 <- ggplot(data, aes(y = Temperature_Change, x = Country, fill = Country)) + 
  
  geom_bar(position="dodge", stat="identity", colour="black", width=.7)  +
  
  guides(fill=guide_legend(nrow = 1, byrow = TRUE)) + 
  scale_fill_manual(values = cbp1) +
  geom_text(aes(label=Temperature_Change), size = 5, fontface = "bold", vjust = -0.2) +
  
  ylab("Weight (g)") +
  xlab("Country (mg/L)") +
  labs(fill = "Country") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 18, hjust = 0.5),
        
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.position = "top",
        
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 16))

plot1

