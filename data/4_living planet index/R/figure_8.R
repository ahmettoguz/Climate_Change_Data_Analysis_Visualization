# Statistics Netherlands
# Living Planet Index from 1970 to 2010, categorized by different income categories of the countries

library(dplyr)

remove(list = ls())
cat("\014")

df <- read.csv("livingPlanet.csv")
colnames(df) <-
  c("Year", "High Income Countries", "Middle Income Countries" , "Low Income Countries")
df <- na.omit(df)
df <- df[df$Year %in% seq(1970, 2010, by = 5), ]
df



#------------
library(tidyr)
library("ggplot2")
graphics.off()


df_long <- df %>%
  pivot_longer(cols = -Year, names_to = "IncomeCategory", values_to = "Income")
df_long <- df_long %>%
  mutate(IncomeCategory = factor(IncomeCategory, levels = c("High Income Countries", "Middle Income Countries", "Low Income Countries")))


ggplot(data = df_long, aes(x = Year, y = Income, color = IncomeCategory, group = IncomeCategory)) +
  geom_line() +
  facet_wrap(~ IncomeCategory, scales = "free_y", nrow = 3) +
  labs(x = "Year", y = "Living Planet Index") +
  scale_color_manual(values = c("green", "blue", "red"), labels = c("High", "Middle", "Low")) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        plot.margin = margin(30, 30, 30, 30, "pt"),
        axis.title.x = element_text(margin = margin(t = 20, unit = "pt")),
        axis.title.y = element_text(margin = margin(r = 20, unit = "pt")),
        strip.text = element_text(size = 18))  # Adjust the size as desired