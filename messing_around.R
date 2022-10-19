
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)




Wdata <- read_csv("newdata.csv")
View(newdata)


Wdata$Age <- as.numeric(Wdata$Age )
newdata <- Wdata %>% 
  filter(!is.na(Wdata$Age))
newdata$Time <- strptime(newdata$Time, "%H:%M:%S")
newdata$Time <- hms::as_hms(newdata$Time)

head(newdata)



new_newdata <- newdata %>% 
  filter(is.na(Time) == F)




x <- new_newdata %>%
  group_by(Race) %>% 
  summarize(mean_run = mean(Time, trim=0.10)) %>% 
  print(n=Inf)



df_x <- as.data.frame(x)

ggplot(df_x, aes(x = Race, y=mean_run)) +
  geom_point() 
  

new_newdata %>% 
  filter(Race == 1978) %>% 
  print(n=Inf)

#Only 23 observations for this year?





