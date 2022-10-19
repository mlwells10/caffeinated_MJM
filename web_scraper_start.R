#install.packages("rvest")
library(rvest)
library(tidyverse)

cherry_data <- c()
tbls_ls <- c()
for (X in 1:500){
  for (Y in 1973:2019){
      cherry_blossom <- read_html(paste("https://www.cballtimeresults.org/performances?division=Overall+Women&page=",X,"&section=10M&sex=&utf8=%E2%9C%93&year=",Y,sep=""))
      tbls_ls <- as.data.frame(cherry_blossom %>%
                                 html_table(fill = TRUE))
      cherry_data <- rbind(cherry_data, tbls_ls)
    }
}

head(cherry_data)

library(readr)
library(ggplot2)
library(dplyr)

mendata <- read_csv("C:/Users/justin/OneDrive/School/Stat 510 consulting/mendata.csv")

mean_race_time = c()
mean_time = c()




