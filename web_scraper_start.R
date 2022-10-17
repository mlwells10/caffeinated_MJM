install.packages("rvest")
library(rvest)
library(tidyverse)

cherry_data <- c()
tbls_ls <- c()
for (X in 1:417){
  cherry_blossom <- read_html(paste("https://www.cballtimeresults.org/performances?division=Overall+Women&page=",X,"&section=10M&sex=W&utf8=%E2%9C%93&year=2022",sep=""))
  tbls_ls <- as.data.frame(cherry_blossom %>%
                 html_table(fill = TRUE))
  cherry_data <- rbind(cherry_data, tbls_ls)
}

head(cherry_data)


