

cherry_data3 <- c()
tbls_ls3 <- c()
for (X in 1:554){
  for (i in 1972:2023) {
    cherry_blossom <- read_html(paste("https://www.cballtimeresults.org/performances?division=Overall+Women&page=",X,
                                      "&section=10M&sex=W&utf8=%E2%9C%93&year=",i,sep=""))
    tbls_ls3 <- as.data.frame(cherry_blossom %>%
                                html_table(fill = TRUE))
    cherry_data3 <- rbind(cherry_data3, tbls_ls3)
  }
}


cherry_dataMen <- c()
tbls_Men <- c()
for (X in 1:554){
  for (i in 1972:2023) {
    cherry_blossom <- read_html(paste("https://www.cballtimeresults.org/performances?division=Overall+Men&page=",X,
                                      "&section=10M&sex=M&utf8=%E2%9C%93&year=",i,sep=""))
    tbls_Men <- as.data.frame(cherry_blossom %>%
                                html_table(fill = TRUE))
    cherry_dataMen <- rbind(cherry_dataMen, tbls_Men)
  }
}
