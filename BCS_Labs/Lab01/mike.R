setwd('~./caffeinated_MJM/BCS_Labs/Lab01/')

load('CBdata.1_10.RData')

dat.76 = CBdata.1_10[[4]]

n = ncol(dat.76)

dat.76 = dat.76[,-6]


idx = c()
for(i in 1:nrow(dat.76))
{
  if(is.na(dat.76$Age[i]))
  {
    idx = c(idx, F)
  }else{
    idx = c(idx, T)
  }
}
dat.76.clean = dat.76[idx,]
dat.76.clean2 = dat.76[!is.na(dat.76$Age),]
print(identical(dat.76.clean,dat.76.clean2))

