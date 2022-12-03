rm(list = ls())
setwd('~/caffeinated_MJM/')

df = read.csv('data/all_data.csv')
#dg = read.csv('newdata.csv')
#gender = rep('M',nrow(df))
#cbind(df,gender)
#gender = rep('F',nrow(dg))
#cbind(dg,gender)
#df = rbind(df,dg)
df = df[df$Hometown!='NR',]
name.hometown = unique(df[,c('Name','Hometown')])

k=1
ID = rep(0,nrow(df))
num.obs = rep(0,nrow(df))
for(k in 1:nrow(name.hometown))
{
  id = name.hometown[k,]
  #df.id = df[df$Name%in%id[1]&df$Hometown%in%id[2],]
  ID[df$Name%in%id[1]&df$Hometown%in%id[2]] = k
  num.obs[ID==k] = length(ID[ID==k])
  if(k%%1000==0)
    print(k)
}

#df = cbind(df,ID)
df$ID = ID
df$num_obs = num.obs
write.csv(df,'data/all_dta_formatted.csv',row.names=F)
