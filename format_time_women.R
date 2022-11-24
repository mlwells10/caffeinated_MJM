#code for missing time is -999.0

rm(list = ls())
setwd('~/caffeinated_MJM')

df = read.csv('data.csv')

t = df$Time[1]

format_time = function(t)
{
  if(t == 'NR')
    return(-999)
  hours = substr(t,1,2)
  minute_format = T
  if(grepl(':',hours))
    minute_format =F
  if(minute_format==F)
  {
    hours = substr(t,1,1)
    minutes = substr(t, 3,4)
    seconds = substr(t,6,7)
   
  }
  else
  {
    hours = 0
    minutes = substr(t,1,2)
    seconds = substr(t,4,5)
    #print(seconds)
  }
  total.time = 3600*as.double(hours) + 60*as.double(minutes) + as.double(seconds)
  return(total.time)
}  
#df = df[!is.na(df$Time),]Time = s
Time = sapply(df$Time,format_time)
df$Time = Time
write.csv(df,'data/women_time_formatted.csv',row.names=F)