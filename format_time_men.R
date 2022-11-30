#code for missing time is -999.0

rm(list = ls())
setwd('~/caffeinated_MJM')

df = read.csv('data/combined_data_with_ID.csv')

t = df$Time[1]
i=1
format_time = function(t)
{
  if(t == 'NR' || t=='')
    return(NA)
  hours = substr(t,1,2)
  minute_format = T
  if(grepl(':',hours))
  {
    minute_format =F
    #print(t)
  }
 
  if(minute_format==F)
  {
    hours = substr(t,1,1)
    minutes = substr(t, 3,4)
    seconds = substr(t,6,7)
   
  }
  else
  {
    if(grepl('0.',substr(t,1,2)))
    {
      return(NA)
    }
    hours = 0
    minutes = substr(t,1,2)
    seconds = substr(t,4,5)
    
    #print(seconds)
  }
  total.time = 3600*as.double(hours) + 60*as.double(minutes) + as.double(seconds)
  if(t !='' && total.time < 2000)
  {
   # print(t)
  #  print(i)
  }
  assign("i",i+1, envir=.GlobalEnv)
  return(total.time)
}  
#df = df[!is.na(df$Time),]Time = s
Time = sapply(df$Time,format_time)
df$Time = Time
write.csv(df,'data/all_data_formatted_ID.csv',row.names=F)