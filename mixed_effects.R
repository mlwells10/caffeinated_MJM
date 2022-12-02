rm(list = ls())
setwd('~/caffeinated_MJM/')


library(nlme)
library(AICcmodavg)
library(MuMIn)
df = read.csv('data/all_data_formatted.csv')
df[df=='NR'] = NA
df$Age = as.numeric(df$Age)
df = df[!is.na(df$Age)&!is.na(df$Time),]
#df = df[df$Time > 70.6 & df$Time < 114.5,]

#bx_cx = boxcox(Time ~ Age, data =df, lambda = seq(-2,5,1.5))
#lambda_M = bx_cx$x[which.max(bx_cx$y)]
#df$yboxcox = (df$Time^lambda_M-1)/lambda_M

dg = df[df$num_obs > 4,]

c=1
#dg = dg[dg$Time<75,]
dg = dg[dg$Age > 10,]
dg = dg[dg$gender=='M',]

model = lme(Time ~ Age, data=dg,random=~1 + Age | ID)
print(summary(model))
print(r.squaredGLMM(model))
xstar = seq(from = 20, to = 65, by = 1)


beta = model$coefficients$fixed
plot(xstar,beta[2]*xstar + beta[1],type='l',ylim=c(40,120),xlab='Age',ylab='Time')
ID.1 = 2319
ID.2 = 43
ID.3 = 699
ID.4 = 5215
ID.5 = 5537
y1 = dg[dg$ID==ID.1,c('Age','Time')]
y2 = dg[dg$ID==ID.2,c('Age','Time')]
y3 = dg[dg$ID==ID.3,c('Age','Time')]
y4 = dg[dg$ID==ID.4,c('Age','Time')]
y5 = dg[dg$ID==ID.5,c('Age','Time')]


points(y1[,1],y1[,2],col=1,pch=16)
points(y2[,1],y2[,2],col=2,pch=16)
points(y3[,1],y3[,2],col=3,pch=16)
points(y4[,1],y4[,2],col=4,pch=16)
points(y5[,1],y5[,2],col=5,pch=16)
alpha.2 = model$coefficients$random$ID[which(sort(ID)==ID.2),]
alpha.1 = model$coefficients$random$ID[which(sort(ID)==ID.1),]
alpha.3 = model$coefficients$random$ID[which(sort(ID)==ID.3),]
alpha.4 = model$coefficients$random$ID[which(sort(ID)==ID.4),]
alpha.5 = model$coefficients$random$ID[which(sort(ID)==ID.5),]
lines(y1[,1],(beta[2]+alpha.1[2])*y1[,1]+beta[1]+alpha.1[1],col=1)
lines(y2[,1],(beta[2]+alpha.2[2])*y2[,1]+beta[1]+alpha.2[1],col=2)
lines(y3[,1],(beta[2]+alpha.3[2])*y3[,1]+beta[1]+alpha.3[1],col=3)
lines(y4[,1],(beta[2]+alpha.4[2])*y4[,1]+beta[1]+alpha.4[1],col=4)
lines(y5[,1],(beta[2]+alpha.5[2])*y5[,1]+beta[1]+alpha.5[1],col=5)
stop()
K = function(x,y)
{
  return(outer(x,y,function(s,t){(s*t+c)^2}))
}
x.i = dg$Age
f = function(x,alpha)
{
  return(K(x,x.i)%*%alpha)
}
#dg$Age = (dg$Age-mean(dg$Age))/sd(dg$Age)
#dg$Time = (dg$Time-mean(dg$Time))/sd(dg$Time)
K.mat = K(dg$Age,dg$Age)
n = nrow(dg)
lambda = 0.0001
y = dg$Time
alpha = solve(K.mat + n*lambda*diag(n),y)

x.star = seq(from=min(dg$Age),to = max(dg$Age),by=1)
plot(x.star,f(x.star,alpha),type='l',ylim=c(40,150),xlab='age',ylab='time',lwd=2,col='blue')
points(dg$Age,dg$Time)
model=lm(Time ~ Age, dg)
print('linear model:')
print(sum(resid(model)^2))
print('KRR:')
print(sum((f(dg$Age,alpha)-dg$Time)^2))
age = sort(unique(dg$Age))
mean.time = rep(0,length(age))
sd.time = rep(0,length(age))
for(k in 1:length(age))
{
  mean.time[k] = mean(dg$Time[dg$Age==age[k]])
  sd.time[k] = sd(dg$Time[dg$Age==age[k]])
}

idx = apply(outer(dg$Age,age,function(x,y){abs(x-y)}),1,which.min)
print('Mean:')
print(sum((dg$Time-mean.time[idx])^2))
stop()
age = sort(unique(df$Age))
mean.time = rep(0,length(age))
sd.time = rep(0,length(age))
for(k in 1:length(age))
{
  mean.time[k] = mean(df$Time[df$Age==age[k]])
  sd.time[k] = sd(df$Time[df$Age==age[k]])
}
plot(age,mean.time,type='l',lwd=2,ylim=c(20,220))
lines(age,mean.time-sd.time)
lines(age,mean.time+sd.time)

x.i = age
sigma = 10
K = function(x,y)
{
  return(outer(x,y,function(s,t){(s*t+c)^2}))
}
f = function(x,alpha)
{
  return(K(x,x.i)%*%alpha)
}
#dg$Age = (dg$Age-mean(dg$Age))/sd(dg$Age)
#dg$Time = (dg$Time-mean(dg$Time))/sd(dg$Time)
K.mat = K(age,age)
n = length(age)
lambda = 0.0000001
y = mean.time
alpha = solve(K.mat + n*lambda*diag(n),mean.time)

x.star = seq(from=min(age),to = max(age),by=1)
plot(x.star,f(x.star,alpha),type='l',ylim=c(0,220),xlab='age',ylab='time',lwd=2)
lines(age,mean.time,col='blue')