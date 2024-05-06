library(imv)
x<-rnorm(1000)
y<-rbinom(length(x),1,1/(1+exp(-x)))
df<-data.frame(x=x,y=y)
m<-glm(y~x,df,family="binomial")

new.y<-rbinom(length(x),1,1/(1+exp(-x)))
pr<-predict(m,data.frame(x=x),type='response')
imv.binary(new.y,mean(y),pr) #imv for new set of outcomes

imv0glm(m) #imv for full model versus null model

df$z<-rnorm(nrow(df))
m<-glm(y~x+z,df,family="binomial")
#summary(m)
imvglm.rmvar(m,var.nm='z')

z<-round(df$z)
df$z<-as.factor(ifelse(z<0 | z>2,0,z))
m1<-glm(y~x,df,family="binomial")
m2<-glm(y~x+z,df,family="binomial")
imv0glm(m2)
imvglm.rmvar(m2,var.nm='z')
imvglm(m1,m2)



#######################
##MIRT

library(mirt)
library(imv)
data <- expand.table(LSAT7)
mod1 <- mirt(data, 1)
imv0mirt(mod1)
