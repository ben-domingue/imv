# imv
See:

https://osf.io/preprints/socarxiv/gu3ap/

https://psyarxiv.com/q3djt/

Examples

```
library(imv)
x<-rnorm(1000)
y<-rbinom(length(x),1,1/(1+exp(-x)))
df<-data.frame(x=x,y=y)
m<-glm(y~x,df,family="binomial")

new.y<-rbinom(length(x),1,1/(1+exp(-x)))
pr<-predict(m,data.frame(x=x),type='response')
imv_binary(new.y,mean(y),pr) #imv for new set of outcomes

imv0glm(m) #imv for full model versus null model

df$z<-rnorm(nrow(df))
m<-glm(y~x+z,df,family="binomial")
#summary(m)
imvglm(m,var.nm='z')
```
