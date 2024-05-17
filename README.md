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
imv.binary(new.y,mean(y),pr) #imv for new set of outcomes

imv0glm(m) #imv for full model versus null model

df$z<-rnorm(nrow(df))
m<-glm(y~x+z,df,family="binomial")
#summary(m)
imvglm(m,var.nm='z')

#######################
##MIRT

library(mirt)
library(imv)
resp <- expand.table(LSAT7)
mod1 <- mirt(resp, 1,'Rasch')
imv.mirt.compare(mod1) #compared to item-level means


##with IRW data
library(irw) 
dataset <- redivis::user("datapages")$dataset("item_response_warehouse")
df <- dataset$table("kim2023")$to_data_frame()
items<-unique(df$item)
if (all(items %in% 1:length(items))) {
    df$item<-paste("item_",df$item,sep='')
    items<-unique(df$item)
}
resp<-irw::long2resp(df)
id<-resp$id
resp$id<-NULL
mod1<-mirt(resp,1,'Rasch',verbose=FALSE)
##2pl
ni<-ncol(resp)
s<-paste("F=1-",ni,"
         PRIOR = (1-",ni,", a1, lnorm, 0.0, 1.0)",sep="")
model<-mirt.model(s)
mod2<-mirt(resp,model,itemtype=rep("2PL",ni),method="EM",technical=list(NCYCLES=10000),verbose=FALSE)
imv.mirt.compare(mod1,mod2)

```
