# IMV
This package provides code for computing the IMV. You can read more about the IMV below:

- https://osf.io/preprints/socarxiv/gu3ap/

- https://psyarxiv.com/q3djt/

You can install this package via the following command in R:
```
devtools::install_github("ben-domingue/imv", ref="main")
```
## GLM examples
We'll first consider some basic examples using the standard `glm()` function in R. We'll begin by simulating a binary outcome `y` based on values of some independent variable `x`. 

```
set.seed(1010)
library(imv)
x<-rnorm(1000)
y<-rbinom(length(x),1,1/(1+exp(-x)))
df<-data.frame(x=x,y=y)
```
We'll then use `glm()` to analyze the relationship between `x` and `y`; we'll call this model `m`.
```
m<-glm(y~x,df,family="binomial")
```
Let's now generate a new set of outcomes `new.y` based on the same observations of `x`. We can use `m` to make predictions `pr` about `new.y`.
```
new.y<-rbinom(length(x),1,1/(1+exp(-x)))
pr<-predict(m,data.frame(x=x),type='response')
```
We finally come to the calcluation of the IMV. Let's calculate the IMV for predictions `pr` compared to those based on `mean(y)`.
```
imv.binary(new.y,mean(y),pr) #imv for new set of outcomes
```
How should we interpret this value of `0.44`? In Table 1 [here](https://osf.io/preprints/socarxiv/gu3ap) we provide a range of values from empirical studies that this value could be compared to. 

Alternatively, we can compute a cross-fold IMV based on just the model `m`.
```
imv0glm(m) #imv for full model versus null model
```
Finally, we can use the IMV to quantify the role of an individual predictor. We'll do that here by adding a predictor `z` that is unassociated with the outcomes `y`.
```
df$z<-rnorm(nrow(df))
m<-glm(y~x+z,df,family="binomial")
#summary(m)
imvglm.rmvar(m,var.nm='z')
```
These values are, not surprisingly, very near zero!

## IRT examples

We'll now turn to analysis of item response outcomes with the IMV. We'll use some functionality of the `mirt` package. Let's start by looking at cross-validated predictions from a simple 1PL/Rasch model as compared to predictions based on out-of-sample item means:

```
library(mirt)
resp <- expand.table(LSAT7)
mod1 <- mirt(resp, 1,'Rasch')
imv.mirt.compare(mod1) #compared to item-level means
```
As we'd expect, the 1PL/Rasch predictions that vary between-people for the same item are quite valuable relative to those that only vary between items. 

Let's now turn to a second example using data from the [IRW](https://datapages.github.io/irw/). Here we'll compare predictions from the 1PL to those from the 2PL (after imposing weak priors on the discrimination parameters for the 2PL).
```
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
You can compare these values to what we get in simulation (Figure 1) and to results from other empirical data (Figure 4) [here](https://osf.io/preprints/psyarxiv/q3djt).
