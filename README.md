# IMV
## The `imv` package
This package provides code for computing the IMV. You can install this package via the following command in R:
```
devtools::install_github("ben-domingue/imv", ref="main")
```
## Wait, what is the IMV?
The IMV is a metric for understanding the predictive differences between two models for binary outcomes. You can read more about the IMV below:

- https://osf.io/preprints/socarxiv/gu3ap/

- https://psyarxiv.com/q3djt/

Below we are going to show some examples of how the IMV can be used in logistic regression (with the `glm()` function in R) and item response theory (IRT) models. To interpret these results, it will help to offer some intuition about IMV values. Given the construction of the IMV as expected profits from a gambling scenario, we can compare values to games of chance. For example, for every $1 a casino bets on a blackjack hand, they expect to take in $0.01 in profit. This translates to an IMV of 0.01. We can also use the fact that coins are more likely to land with the same side facing up as the coin started prior to the toss (see [here](https://arxiv.org/abs/2310.04153)). If both parties bet $1 on a fair coin toss, you would expect to make $0.019 per toss if you had access to knowledge of the coin's original state; this again translates to an IMV of 0.019. 

In Table 1 [here](https://osf.io/preprints/socarxiv/gu3ap/), we also offer IMVs from a large number of prediction examples. We describe a few:
- The most predictive model of whether someone was to be evicted from a recent [prediction competition](https://www.fragilefamilieschallenge.org/) had an IMV of 0.005 relative to a simple benchmark model. Similarly, the most predictive model of layoffs relative to the standard benchmark was 0.01. 
- Having information on grip and gait relative to just age, sex, and education resulted in an IMV of 0.29 in predicting death by age 90.
- Knowledge of sex and ticket class resulted in an IMV of 0.35 in predicting death on the Titanic.

One of the advantages of the IMV is that it is portable so the values that we derive below can be directly compared to these values we have just discussed. 

## GLM examples
### A simulated example

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

### Cross-validation and the IMV
Alternatively, we can compute a cross-fold IMV based on just the model `m`.
```
imv0glm(m) #imv for full model versus null model
```
### The IMV and individual predictors
Finally, we can use the IMV to quantify the role of an individual predictor. We'll do that here by adding a predictor `z` that is unassociated with the outcomes `y`.
```
df$z<-rnorm(nrow(df))
m<-glm(y~x+z,df,family="binomial")
#summary(m)
imvglm.rmvar(m,var.nm='z')
```
These values are, not surprisingly, very near zero!

### An empirical example
We can also look at an empirical example focused on Diabetes. In [this data](https://rdrr.io/cran/mlbench/man/PimaIndiansDiabetes.html), we can look at the role of glucose tolerance in predicting diabetes. Glucose tolerance is sometimes used as a [screener](https://www.mayoclinic.org/tests-procedures/glucose-tolerance-test/about/pac-20394296) for diabetes so it should be no surprise that it is a valuable predictor.
```
set.seed(8675309)
data("PimaIndiansDiabetes", package = "mlbench")
PimaIndiansDiabetes$diabetes<-ifelse(PimaIndiansDiabetes$diabetes=='pos',1,0)
m<-glm(diabetes~glucose+mass+age,PimaIndiansDiabetes,family='binomial')
mean(imvglm.rmvar(m,var.nm='glucose'))
```
A value of `0.081` is similar to, for example, the degree to which symptoms were predictive of a COVID diagnosis in the early months of the pandemic (see value of 0.092 in Table 1 [here](https://osf.io/preprints/socarxiv/gu3ap)). 

## IRT examples
### 1PL predictions
We'll now turn to analysis of item response outcomes with the IMV. We'll use some functionality of the `mirt` package. Let's start by looking at cross-validated predictions from a simple 1PL/Rasch model as compared to predictions based on out-of-sample item means:

```
set.seed(10101)
library(mirt)
resp <- expand.table(LSAT7)
mod1 <- mirt(resp, 1,'Rasch')
imv.mirt.compare(mod1) #compared to item-level means
```
As we'd expect, the 1PL/Rasch predictions that vary between-people for the same item are quite valuable relative to those that only vary between items. 

### Comparing 2PL and 1PL predictions
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
