imv.binary<-function(y, #outcomes
                     p1,#baseline
                     p2, #enhanced
                     sigma=1e-4 #sigma ensures no numerical computation problems
                     ) {
    ##
    p1<-ifelse(p1<sigma,sigma,p1)
    p2<-ifelse(p2<sigma,sigma,p2)
    p1<-ifelse(p1>1-sigma,1-sigma,p1)
    p2<-ifelse(p2>1-sigma,1-sigma,p2)
    ##
    ll<-function(x,p) {
        z<-log(p)*x+log(1-p)*(1-x)
        z<-sum(z)/length(x)
        exp(z)
    }    
    loglik1<-ll(y,p1)
    loglik2<-ll(y,p2)
    getcoins<-function(a) {
        f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
        nlminb(.5,f,lower=0.001,upper=.999,a=a)$par
    }
    c1<-getcoins(loglik1)
    c2<-getcoins(loglik2)
    ew<-function(p1,p0) (p1-p0)/p0
    imv<-ew(c2,c1)
    imv
}
