
imv0glm<-function(m,nfold=5) {
    x<-m$model
    if (nfold>nrow(x)) stop()
    x$group<-sample(1:nfold,nrow(x),replace=TRUE)
    foldfun<-function(train,test,m) {
        mm<-update(m,data=train)#glm(fm,train,family="binomial")
        fm<-m$formula
        new.fm<-paste(all.vars(fm)[1],"~1")
        m0<-update(m,formula.=as.formula(new.fm),data=train)#glm(,train,family="binomial")
        ##
        p0<-predict(m0,test,type="response")
        p1<-predict(mm,test,type="response")
        om<-imv_binary(test[[all.vars(fm)[1] ]],p0,p1)
        om
    }
    L<-split(x,x$group)
    om<-numeric()
    for (i in 1:length(L)) {
        test<-L[[i]]
        train<-data.frame(do.call("rbind",L[-i]))
        om[i]<-foldfun(train=train,test=test,m=m)
    }
    om
}


imvglm<-function(m,nfold=5,
                 var.nm #name of variable you want to remove
                 ) {
    x<-m$model
    if (nfold>nrow(x)) stop()
    x$group<-sample(1:nfold,nrow(x),replace=TRUE)
    foldfun<-function(train,test) {
        mm<-update(m,data=train)#glm(fm,train,family="binomial")
        fm<-m$formula
        new.fm<-update.formula(fm,as.formula(paste(".~. - ",var.nm,sep='')))
        m0<-update(m,formula.=as.formula(new.fm),data=train)#glm(,train,family="binomial")
        ##
        p0<-predict(m0,test,type="response")
        p1<-predict(mm,test,type="response")
        om<-imv_binary(test[[all.vars(fm)[1] ]],p0,p1)
        om
    }
    L<-split(x,x$group)
    om<-numeric()
    for (i in 1:length(L)) {
        test<-L[[i]]
        train<-data.frame(do.call("rbind",L[-i]))
        om[i]<-foldfun(train=train,test=test)
    }
    om
}
