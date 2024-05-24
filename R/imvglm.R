imv0glm<-function(m,nfold=5) {
    x<-m$data
    ##get complete cases
    tmp<-x[,all.vars(m$formula)]
    x<-x[rowSums(is.na(tmp))==0,]
    ##
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
        om<-imv.binary(test[[all.vars(fm)[1] ]],p0,p1)
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

imv.glm.rmvar<-function(m,nfold=5,
                 var.nm #name of variable you want to remove
                 ) {
    x<-m$data
    ##get complete cases
    tmp<-x[,all.vars(m$formula)]
    x<-x[rowSums(is.na(tmp))==0,]
    ##
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
        om<-imv.binary(test[[all.vars(fm)[1] ]],p0,p1)
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

imv.glm<-function(m1,
                  m2=NULL,
                  var.nm=NULL,
                  nfold=5) {
    x<-m1$data
    if (is.null(m2)) return(imv0glm(m=m1,nfold=nfold))
    if (!is.null(var.nm)) return(imv.glm.rmvar(m=m1,var.nm=var.nm,nfold=nfold))
    xx<-m2$data
    if (!all.equal(x,xx)) stop("bad data") 
    ##get complete cases
    tmp<-x[,all.vars(m1$formula)]
    x<-x[rowSums(is.na(tmp))==0,]
    ##
    if (nfold>nrow(x)) stop()
    x$group<-sample(1:nfold,nrow(x),replace=TRUE)
    foldfun<-function(train,test,m1,m2) {
        mm1<-update(m1,data=train)#glm(fm,train,family="binomial")
        mm2<-update(m2,data=train)
        ##
        p0<-predict(mm1,test,type="response")
        p1<-predict(mm2,test,type="response")
        fm<-m1$formula
        om<-imv.binary(test[[all.vars(fm)[1] ]],p0,p1)
        om
    }
    L<-split(x,x$group)
    om<-numeric()
    for (i in 1:length(L)) {
        test<-L[[i]]
        train<-data.frame(do.call("rbind",L[-i]))
        om[i]<-foldfun(train=train,test=test,m1=m1,m2=m2)
    }
    om
}
