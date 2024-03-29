

makeresponse<-function(x) {
    #make IR matrix
    nms<-unique(x$item)
    if (all(nms %in% 1:length(nms))) x$item<-paste("item_",x$item,sep='')
    ##make response matrix
    id<-unique(x$id)
    L<-split(x,x$item)
    out<-list()
    for (i in 1:length(L)) {
        z<-L[[i]]
        index<-match(z$id,id)
        resp<-rep(NA,length(id))
        resp[index]<-z$resp
        out[[i]]<-resp
    }
    resp<-do.call("cbind",out)
    resp<-data.frame(resp)
    names(resp)<-names(L)
    resp$id<-id
    nr<-apply(resp,2,function(x) length(table(x)))
    resp<-resp[,nr>1]
    resp<-resp[rowSums(!is.na(resp))>1,]
    resp
}

imv0mirt<-function(mod,
                   nfold=5,
                   fscores.options=(list(method="EAP"))
                   )
{
    x<-mod@Data$data
    id<-1:nrow(x)
    L<-list()
    for (i in 1:ncol(x)) L[[i]]<-data.frame(id=id,item=colnames(x)[i],resp=x[,i])
    x<-data.frame(do.call("rbind",L))
    x$group<-sample(1:nfold,nrow(x),replace=TRUE)
    ##
    call<-mod@Call
    call<-deparse(call)
    call<-gsub("data = data","data = train",call)
    call<-parse(text=call)
    ##
    om<-numeric()
    for (i in 1:nfold) {
        train<-makeresponse(x[x$group!=i,])
        id<-train$id
        train$id<-NULL
        mm<-eval(call)
        th<-do.call("fscores",c(list(object=mm),fscores.options))
        test<-x[x$group==i,]
        ll<-list()
        items<-unique(test$item)
        for (j in 1:length(items)) {
            it<-items[j]
            it<-extract.item(mm,it)
            pp<-probtrace(it,th[,1])
            ll[[j]]<-data.frame(id=id,item=names(coef(mm))[j],pr=pp[,2])
        }
        y<-data.frame(do.call("rbind",ll))
        y<-merge(test,y,all.x=TRUE)
        y$p0<-mean(x$resp[x$group!=i],na.rm=TRUE)
        y<-y[!is.na(y$pr),]
        om[i]<-imv.binary(y$resp,y$p0,y$pr)
    }
    om
}    
