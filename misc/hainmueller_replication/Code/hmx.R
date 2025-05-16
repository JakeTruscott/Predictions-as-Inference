## Jens Hainmueller; Jonathan Mummolo; Yiqing Xu
## This Version: 2015.12.05


## 1. replicate
## 2. plot.all: produce all plots



################# replicate ###########################

replicate<-function(data,Y, D, X, Z=NULL, FE=NULL, weight=NULL){
    f1<-paste(Y,"~",D)
    inter<-paste(X,D,sep="*")

    ## parsing fixed effects
    if (is.null(FE)==FALSE) {
        if (is.null(Z)==TRUE) {Z<-c()}
        for (i in 1:length(FE)) {
            Z<-c(Z,paste("as.factor(",FE[i],")",sep=""))
        } 
    }
    form<-paste(f1, inter, sep="+" )
    if (length(Z)>0) {
        form <- paste(form, paste(Z, collapse = " + "), sep = "+")
    } 
    
    if(is.null(weight)==TRUE){
        m<-summary(lm(form, data=data))
    } else {
        data.aug<-data
        data.aug$weights<-data[,weight]
        m<-summary(lm(form, data=data.aug, weights=weights))
    }
    
    return(m)
    
}


############# plot all ##################

plot.all<-function(data, graphpath, name, main, Y, D, X, Z=NULL, FE=NULL,
                   Ylabel=NULL, Dlabel=NULL, Xlabel=NULL, 
                   smooth=TRUE, cuts=NULL, cuts2=NULL,
                   vartype=NULL, cl=NULL, time=NULL, pairwise=TRUE, weights=NULL,
                   bandwidth=NULL,smooth.lim=NULL,smooth.cl=TRUE, nboots=200,
                   grid=20,neval=50,span=NULL,interval=NULL, cores=6,
                   raw.pos=NULL, density=T,
                   metric = "MSPE", wald = TRUE) {

    options(warn=-1)
    set.seed(123)
    library(ggplot2)
    begin.time<-Sys.time()
        
    if (is.null(vartype)==TRUE) {vartype<-"homoscedastic"}
    
    if (length(unique(data[,D]))==2) { # binary case
        
        cat("Step 1: scatterplot\n")
        out.raw <- inter.raw(Y=Y,D=D,X=X,data=data, Xlabel=Xlabel,
                             Ylabel=Ylabel, Dlabel=Dlabel,span=span,
                             pos=raw.pos)
        pdf(paste(graphpath,name,"_raw.pdf",sep=""),height=7,width=5)
        print(out.raw$graph)
        graphics.off()
        
        cat("Step 2: binned estimates\n")
        
        out.est1<-inter.binning(Y=Y,D=D,X=X,Z=Z,FE=FE,data=data,
                                Xlabel=Xlabel, Ylabel=Ylabel, Dlabel=Dlabel,
                                nbins=3, cutoffs=cuts, var=vartype, cl=cl, time=time,
                                pairwise=pairwise, weights=weights, Xdistr = "histogram",
                                main=main,interval=interval,ylim=smooth.lim,
                                wald=wald)
        pdf(paste(graphpath,name,"_est.pdf",sep=""))
        print(out.est1$graph)
        graphics.off()
        
        ##regenerate without title
        out.est1a <- inter.binning(Y=Y,D=D,X=X,Z=Z,FE=FE, data=data,
                                   Xlabel=Xlabel, Ylabel=Ylabel,
                                   Dlabel=Dlabel, nbins=3,
                                   cutoffs=cuts, var=vartype, cl=cl,
                                   time=time, pairwise=pairwise,
                                   weights=weights,
                                   Xdistr = "histogram",
                                   interval=interval, ylim=smooth.lim,
                                   wald=wald)
        pdf(paste(graphpath,name,"_est0.pdf",sep=""))
        print(out.est1a$graph)
        graphics.off()
        
        out.est2<-inter.binning(Y=Y,D=D,X=X,Z=Z,FE=FE,
                                data=data, Xlabel=Xlabel, Ylabel=Ylabel, Dlabel=Dlabel,
                                nbins=2, cutoffs=cuts2, var=vartype, cl=cl, time=time, pairwise=pairwise,
                                weights=weights, figure=FALSE,wald=wald)
        
        
        if (smooth==TRUE) {
            cat("Step 3: kernal smoothed estimates\n")
            if (vartype=="cluster") {cl2<-cl} else {cl2<-NULL}
            out.kernel<-inter.kernel(data=data, Y=Y,D=D,X=X,Z=Z,FE=FE,
                                     nboots=nboots,cl=cl2,cores=cores,
                                     Dlabel=Dlabel, Xlabel=Xlabel, Ylabel=Ylabel,
                                     bw=bandwidth, Xdistr = "histogram", ylim=smooth.lim,
                                     file=paste(graphpath,name,"_smooth.pdf",sep=""))
        }
        
    } else { # continuous case
        
        cat("Step 1: Scatterplot\n")
        out.raw<-inter.raw(Y=Y,D=D,X=X,data=data,Xlabel=Xlabel,
                           Ylabel=Ylabel, Dlabel=Dlabel, cutoffs=cuts,span=span)
        pdf(paste(graphpath,name,"_raw.pdf",sep=""),height=4,width=10)
        print(out.raw$graph)
        graphics.off()
        
        cat("Step 2: GAM plot\n")
        pdf(paste(graphpath,name,"_gam.pdf",sep=""))
        inter.gam(Y=Y,D=D,X=X,Z=NULL,FE=NULL,data=data,SE=0, weights=weights)
        graphics.off()
        
        cat("Step 3: Binning estimates\n")
        out.est1<-inter.binning(Y=Y,D=D,X=X,Z=Z,FE=FE,
                                data=data,Xlabel=Xlabel, Ylabel=Ylabel, Dlabel=Dlabel,
                                nbins=3, cutoffs=cuts, var=vartype, cl=cl, time=time, pairwise=pairwise,
                                weights=weights, Xdistr = "histogram",
                                main=main,interval=interval,ylim=smooth.lim,
                                wald=wald)
        pdf(paste(graphpath,name,"_est.pdf",sep=""))
        print(out.est1$graph)
        graphics.off()
        
        out.est1a<- inter.binning(Y=Y,D=D,X=X,Z=Z,FE=FE, data=data,
                                  Xlabel=Xlabel, Ylabel=Ylabel,
                                  Dlabel=Dlabel, nbins=3,cutoffs=cuts,
                                  var=vartype,cl=cl,time=time,
                                  pairwise=pairwise, weights=weights,
                                  Xdistr = "histogram",
                                  interval=interval, ylim=smooth.lim,
                                  wald=wald)
        pdf(paste(graphpath,name,"_est0.pdf",sep=""))
        print(out.est1a$graph)
        graphics.off()
        
        out.est2<-inter.binning(Y=Y,D=D,X=X,Z=Z,FE=FE,
                                data=data, Xlabel=Xlabel, Ylabel=Ylabel, Dlabel=Dlabel,
                                nbins=2, cutoffs=cuts2, var=vartype,cl=cl,time=time, pairwise=pairwise,
                                weights=weights, figure=FALSE, wald=wald)
        
        
        if (smooth==TRUE) {
            cat("Step 4: Kernal smoothed estimates\n")
            if (vartype=="cluster" & smooth.cl==TRUE) {cl2<-cl} else {cl2<-NULL}
            out.kernel<-inter.kernel(Y=Y,D=D,X=X,Z=Z,FE=FE, data=data,
                                     nboots=nboots, grid=grid,cl=cl2,
                                     cores=cores, neval = neval,
                                     Ylabel=Ylabel,Dlabel=Dlabel,
                                     Xlabel=Xlabel, bw=bandwidth,
                                     Xdistr = "histogram",
                                     metric = "MSPE", ylim=smooth.lim,
                                     file=paste(graphpath,name,"_smooth.pdf",sep=""))
        } 
    }
    
    cat("\n### Statistics ###\n")
    output<-list(binary.treatment=out.est1$binary.treatment,
                 treat.variation=out.est1$treat.variation.byGroup,
                 LKurtosis = out.est1$X.Lkurtosis,
                 correct.order=out.est1$correctOrder,
                 three.bin.coefs=out.est1$est.binning[,2],
                 three.bin.pvalues=out.est1$p.twosided,
                 two.bin.pvalues=out.est2$p.twosided,
                 wald = out.est1$p.wald)
                 ## betas = out.est1$betas,
    ## p.ftest = out.est1$p.ftest)
    if (smooth==TRUE) {
        output<-c(output, list(CV.out = out.kernel$CV.out,
                               bw = round(out.kernel$bw,3)))
    }
    options(warn=0)
    print(Sys.time()-begin.time)
    return(output)
    
}




