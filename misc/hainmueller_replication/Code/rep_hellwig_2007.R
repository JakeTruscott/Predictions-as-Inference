### How Much Should We Trust Estimates from Multiplicative Interaction Models?
### Jens Hainmueller, Jonathan Mummolo, Yiqing Xu


library(foreign)
library(interflex)
# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
## set graph folder
graphpath<-paste(path,"Graphs/",sep="")
source(paste(path,"Code/hmx.R",sep=""))

## First interaction


d<-read.dta(paste0(path,"Data/Hellwig_Samuels_CPS_2007/rep_hellwig_2007a.dta"), convert.factors=FALSE)
name<-"hellwig_2007a"
head(d)
dim(d)
Y="incvotet" 
D="dgdp" 
X="tradeshr" 
Z<-c("incvotet1", "electype" ,"gdpxelectype" ,"presrun", "enlp" ,"income" ,"regafrica", "regasia" ,"regcee" ,"reglatam")
Dlabel<-"Economy"
Xlabel<-"Trade as Share of GDP"
Ylabel<-"Election"
table(d[,D], exclude=NULL) 
table(d[,X], exclude=NULL) 
quantile(d[,X],probs=seq(0,1,1/3))
vartype<-"cluster"
cl<-"code"
cuts<-cuts2<-time<-NULL
main<-"Hellwig & Samuels (2007) \n CPS"


##Stata: regress incvotet incvotet1 dgdp tradeshr gdpxtradeshr electype gdpxelect presrun enlp income regafrica regasia regcee reglatam, cluster(code)


## Table 1 Model 1 Figure 1
plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         smooth.lim=c(-3,3), bandwidth = 1.838)



####################
## Second interaction
####################

d<-read.dta(paste0(path,"Data/Hellwig_Samuels_CPS_2007/rep_hellwig_2007b.dta"), convert.factors=FALSE)
name<-"hellwig_2007b"
head(d)
dim(d)
Y="incvotet" 
D="dgdp" 
X="grosscap" 
Z<-c("incvotet1", "electype" ,"gdpxelectype" ,"presrun", "enlp" ,"income" ,"regafrica", "regasia" ,"regcee" ,"reglatam")
Dlabel<-"Economy"
Xlabel<-"Capital Flows as Share of GDP"
Ylabel<-"Election"
vartype<-"cluster"
table(d[,D], exclude=NULL) 
table(d[,X], exclude=NULL) 
hist(as.numeric(d[,X])) 
cl<-"code"
cuts<-cuts2<-time<-NULL
quantile(d[,X],probs=seq(0,1,1/3))
main<-"Hellwig & Samuels (2007) \n CPS"

##Stata: regress incvotet incvotet1 dgdp grosscap gdpxgrosscap electype gdpxelect presrun enlp income regafrica regasia regcee reglatam, cluster(code)


## Table 1 Model 1 Figure 2
replicate(d, X=X, Y=Y, D=D, Z=Z)

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=1,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=4.688, interval=c(0,0.5),smooth.lim=c(-25,8))

