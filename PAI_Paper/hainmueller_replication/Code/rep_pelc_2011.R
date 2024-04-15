### How Much Should We Trust Estimates from Multiplicative Interaction Models?
### Jens Hainmueller, Jonathan Mummolo, Yiqing Xu

## Takes 1.26 hours with 8 cores

library(foreign)
library(interflex)
# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
## set graph folder
graphpath<-paste(path,"Graphs/",sep="")
source(paste(path,"Code/hmx.R",sep=""))



######################################
##first interaction: Table 4 Column 2
######################################

##Stata: reg depth3 logGDPconst logfullimports logfullexports accessionperiod loggdpcap MFNpre polity3 chinadum membershiptiming polityXimp polityXexp, cluster(reporter)

d<-read.dta(paste0(path,"Data/Pelc_IO_2011/rep_pelc_2011a.dta"), convert.factors=F)
name<-"pelc_2011a"
head(d)
dim(d)
Y="depth3" 
D="polity3" 
X="logfullimports" 
Z<-c("logGDPconst", "logfullexports" , "accessionperiod", "loggdpcap" ,
     "MFNpre", "chinadum" , "membershiptiming","polityXexp")
Dlabel<-"Regime Type"
Xlabel<-c("Imports")
Ylabel<-"de facto Depth"
vartype<-"cluster"
cl<-"reporter"
cuts<-cut2<-time<-NULL
main<-"Pelc (2011) \n IO"

##replicate(data=d, X=X, D=D, Y=Y, Z=Z)

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=1,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel,
         Xlabel=Xlabel, cuts=cuts, cuts2=cuts, vartype=vartype, cl=cl,
         time=time, pairwise=TRUE, cores = 24, bandwidth=1.195,
         nboots = 200, neval = 20, smooth.lim=c(-0.6,1.0))

######################################
##second interaction: Table 4 Column 3
######################################

##Stata: reg overhang logGDPconst logfullimports logfullexports accessionperiod loggdpcap MFNpre polity3 chinadum membershiptiming polityXimp polityXexp, cluster(reporter)
d<-read.dta(paste0(path,"Data/Pelc_IO_2011/rep_pelc_2011b.dta"), convert.factors=F)
name<-"pelc_2011b"
head(d)
dim(d)
Y="overhang" 
D="polity3" 
X="logfullimports" 
Z<-c("logGDPconst", "logfullexports" , "accessionperiod", "loggdpcap" ,"MFNpre", "chinadum" , "membershiptiming","polityXexp")
Dlabel<-"Regime Type"
Xlabel<-c("Imports")
Ylabel<-"Overhang"
vartype<-"cluster"
cl<-"reporter"
table(d[,D], exclude=NULL) 
table(d[,X], exclude=NULL) 
quantile(d[,X],probs=seq(0,1,1/3))

##replicate(data=d, X=X, Y=Y, D=D, Z=Z)

## This takes a long time
plot.all(data=d, graphpath=graphpath, name=name, main=main,
         smooth=TRUE, Y=Y, D=D, X=X,  Z=Z,  Ylabel=Ylabel,
         Dlabel=Dlabel, Xlabel=Xlabel, cuts=cuts, cuts2=cuts,
         vartype=vartype, cl=cl, time=time, pairwise=TRUE, cores = 4,
         bandwidth=1.195,nboot=200,neval=20, smooth.lim=c(-1.5,1.0))










