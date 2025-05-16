### How Much Should We Trust Estimates from Multiplicative Interaction Models?
### Jens Hainmueller, Jonathan Mummolo, Yiqing Xu


library(foreign)
library(interflex)
# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
## set graph folder
graphpath<-paste(path,"Graphs/",sep="")
source(paste(path,"Code/hmx.R",sep=""))



d<-read.dta(paste0(path,"Data/Tavits_CPS_2007/rep_tavits_2008.dta"), convert.factors=F)
name<-"tavits_2008"
head(d)
dim(d)
Y="voteslost" 
D="neighbor" 
X="importance" 
Z<-c("yearsdem" ,"turn_ch" ,"govt", "unemp", "gdp" ,"ln_mdm", "ln_infl", "votes_last" ,"c1" ,"c2", "c3" ,"c4", "c5" ,"c6" ,"c7" ,"c8", "c9")
Dlabel<-"Neighbor"
Xlabel<-"Issue Importance"
Ylabel<-"Vote Loss"
vartype<-"robust"
table(d[,D], exclude=NULL) ##dummy
table(d[,X], exclude=NULL) ##continuous
cuts<-NULL
cuts2<-NULL
time<-NULL
cl<-NULL
main<-"Tavits (2008) \n CPS"

replicate(data=d, X=X, Y=Y, Z=Z, D=D)#replicates Table 1 Model 2


plot.all(data=d, graphpath=graphpath, name=name, main=main,
         smooth=TRUE, Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel,
         Dlabel=Dlabel, Xlabel=Xlabel, cuts=cuts, cuts2=cuts2,
         vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=16.222, smooth.lim=c(-0.5,0.5))

