### How Much Should We Trust Estimates from Multiplicative Interaction Models?
### Jens Hainmueller, Jonathan Mummolo, Yiqing Xu



library(foreign)
library(interflex)

# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"

## set graph folder
graphpath<-paste(path,"Graphs/",sep="")
source(paste(path,"Code/hmx.R",sep=""))



## prepare the data
d<-read.dta(paste0(path,"Data/Chapman_IO_2009/rep_chapman_2009.dta"),convert.factor=FALSE)
name<-"chapman_2009"
head(d)
dim(d)
Y="rally" 
D="unauth" 
X="S" 
Z<-c("priorpop","bipart","statemnt","nextelec","nytcov","buscon","revorg","wardumk","majopp","allies","war","SCappeal","regorgact","ordsever","hostlvl")
Dlabel<-"UN authorization"
Xlabel<-"US affinity with UN Security Council"
Ylabel<-"rallies"
vartype<-"robust"
cl<-NULL
cuts<-cuts2<-time<-NULL
hist(d[,Y])
table(d[,D]) 
table(d[,X]) 
main<-"Chapman (2009) \n IO"

table(d[d[,D]==1, X])
d2<-subset(d, d[,X]>=-0.514705896377563 & d[,X]<=-0.46000000834465)
dim(d2)

replicate(d, X=X, Y=Y, D=D, Z=Z)

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=0,span=1,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=0.3,smooth.lim=c(-300,200), raw.pos=c(-65,80))



