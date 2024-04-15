### How Much Should We Trust Estimates from Multiplicative Interaction Models?
### Jens Hainmueller, Jonathan Mummolo, Yiqing Xu


library(foreign)
library(interflex)
# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
## set graph folder
graphpath<-paste(path,"Graphs/",sep="")
source(paste(path,"Code/hmx.R",sep=""))

##Table 2 Model 2
##reg log_illit_all c.educgdp##c.parindex log_capgdpppp poprural lamerica psoviet sasia2 easia2 mideast if polity2>0, robust

## prepare the data
d<-read.dta(paste0(path,"Data/Hicken_Simmons_ajps_2008/rep_hicken_2008a.dta"),convert.factor=FALSE)
name<-"hicken_2008a"
head(d)
dim(d)
Y="log_illit_all"
D="educgdp"
X="parindex"
Z<-c("log_capgdpppp","poprural","lamerica","psoviet","sasia2","easia2","mideast")
vartype<-"robust"
cl<-NULL
Dlabel<-"Education Spending"
Xlabel<-"Parindex"
Ylabel<-"Log Illiteracy"
table(d[,D])
table(d[,X]) 
cuts<-cuts2<-time<-NULL
main<-"Hicken & Simmons (2008) \n AJPS"

replicate(d, X=X, Y=Y, D=D, Z=Z)


plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         smooth.lim=c(-1,0.4), bandwidth = 2)



