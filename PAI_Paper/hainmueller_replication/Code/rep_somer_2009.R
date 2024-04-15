### How Much Should We Trust Estimates from Multiplicative Interaction Models?
### Jens Hainmueller, Jonathan Mummolo, Yiqing Xu


library(foreign)
library(interflex)
# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
## set graph folder
graphpath<-paste(path,"Graphs/",sep="")
source(paste(path,"Code/hmx.R",sep=""))

##reg absch1 votech2 monthstoprevelect votechtime absch2, cluster (edate)

d<-read.dta(paste0(path,"Data/Somer_Topcu_JOP_2009/rep_somer_2009.dta"), convert.factors=F)
name<-"somer_2009"
head(d)
dim(d)
Y="absch1" 
D="votech2" 
X="monthstoprevelect" 
Z<-c("absch2")
Dlabel<-"Lagged Party Policy"
Xlabel<-c("Months since Election")
Ylabel<-"Change in Party Policy"
vartype<-"cluster"
cl<-"edate"
table(d[,D], exclude=NULL) 
table(d[,X], exclude=NULL) 
hist(as.numeric(d[,X])) 
dev.off()
##are cuts unique?
cuts<-quantile(d[,X],probs=seq(0,1,1/3))
cuts2<-NULL
time<-NULL
main<-"Somer-Topcu (2009) \n JOP"


replicate(data=d, X=X, D=D, Y=Y, Z=Z)


plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE, bandwidth = 22.146)


