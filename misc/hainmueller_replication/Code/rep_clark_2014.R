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
d<-read.dta(paste0(path,"Data/Clark_Leiter_CPS_2014/rep_clark_2014.dta"),convert.factor=FALSE)
name<-"clark_2014"
head(d)
dim(d)
Y="votechgtt1"  
D="cgscn2tt1" 
X="partydispuw" 
Z<-c("votechgt1t2","ingovt_mc")
FE<-"partnum"
 # since it's fixed-effect
Dlabel<-"change in valence"
Xlabel<-"change in party dispersion"
Ylabel<-"change in vote share"
vartype<-"cluster"
cl<-"partnum"
table(d[,D]) 
table(d[,X]) 
##hist(d[,X])
cuts<-cuts2<-time<-NULL
main<-"Clark & Leiter (2014) \n CPS"
quantile(d[,X],seq(0,1,0.02))

##xtreg votechgtt1 votechgt1t2 cgscn2tt1 ingovt_mc partydispuw valbydispuw , fe robust

replicate(d, X=X, Y=Y, D=D, Z=Z, FE=FE)

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=1,
         Y=Y, D=D, X=X, FE=FE, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=0.353, smooth.lim=c(-0.25,0.6),span=1)

