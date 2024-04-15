### How Much Should We Trust Estimates from Multiplicative Interaction Models?
### Jens Hainmueller, Jonathan Mummolo, Yiqing Xu


library(foreign)
library(interflex)
# ## set root directory of the replication file
## path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
## set graph folder
graphpath<-paste(path,"Graphs/",sep="")
source(paste(path,"Code/hmx.R",sep=""))


###########################
### First panel in Figure 4
###########################
##xtreg roa npc npcXso_portion so_portion fyear_ind* [aweight=weights_w4], fe vce(robust)



d<-read.dta(paste0(path,"Data/Truex_APSR_2014/rep_truex_2014a.dta"),convert.factor=FALSE)
name<-"truex_2014a"
head(d)
dim(d)
Y="roa" 
D="npc" 
X="so_portion" 
Z<-NULL
FE<-c("gvkey","fyear")
Dlabel<-"NPC Membership"
Xlabel<-"State-owned Portion"
Ylabel<-"Return on Assets"
vartype<-"cluster"
cl<-"gvkey"
weights<-"weights_w4"
main="Truex (2014) \n APSR"
table(d[,D])
cuts <- NULL

##replicate(d, X=X, D=D, Y=Y, Z=Z, FE = FE, weight=weights)

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         weights=weights, vartype=vartype, cl=cl, FE=FE, nboots = 200, bandwidth = 0.821,
         time=NULL, pairwise=TRUE, wald = TRUE, smooth.lim=c(-0.05,0.05))

###########################
## Second panel in Figure 4
###########################

##xtreg roa npc npcXrev2007 rev2007 fyear_ind* [aweight=weights_w4], fe vce(robust)
d<-read.dta(paste0(path,"Data/Truex_APSR_2014/rep_truex_2014a.dta"),convert.factor=FALSE)
name<-"truex_2014b"
Y="roa" 
D="npc" 
X="rev2007" 
Z<-NULL
FE<-c("gvkey","fyear")
Dlabel<-"NPC Membership"
Xlabel<-"Revenue (2007)"
Ylabel<-"Return on Assets"
vartype<-"cluster"
cl<-"gvkey"
weights<-"weights_w4"
main="Truex (2014) \n APSR"
table(d[,D]) 

##replicate(d, X=X, D=D, Y=Y, Z=Z, weight=weights)

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         weights=weights, vartype=vartype, cl=cl,FE=FE, bandwidth = 81095,
         time=NULL, pairwise=TRUE, interval=c(0,50000),wald=TRUE,
         smooth.lim=c(-0.11,0.03))

###########################
## Third panel in Figure 4
###########################

##xtreg margin npc npcXso_portion so_portion fyear_ind* [aweight=weights_w4], fe vce(robust)
d<-read.dta(paste0(path,"Data/Truex_APSR_2014/rep_truex_2014b.dta"),convert.factor=FALSE)
name<-"truex_2014c"
Y="margin" 
D="npc" 
X="so_portion"
Z<-NULL
FE<-c("gvkey","fyear")
Dlabel<-"NPC Membership"
Xlabel<-"State-owned Portion"
Ylabel<-"Return on Assets"
vartype<-"cluster"
cl<-"gvkey"
weights<-"weights_w4"
table(d[,D]) 

##replicate(d, X=X, D=D, Y=Y, Z=Z, weight=weights)


plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         weights=weights, vartype=vartype, cl=cl, FE=FE,
         time=NULL, pairwise=TRUE, wald = TRUE, bandwidth = 0.239,
         smooth.lim=c(-0.10,0.10))

###########################
## Fourth panel in Figure 4
###########################

##xtreg margin npc npcXrev2007 rev2007 fyear_ind* [aweight=weights_w4], fe vce(robust)
d<-read.dta(paste0(path,"Data/Truex_APSR_2014/rep_truex_2014b.dta"),convert.factor=FALSE)
name<-"truex_2014d"
Y="margin" 
D="npc" 
X="rev2007"
Z<-NULL
FE<-c("gvkey","fyear")
Dlabel<-"NPC Membership"
Xlabel<-"Revenue (2007)"
Ylabel<-"Return on Assets"
vartype<-"cluster"
cl<-"gvkey"
weights<-"weights_w4"
table(d[,D]) 
quantile(d[,X],seq(0,1,0.02))
main="Truex (2014) \n APSR"
cuts <- NULL

##replicate(d, X=X, D=D, Y=Y, Z=Z, weight=weights)

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X,  Z=Z,  Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         weights=weights, vartype=vartype, cl=cl, FE=FE, bandwidth = 35589.216,
         time=NULL, pairwise=TRUE, interval=c(0,50000), wald = TRUE,
         smooth.lim=c(-0.10,0.10))

