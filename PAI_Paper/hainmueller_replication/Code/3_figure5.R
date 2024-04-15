## This file replicates Figure 5 in the main text.

library(foreign)
library(interflex)

# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
graphpath<-paste0(path,"Graphs/main/")

## prepare the data
d<-read.dta(paste0(path,"Data/Chapman_IO_2009/rep_chapman_2009.dta"),convert.factor=FALSE)
Y="rally" 
D="unauth" 
X="S" 
Z<-c("priorpop","bipart","statemnt","nextelec","nytcov","buscon","revorg","wardumk","majopp","allies","war","SCappeal","regorgact","ordsever","hostlvl")
Dlabel<-"UN authorization"
Xlabel<-"US affinity with UN Security Council"
Ylabel<-"rallies"
vartype<-"robust"
table(d[,D]) 
table(d[,X]) 

## Figure 5a
pdf(paste(graphpath,"fig5_a.pdf",sep=""),height=7,width=5)
inter.raw(Y=Y,D=D,X=X,Ylabel=Ylabel, Xlabel=Xlabel, Dlabel=Dlabel,
              data=d, span=1, pos=c(65,80))
graphics.off()

## Figure 5b
pdf(paste(graphpath,"fig5_b.pdf",sep=""))
inter.binning(Y=Y,D=D,X=X,Z=Z,Ylabel=Ylabel, Xlabel=Xlabel, Dlabel=Dlabel, 
	data=d, ylim = c(-300, 200), var = vartype)
graphics.off()

## Figure 5c: bw is obtained from CV
pdf(paste(graphpath,"fig5_c.pdf",sep=""))
inter.kernel(Y=Y,D=D,X=X,Z=Z, Ylabel=Ylabel, Xlabel=Xlabel, Dlabel=Dlabel, 
	data=d, bw = 0.300, ylim = c(-300, 200))
graphics.off()

