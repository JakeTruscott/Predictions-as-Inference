## This file replicates Figure 6(b and c) in the main text.

library(foreign)
library(interflex)

# ## set root directory of the replication file
path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
graphpath<-paste0(path,"Graphs/main/")

d<-read.dta(paste0(path,"Data/Malesky_APSR_2012/rep_malesky_2012.dta"),convert.factor=FALSE)
Y="d_question_count" 
D="t2" 
X="internet_users100" 
Z<-c("centralnominated","fulltime","retirement","city","ln_gdpcap","ln_pop","transfer","south","unweighted")
Dlabel<-"Treatment"
Xlabel<-"Internet Penetration"
Ylabel<-"Question Count (a)"
vartype<-"cluster"
cl<-"pci_id" 

## Figure 6b
pdf(paste(graphpath,"fig6_b.pdf",sep=""))
inter.binning(Y=Y,D=D,X=X,Z=Z,Ylabel=Ylabel, Xlabel=Xlabel, Dlabel=Dlabel, 
	data=d, ylim = c(-2, 1), var = vartype, cl = cl)
graphics.off()

## Figure 6b: dropping for observations
pdf(paste(graphpath,"fig6_c.pdf",sep=""))
inter.binning(Y=Y,D=D,X=X,Z=Z,Ylabel=Ylabel, Xlabel=Xlabel, Dlabel=Dlabel, 
	data=d[-c(14,19,20,34),], ylim = c(-2, 1), var = vartype, cl = cl)
graphics.off()

## Figure 6d: bw from CV
set.seed(1234)
pdf(paste(graphpath,"fig6_d.pdf",sep=""))
inter.kernel(Y=Y,D=D,X=X,Z=Z, Ylabel=Ylabel, Xlabel=Xlabel, Dlabel=Dlabel, 
	data=d, bw = 1.62, ylim = c(-2, 1), cl = cl, nboots = 1000)
graphics.off()

