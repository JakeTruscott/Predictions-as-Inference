## This file replicates Figure 4 in the main text.

library(foreign)
library(interflex)

# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
graphpath<-paste0(path,"Graphs/main/")

d<-read.dta(paste0(path,"Data/Huddy_APSR_2015/rep_huddy_2015a.dta"),convert.factor=FALSE)
dim(d)
name<-"huddy_2015a"
names(d)
dim(d)
d$pidstr2_threat<-d$pidstr2 * d$threat
d$issuestr2_threat<-d$issuestr2 * d$threat
Y="totangry" 
D="threat" 
X="pidentity"
Dlabel<-"Threat"
Xlabel<-"Partisan Identity"
Ylabel<-"Anger"
vartype<-"robust"
Z<-c("issuestr2", "pidstr2", "knowledge" , "educ" , "male" , "age10" )

## Figure 4a
pdf(paste(graphpath,"fig4_a.pdf",sep=""),height=7,width=5)
inter.raw(Y=Y,D=D,X=X,Ylabel=Ylabel, Xlabel=Xlabel, Dlabel=Dlabel,
              data=d, span=1, pos=c(-0.2,1.2))
graphics.off()

## Figure 4b
pdf(paste(graphpath,"fig4_b.pdf",sep=""))
inter.binning(Y=Y,D=D,X=X,Z=Z,Ylabel=Ylabel, Xlabel=Xlabel, Dlabel=Dlabel, 
	data=d, ylim = c(-0.18, 0.5), var = vartype)
graphics.off()

## Figure 4c
pdf(paste(graphpath,"fig4_c.pdf",sep=""))
inter.kernel(Y=Y,D=D,X=X,Z=Z, Ylabel=Ylabel, Xlabel=Xlabel, Dlabel=Dlabel, 
	data=d, bw = 0.917, ylim = c(-0.18, 0.5))
graphics.off()
