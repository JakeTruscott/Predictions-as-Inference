## This file replicates Figure 7 in the main text.

library(foreign)
library(interflex)

# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
graphpath<-paste0(path,"Graphs/main/")


d<-read.dta(paste0(path,"Data/Clark_Golder_CPS_2006/rep_clark_2006a.dta"),convert.factor=FALSE)
Y="enep1" 
D= "proximity1" 
X="enpres" 
Z<-c("uppertier" ,"logmag" , "uppertier_eneg" ,"eneg", "logmag_eneg")
Dlabel<-"presidential elections"
Xlabel<-"effective no. of pres. candidates"
Ylabel<-"effective no. of parties"
vartype<-"cluster"
cl<-"country"
time<-NULL
cuts<-c(0,0.1,3,4,7)


## Figure 7a
pdf(paste(graphpath,"fig7_a.pdf",sep=""))
inter.binning(Y=Y,D=D,X=X,Z=Z,Ylabel=Ylabel, Xlabel=Xlabel, Dlabel=Dlabel, 
	data=d, ylim = c(-6, 4), var = vartype, cutoffs = cuts, cl = cl)
graphics.off()

## Figure 7b
pdf(paste(graphpath,"fig7_b.pdf",sep=""))
inter.kernel(Y=Y,D=D,X=X,Z=Z, Ylabel=Ylabel, Xlabel=Xlabel, Dlabel=Dlabel, 
	data=d, bw = 1, ylim = c(-6, 4), cl = cl)
graphics.off()

