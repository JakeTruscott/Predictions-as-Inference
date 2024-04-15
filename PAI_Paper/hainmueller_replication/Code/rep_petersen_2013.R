### How Much Should We Trust Estimates from Multiplicative Interaction Models?
### Jens Hainmueller, Jonathan Mummolo, Yiqing Xu


library(foreign)
library(interflex)
# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
## set graph folder
graphpath<-paste(path,"Graphs/",sep="")
source(paste(path,"Code/hmx.R",sep=""))



###Figure 1
d <- read.dta(paste0(path,"Data/Petersen_APSR_2014/rep_petersen_2013a.dta"))
name<-"petersen_2013a"
Y="tougher" 
D="lazy" 
X="imagine" 
Z<-NULL
Dlabel<-"Attitude"
Xlabel<-"Imagination"
Ylabel<-"Tougher Means Testing"
vartype<-NULL
table(d[,D]) 
table(d[,X]) 
cuts<-cuts2<-time<-NULL
main<-"Petersen and Arroe (2013) \n APSR"
weights<-NULL

summary(lm(tougher~lazy+imagine+lazy*imagine, data=d))

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=1,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype,
         cl=NULL, time=time, pairwise=TRUE,
         bandwidth=0.098,smooth.lim=c(-2,2),span=1, interval=c(.63,.88))




##### FIGURE 2
##Table A12 in Appendix, Model 1, Basis for Figure 2 Panel A

d <- read.dta(paste0(path,"Data/Petersen_APSR_2014/rep_petersen_2013b.dta"))
d$im<-d$imagine01
name<-"petersen_2013b"
head(d)
dim(d)
Y="dictator" 
D="attitude" 
X="im" 
Z<-NULL
Dlabel<-"Attitude"
Xlabel<-"Imagination"
Ylabel<-"Dictator"
vartype<-NULL
table(d[,D]) 
table(d[,X]) 
cuts<-cuts2<-time<-NULL
main<-"Petersen and Arroe (2013) \n APSR"
weights<-NULL

summary(lm(dictator~attitude+im+attitude*im, data=d))##this replicates


plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=1,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype,
         cl=NULL, time=time, pairwise=TRUE,
         bandwidth=0.75,smooth.lim=c(-3,1.5),span=1,interval=c(.67,.92))         


