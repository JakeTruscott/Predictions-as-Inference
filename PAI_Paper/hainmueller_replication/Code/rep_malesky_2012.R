### How Much Should We Trust Estimates from Multiplicative Interaction Models?
### Jens Hainmueller, Jonathan Mummolo, Yiqing Xu


library(foreign)
library(interflex)
# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
## set graph folder
graphpath<-paste(path,"Graphs/",sep="")
source(paste(path,"Code/hmx.R",sep=""))


d<-read.dta(paste0(path,"Data/Malesky_APSR_2012/rep_malesky_2012.dta"),convert.factor=FALSE)

########################
### First Interaction
##Table 5 Model 4
name<-"malesky_2012a"
head(d)
dim(d)
Y="d_question_count" 
D="t2" 
X="internet_users100" 
Z<-c("centralnominated","fulltime","retirement","city","ln_gdpcap","ln_pop","transfer","south","unweighted")
Dlabel<-"Treatment"
Xlabel<-"Internet Penetration"
Ylabel<-"Question Count (a)"
vartype<-"cluster"
cl<-"pci_id" 
table(d[,D]) 
table(d[,X]) 
cuts<-cuts2<-time<-NULL
main<-"Malesky et al. (2012) \n APSR"
quantile(d[,X],seq(0,1,1/3))
quantile(d[,X],seq(0,1,1/6))

replicate(data=d,X=X, D=D, Y=Y, Z=Z) #replicates


plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=1,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=1.62,smooth.lim=c(-2,1))


########################
### Second Interaction
####Table 5 Model 8
##reg  d.question_count i.t2##c.internet_users100 centralnom fulltime retirement  city ln_gdpcap ln_pop transfer south unweighted if session==6 & politburo==0, robust cluster(pci_id)


name<-"malesky_2012b"
head(d)
dim(d)
Y="d_criticize_total_per" 
D="t2" 
X="internet_users100" 
Z<-c("centralnominated","fulltime","retirement","city","ln_gdpcap","ln_pop","transfer","south","unweighted")
Dlabel<-"Treatment"
Xlabel<-"Internet Penetration"
Ylabel<-"Critical Questions % (a)"
vartype<-"cluster"
cl<-"pci_id"
table(d[,D]) 
table(d[,X]) 
cuts<-cuts2<-time<-NULL
main<-"Malesky et al. (2012) \n APSR"

replicate(data=d,X=X, D=D, Y=Y, Z=Z)#replicates


plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=5.571, smooth.lim=c(-17.5,10.2))




########################
### Third Interaction
####Table 5 Model 9

name<-"malesky_2012c"
head(d)
dim(d)
Y="diff_quest" 
D="t2" 
X="internet_users100" 
Z<-c("centralnominated","fulltime","retirement","city","ln_gdpcap","ln_pop","transfer","south","unweighted")
Dlabel<-"Treatment"
Xlabel<-"Internet penetration"
Ylabel<-"Question Count (b)"
vartype<-"cluster"
cl<-"pci_id"
table(d[,D]) 
table(d[,X]) 
cuts<-cuts2<-time<-NULL
main<-"Malesky et al. (2012) \n APSR"


replicate(data=d,X=X, D=D, Y=Y, Z=Z)#replicates

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=4.535,smooth.lim=c(-1,0.7))


########################
### Fourth Interaction
#####Table 5 Model 10

name<-"malesky_2012d"
head(d)
dim(d)
Y="diff_crit" 
D="t2" 
X="internet_users100" 
Z<-c("centralnominated","fulltime","retirement","city","ln_gdpcap","ln_pop","transfer","south","unweighted")
Dlabel<-"Treatment"
Xlabel<-"Internet Penetration"
Ylabel<-"Critical Questions % (b)"
vartype<-"cluster"
cl<-"pci_id"
table(d[,D]) 
table(d[,X]) 
cuts<-cuts2<-time<-NULL
main<-"Malesky et al. (2012) \n APSR"


replicate(data=d, X=X, D=D, Y=Y, Z=Z)#replicates


plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=2.445,smooth.lim=c(-10,7))


