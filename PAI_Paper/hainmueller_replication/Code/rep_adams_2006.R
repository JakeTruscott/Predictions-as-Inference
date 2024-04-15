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
name<-"adams_2006"
d<-na.omit(read.dta(paste0(path,"Data/Adams_et_al_AJPS_2006/rep_adams_2006.dta")))
head(d)
dim(d)
Y="pshift2" # change in ideological shift
D="idparty" # niche party
X="vshift" # public opinion shift
Z<-c("pshiftt12","votec1","pvoteshift","Italy","Britain","Greece","Luxembourg","Denmark","Netherlands","Spain")
Xlabel<-"Public Opinion Shift"
Ylabel<-"Policy Shift"
Dlabel<-"Niche Party"
main<-"Adams et al. (2006)\n AJPS"
vartype<-NULL; cl<-NULL; time<-NULL; FE = NULL;
table(d[,D]) 
table(d[,X])

## replicate original results
replicate(d,Y=Y,X=X,D=D,Z=Z)

## diagnostic plots
set.seed(1234)
plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=1,
         Y=Y, D=D, X=X, Z=Z,
         Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         vartype=vartype, cl=cl, time=time, pairwise=0,
         bandwidth=1.007, smooth.lim=c(-2,2.8))

