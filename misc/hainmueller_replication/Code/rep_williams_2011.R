### How Much Should We Trust Estimates from Multiplicative Interaction Models?
### Jens Hainmueller, Jonathan Mummolo, Yiqing Xu


library(foreign)
library(interflex)
# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
## set graph folder
graphpath<-paste(path,"Graphs/",sep="")
source(paste(path,"Code/hmx.R",sep=""))


###NOTE THIS PAPER USES 90% CIs in pictures but our code uses 95%
##note there are two interactions in this model but I am focusing on the one that generated the picture (figure 3 in the ms)

##########################################
##first model, Figure 3 (table 1 model3)
##########################################

d<-read.dta(paste0(path,"Data/Williams_CPS_2011/rep_williams_2011a.dta"), convert.factors=F)
name<-"williams_2011a"
head(d)
dim(d)
Y="change" 
D="opp_conf_party_elecdate" 
X="eff_par" 
Z<-c("opp_conf_elecdate","majority", "gparties", "lag_pervote" ,"rgdppc_growth" ,"eoc")
Dlabel<-"No. ncm's"
Xlabel<-"Effective no. of Parties"
Ylabel<-"Vote Share"
vartype<-"cluster"
table(d[,D], exclude=NULL) ##continuous
table(d[,X], exclude=NULL) ##continuous
##hist(as.numeric(d[,X])) 
quantile(d[,X],probs=seq(0,1,1/3))
cl<-"ccode"
cuts<-cuts2<-NULL
time<-NULL
main<-"Williams (2011) \n CPS"


replicate(data=d, X=X, Y=Y, D=D, Z=Z)

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X, Z=Z,Ylabel=Ylabel, Dlabel=Dlabel,
         Xlabel=Xlabel, cuts=cuts, cuts2=cuts2, vartype=vartype,
         cl=cl, time=time, pairwise=TRUE, bandwidth=1.8,
         smooth.lim=c(-1.6,1.7),interval=c(2,7), span=1)

##########################################
###second model ##Figure 4 (table 1 model4)
##########################################

d<-read.dta(paste0(path,"Data/Williams_CPS_2011/rep_williams_2011b.dta"), convert.factors=F)
name<-"williams_2011b"
head(d)
dim(d)
Y="change" 
D="opp_conf_party_elecdate" 
X="abs_rile" 
Z<-c("majority" ,"gparties", "lag_pervote", "rgdppc_growth" ,"ncm_all_abs_rile","opp_conf_elecdate")
Dlabel<-"No. ncm's"
Xlabel<-"Ideological Extremism"
Ylabel<-"Vote Share"
vartype<-"cluster"
table(d[,D], exclude=NULL) ##continuous
table(d[,X], exclude=NULL) ##continuous
##hist(as.numeric(d[,X])) 
cuts<-cuts2<-NULL
time<-NULL
main<-"Williams (2011) \n CPS"
cl<-"ccode"


replicate(data=d, X=X, Y=Y, D=D, Z=Z)


plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X, Z=Z,Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         interval=c(0,50), span=1,smooth.lim=c(-2.5,2), bandwidth=78.85)


