### How Much Should We Trust Estimates from Multiplicative Interaction Models?
### Jens Hainmueller, Jonathan Mummolo, Yiqing Xu
library(foreign)
library(interflex)
# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
## set graph folder
graphpath<-paste(path,"Graphs/",sep="")
source(paste(path,"Code/hmx.R",sep=""))



###################################################
### First interaction
###################################################

##FIGURE 2
## column 4 
##felsdvreg infadjownexp1k partysd opres partysdXopres absmeandist partysdXabsmeandist opresXabsmeandist  infadjoppexp1k quality    frosh  if incumbent2==1 &  marginprev !=., ivar(stcd_redist) jvar(year) peff(cdfe) feff(yearfe) xb(mainef) res(regresid) mover(movervar)  mnum(mnumvar) pobs(pobsvar) group(coolgroupvar) cluster(stcd)

d<-read.dta(paste0(path,"Data/Kim_Leveck_2013/rep_kim_2013.dta"))

##Table 1, column 4
d2<-na.omit(d[d$incumbent2==1 &  !is.na(d$marginprev),
              c("infadjownexp1k" ,"partysd", "opres",
                "partysdXopres", "absmeandist" ,"partysdXabsmeandist",
                "opresXabsmeandist" , "infadjoppexp1k" ,"quality"   ,
                "frosh",  "year" , "stcd_redist" , "stcd",
                "marginprev","incumbent2","stcd")])

## summary(m2<-lm(infadjownexp1k ~partysd+ opres+ partysdXopres+ absmeandist +partysdXabsmeandist+ opresXabsmeandist + infadjoppexp1k +quality   + frosh +as.factor(year)+as.factor(stcd_redist), data=d2)) $coefficients[1:10,]
## ##this replicates column 4 in Table 1


d<-d2
name<-"kim_2013a"
names(d)
dim(d)
Y="infadjownexp1k" 
D="partysd" 
X="opres"
Z<-c("absmeandist","partysdXabsmeandist","opresXabsmeandist", "infadjoppexp1k", "quality","frosh")
FE<-c("year","stcd_redist")
Dlabel<-"Uncertainty in Party Rep."
Xlabel<-"District Partisanship"
Ylabel<-"Campaign Spending"
vartype<-"cluster"
cl<-"stcd"
quantile(d[,X],probs=seq(0,1,by=1/3))
cuts<-cuts2<-time<-NULL
main<-"Kim & LeVeck (2013) \n APSR"

##replicate
replicate(d, Y=Y, D=D, X=X, Z=Z, FE=FE)

Z<-c("absmeandist","opresXabsmeandist","infadjoppexp1k", "quality","frosh")
plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X, Z=Z, FE=FE,
         Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=18.71, nboots = 200, neval = 20, smooth.lim=c(-6000,5000))


###################################################
### Second interaction
###################################################

##FIGURE 3
##felsdvreg infadjownexp1k partysd opres absmeandist opresXabsmeandist infadjoppexp1k quality    frosh  if incumbent2==1 &  marginprev !=., ivar(stcd_redist) jvar(year) peff(cdfe) feff(yearfe) xb(mainef) res(regresid) mover(movervar)  mnum(mnumvar) pobs(pobsvar) group(coolgroupvar) cluster(stcd)

d<-d2
name<-"kim_2013b"
names(d)
dim(d)
Y="infadjownexp1k" 
D="absmeandist" 
X="opres"
Z<-c("partysd","partysdXopres","partysdXabsmeandist","infadjoppexp1k", "quality","frosh")
FE<-c("year","stcd_redist")
Dlabel<-"Incumbent Party Distance"
Xlabel<-"District Partisanship"
Ylabel<-"Campaign Spending"
vartype<-"cluster"
table(d[,D], exclude=NULL) 
cl<-"stcd"
quantile(d[,X],probs=seq(0,1,by=1/3))
cuts<-cuts2<-time<-NULL
main<-"Kim & LeVeck (2013) \n APSR"

##replicate
##replicate(d, Y=Y, D=D, X=X, Z=Z, FE=FE)

Z<-c("partysd","partysdXopres","infadjoppexp1k", "quality","frosh")
plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X, Z=Z, FE=FE, cores = 24,
         Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=79.07,nboots=200, smooth.lim=c(-800,500))


###################################################
### Third interaction
###################################################

#####FIGURE 4

##felsdvreg infadjownexp1k partysd opres partysdXopres absmeandist infadjoppexp1k quality    frosh  if incumbent2==1 &  marginprev !=., ivar(stcd_redist) jvar(year) peff(cdfe) feff(yearfe) xb(mainef) res(regresid) mover(movervar)  mnum(mnumvar) pobs(pobsvar) group(coolgroupvar) cluster(stcd)

d<-d2
name<-"kim_2013c"
names(d)
dim(d)
Y="infadjownexp1k" 
D="absmeandist" 
X="partysd"
Z<-c( "partysdXopres","opresXabsmeandist","infadjoppexp1k", "opres","quality","frosh")
FE<-c("year","stcd_redist")
Dlabel<-"Incumbent Party Distance"
Xlabel<-"Uncertainty in Party Reputation"
Ylabel<-"Campaign Spending"
vartype<-"cluster"
table(d[,D], exclude=NULL) 
##are cuts unique?
##no, set custom cut points
cl<-"stcd"
main<-"Kim & LeVeck (2013) \n APSR"
quantile(d[,X],probs=seq(0,1,by=1/3))


## replicate(d, Y=Y, D=D, X=X, Z=Z, FE=FE)

Z<-c("opres","partysdXopres","infadjoppexp1k", "quality","frosh")
plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=FALSE,
         Y=Y, D=D, X=X, Z=Z, FE=FE, cores = 24, 
         Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=0.079,nboots=200,smooth.lim=c(-800,400))




