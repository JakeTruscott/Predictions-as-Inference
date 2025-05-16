### How Much Should We Trust Estimates from Multiplicative Interaction Models?
### Jens Hainmueller, Jonathan Mummolo, Yiqing Xu


library(foreign)
library(interflex)

# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"

## set graph folder
graphpath<-paste(path,"Graphs/",sep="")
source(paste(path,"Code/hmx.R",sep=""))


## FIRST

##  Table 1 column 2; Figure 2 top 
## xtreg logdm2  l.logdm2 l.lngdp l.dgdp_k l.openness c.lvaw##c.polity2_cen xrdum l.fiscal_balance pres_only leg_only pres_leg i.region ib4.decade  if sample ,  fe cl(cowcode) 

name<-"bodea_io_2015a"
d<-read.dta(paste0(path,"Data/Bodea_IO_2015/rep_bodea_io_2015a.dta"),convert.factor=FALSE)
head(d)
dim(d)
d$polity2_cen<-d$polity2_cen+20 ##rescale moderator to match paper
data <- d
Y="logdm2"
D="lvaw" 
X="polity2_cen" 
Z<-c("L_logdm2","L_lngdp","L_dgdp_k","L_openness","xrdum",
     "L_fiscal_balance","pres_only","leg_only","pres_leg")  
FE<-c("decade","cowcode") ## fixed effects
Dlabel<-"CBI"
Xlabel<-"Polity IV"
Ylabel<-"M2 Change"
vartype<-"cluster"
cl<-"cowcode"
## hist(d[,Y])
## dev.off()
table(d[,D]) 
quantile(d[,X],seq(0,1,1/3))
quantile(d[,X],seq(0,1,01))
cuts<-c(-20,-3,-1,0)+20
cuts2<-c(-20,-1,0)+20
main="Bodea and Hicks (2015b)\n IO"


replicate(d, Y=Y,D=D,X=X,Z=Z, FE=FE)

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE, 
         Y=Y, D=D, X=X, Z=Z, FE=FE, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=NULL, pairwise=TRUE,
         bandwidth=19,smooth.lim=c(-3,3))


## SECOND

## *  Figure 2 bottom *
## xtreg logdm2  l.logdm2 l.lngdp l.dgdp_k l.openness c.lvaw##c.FH_trans  xrdum l.fiscal_balance  pres_only leg_only pres_leg i.region  ib4.decade if sample  ,  fe cl(cowcode)

name<-"bodea_io_2015b"
d<-read.dta(paste0(path,"Data/Bodea_IO_2015/rep_bodea_io_2015b.dta"),convert.factor=FALSE)
head(d)
dim(d)
Y="logdm2" 
D="lvaw" 
X="FH_trans" 
Z<-c("L_logdm2","L_lngdp","L_dgdp_k","L_openness","xrdum","L_fiscal_balance","pres_only","leg_only","pres_leg")
FE<-c("region","decade","cowcode") ## fixed effects
Dlabel<-"CBI"
Xlabel<-"FH Democracy Score"
Ylabel<-"M2 Change"
vartype<-"cluster"
cl<-"cowcode"
##hist(d[,X])
table(d[,D]) 
quantile(d[,X],seq(0,1,1/3))
quantile(d[,X],seq(0,1,0.1))
cuts<-NULL
cuts2<-NULL
main="Bodea and Hicks (2015b)\n IO"


replicate(d, Y=Y,D=D,X=X,Z=Z, FE=FE)

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X, Z=Z, FE=FE, 
         Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=NULL, pairwise=TRUE,
         bandwidth=3.235, smooth.lim=c(-2.6,2))

## THIRD

## Table 1 column 6; Figure 4 top
##xtreg lninfl l.lninfl l.logdm2 l.lngdp l.dgdp_k l.openness c.lvaw##c.polity2_cen xrdum l.fiscal_balance pres_only leg_only pres_leg i.region l.wdgdpdefl /*ib4.decade*/ if sample ,  fe cl(cowcode) 


name<-"bodea_io_2015c"
d<-read.dta(paste0(path,"Data/Bodea_IO_2015/rep_bodea_io_2015c.dta"),convert.factor=FALSE)
d$polity2_cen<-d$polity2_cen+20 ##rescale moderator to match paper
head(d)
dim(d)
Y="lninfl"  
D="lvaw" 
X="polity2_cen" 
Z<-c("L_lninfl","L_logdm2","L_lngdp","L_dgdp_k","L_openness","xrdum","L_fiscal_balance","pres_only","leg_only","pres_leg","L_wdgdpdefl")
FE<-c("region","cowcode") ## fixed effects
Dlabel<-"CBI"
Xlabel<-"Polity IV"
Ylabel<-"Inflation"
vartype<-"cluster"
cl<-"cowcode"
#hist(d[,Y])
table(d[,D]) 
quantile(d[,X],seq(0,1,1/3))
quantile(d[,X],seq(0,1,01))
cuts<-c(-20,-3,-1,0)+20
cuts2<-c(-20,-1,0)+20
main="Bodea and Hicks (2015b)\n IO"


replicate(d, Y=Y,D=D,X=X,Z=Z, FE=FE)

plot.all(data=d, graphpath=graphpath, name=name, main=main,
         smooth=TRUE, Y=Y, D=D, X=X,Z=Z, FE=FE, Ylabel=Ylabel,
         Dlabel=Dlabel, Xlabel=Xlabel, cuts=cuts, cuts2=cuts2,
         vartype=vartype, cl=cl, time=NULL, pairwise=TRUE,
         bandwidth=15.464,smooth.lim=c(-2,1.5))

## FOURTH

##  Figure 4 bottom 
##xtreg lninfl l.lninfl l.logdm2 l.lngdp l.dgdp_k l.openness c.lvaw##c.FH_trans xrdum l.fiscal_balance  pres_only leg_only pres_leg i.region l.wdgdpdefl if sample  ,  fe cl(cowcode) 

name<-"bodea_io_2015d"
d<-read.dta(paste0(path,"Data/Bodea_IO_2015/rep_bodea_io_2015d.dta"),convert.factor=FALSE)
head(d)
dim(d)
Y="lninfl"   
D="lvaw" 
X="FH_trans" 
Z<-c("L_lninfl","L_logdm2","L_lngdp","L_dgdp_k","L_openness","xrdum","L_fiscal_balance","pres_only","leg_only","pres_leg","L_wdgdpdefl")
FE<-c("region","cowcode") ## fixed effects
Dlabel<-"CBI"
Xlabel<-"FH Democracy Score"
Ylabel<-"Inflation"
vartype<-"cluster"
cl<-"cowcode"
table(d[,D]) 
quantile(d[,X],seq(0,1,1/3))
quantile(d[,X],seq(0,1,0.1))
cuts<-NULL
cuts2<-NULL
main="Bodea and Hicks (2015b)\n IO"


replicate(d, Y=Y,D=D,X=X,Z=Z, FE=FE)

plot.all(data=d, graphpath=graphpath, name=name, main=main,
         smooth=TRUE, Y=Y, D=D, X=X, Z=Z, FE=FE, Ylabel=Ylabel,
         Dlabel=Dlabel, Xlabel=Xlabel, cuts=cuts, cuts2=cuts2,
         vartype=vartype, cl=cl, time=NULL, pairwise=TRUE,
         bandwidth=2.143,smooth.lim=c(-2,1.20))

