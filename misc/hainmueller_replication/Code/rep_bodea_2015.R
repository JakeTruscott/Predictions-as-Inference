### How Much Should We Trust Estimates from Multiplicative Interaction Models?
### Jens Hainmueller, Jonathan Mummolo, Yiqing Xu

library(foreign)
library(interflex)

# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"

## set graph folder
graphpath<-paste(path,"Graphs/",sep="")
source(paste(path,"Code/hmx.R",sep=""))

### First Interaction
##Figure 1a, Table 2 model 9

## prepare the data
name<-"bodea_2015a"
d<-read.dta(paste0(path,"Data/Bodea_JOP_2015/rep_bodea_2015a.dta",sep=""),convert.factor=FALSE)
d$polity2_cen<-d$polity2_cen+10 ##rescale the moderator
head(d)
dim(d)
Y="fdiinflow_res"  # use demeaned outcome  
D="llvaw" 
X="polity2_cen" 
Z<-c("l_dffrus","l_dfxreserves","l_openness","l_dgdp_k",
     "l_lngdppc","l_fiscal_balance","l_capital_controls","l_lninfl",
     "l_signyearfill","xrdum","year")   # remember to add in a set of dummies (since its FE model)
FE<-"cowcode"
Dlabel<-"Lag CBI"
Xlabel<-"Polity"
Ylabel<-"FDI"
vartype<-"cluster"
cl<-"cowcode"
# hist(d[,Y])
# dev.off()
table(d[,D]) 
table(d[,X]) 
main<-"Bodea & Hicks (2015a) \n JOP"
cuts<-cuts2<-time<-NULL


replicate(d, X=X, Y=Y, D=D, Z=Z, FE=FE)

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X, Z=Z, FE=FE,
         Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=NULL, pairwise=TRUE,
         bandwidth=19, smooth.lim=c(-32,10))


### Second Interaction
##Figure 1c, Table 2 model 15

## prepare the data
name<-"bodea_2015b"
d<-read.dta(paste0(path,"Data/Bodea_JOP_2015/rep_bodea_2015b.dta",sep=""),convert.factor=FALSE)
d$polity2_cen<-d$polity2_cen+10 ##rescale the moderator
head(d)
dim(d)
Y="real_10yrate_res"  # use demeaned outcome  
D="llvaw" 
X="polity2_cen" 
Z<-c("l_dffrus","l_dfxreserves","l_openness","l_wdgdpdefl","l_dgdp_k","l_lngdppc",
     "l_fiscal_balance","l_capital_controls","l_lngdp","l_lninfl","xrdum","year")   
FE<-"cowcode"
Dlabel<-"Lag CBI"
Xlabel<-"Polity"
Ylabel<-"10-year Bond Rates"
vartype<-"cluster"
cl<-"cowcode"
##hist(d[,Y])
table(d[,D]) 
table(d[,X]) 
main<-"Bodea & Hicks (2015a) \n JOP"
cuts<-cuts2<-time<-NULL

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=1, 
         Y=Y, D=D, X=X, Z=Z, FE=FE, Ylabel=Ylabel, Dlabel=Dlabel,
         Xlabel=Xlabel, cuts=cuts, cuts2=cuts2, vartype=vartype,
         cl=cl, time=time, pairwise=TRUE, bandwidth=5.715,
         smooth.lim=c(-40,30))

