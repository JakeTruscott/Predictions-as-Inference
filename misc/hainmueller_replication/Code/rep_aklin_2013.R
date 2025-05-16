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
d<-read.dta(paste0(path,"Data/Aklin_AJPS_2013/rep_aklin_2013.dta"))
dim(d)

##############################

## First Case

name<-"aklin_2013a"
Y="drenew_capacity_nh_share" 
D="oilcrude_price2007dollar_bp" 
X="lrenewpc" 
Z<-c("left_to_right","right_to_left","left_executive",
     "right_executive","election_year",
     "renewablecapacity_3yr_average","hydronuclear_3yr","year",
     "traditional_electricity_share")
Ylabel<-"Renewable Energy Share"
Dlabel<-"Oil Prices"
Xlabel<-"Lag Positive Reinforcement"
#hist(d[,D]) 
#hist(d[,X]) 
vartype<-"pcse"
cl<-"country_aklin"; FE = NULL
time<-"year"
main<-"Aklin & Urpelainen (2013)\n AJPS"
cuts<-NULL

## same as above: Table 1 Column 3

## xtpcse drenew_capacity_nh_share left_to_right right_to_left linnovation_x_oil oilcrude_price2007dollar_bp lrenewpc left_executive right_executive election_year renewablecapacity_3yr_average hydronuclear_3yr year traditional_electricity_share

replicate(d, Y=Y,X=X,D=D,Z=Z)

set.seed(1234)
plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=1,
         Y=Y, D=D, X=X, Z=Z, Ylabel=Ylabel, Dlabel=Dlabel,
         Xlabel=Xlabel, cuts=cuts, vartype=vartype, cl=cl, time=time,
         pairwise=TRUE, bandwidth=1.101, interval=c(0,3), 
         smooth.lim=c(-0.15,0.1))



#########################################

## Second Case

name<-"aklin_2013b"
Y="drenew_capacity_nh_share"
X="oilcrude_price2007dollar_bp" 
D="lrenewpc" 
Z<-c("left_to_right","right_to_left","left_executive","right_executive","election_year","renewablecapacity_3yr_average","hydronuclear_3yr","year","traditional_electricity_share")
Ylabel<-"renewable energy share"
Xlabel<-"oil prices"
Dlabel<-"lag positive reinforcement"
vartype<-"pcse"
cl<-"country_aklin"
time<-"year"
main<-"Aklin & Urpelainen (2013)\n AJPS"
cuts<-NULL
#hist(d[,D]) 
#hist(d[,X])


## Table 1 Column 3

## xtpcse drenew_capacity_nh_share left_to_right right_to_left linnovation_x_oil oilcrude_price2007dollar_bp lrenewpc left_executive right_executive election_year renewablecapacity_3yr_average hydronuclear_3yr year traditional_electricity_share

## bandwidth = 23.3
set.seed(1234)
plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=1,
         Y=Y, D=D, X=X, Z=Z,
         Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=23.3, smooth.lim=c(-0.5,1))


