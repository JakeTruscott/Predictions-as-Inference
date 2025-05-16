### How Much Should We Trust Estimates from Multiplicative Interaction Models?
### Jens Hainmueller, Jonathan Mummolo, Yiqing Xu


library(foreign)
library(interflex)
# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
## set graph folder
graphpath<-paste(path,"Graphs/",sep="")
source(paste(path,"Code/hmx.R",sep=""))

#################################
### First Interaction
#################################


##Table 2, Model 6 - 1946-2000 established democracies - first panel in Figure 1
##Figure 1, third panel

##stata code:
##regress enep1  eneg logmag uppertier enpres proximity1 logmag_eneg uppertier_eneg proximity1_enpres if old==1, robust cluster(country)


## prepare the data
d<-read.dta(paste0(path,"Data/Clark_Golder_CPS_2006/rep_clark_2006a.dta"),convert.factor=FALSE)
name<-"clark_2006a"
head(d)
dim(d)
Y="enep1" 
D="eneg" 
X="logmag" 
Z<-c("uppertier" ,"enpres" ,"proximity1", "uppertier_eneg" ,"proximity1_enpres" )
Dlabel<-"ethnic heterogeneity"
Xlabel<-"log avg. district magnitude"
Ylabel<-"effective no. of parties"
vartype<-"cluster"
cl<-"country"
time<-NULL
table(d[,D]) 
table(d[,X])
quantile(d[,X],seq(0,1,0.02)) 
cuts<-c(0,0.1,2,7)
cuts2<-NULL
main<-"Clark & Golder (2006) \n CPS"


replicate(d,X=X, Y=Y, D=D, Z=Z)

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=1, smooth.lim = c(-1,3))


#################################
### Second Interaction
#################################


##Figure 1, second panel using cox data

##stata code:
##regress enpv lnml upper enpres  proximit proxpres eneth lmleneth uppereneth if drop==0



## prepare the data
d<-read.dta(paste0(path,"Data/Clark_Golder_CPS_2006/rep_clark_2006b.dta"),convert.factor=FALSE)
name<-"clark_2006b"
head(d)
dim(d)
Y="enpv" 
D="eneth" 
X="lnml" 
Z<-c("upper" ,"enpres" ,"proximit", "proxpres" ,"uppereneth" )
Dlabel<-"ethnic heterogeneity"
Xlabel<-"log average district magnitude"
Ylabel<-"effective no. of parties"
vartype<-NULL
cl<-NULL
time<-NULL
table(d[,D]) 
table(d[,X])
quantile(d[,X],seq(0,1,0.02)) 
cuts<-c(0,0.1,2.3,7)
cuts2<-NULL
main<-"Clark & Golder (2006) \n CPS"


replicate(d, X=X, Y=Y, D=D, Z=Z)

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=1,smooth.lim=c(-2,4))

##Table 2, Model 4
##Figure 1, third panel

##stata code:
## regress enep1  eneg logmag uppertier enpres proximity1 logmag_eneg uppertier_eneg proximity1_enpres if nineties==1 & old==1


#################################
### Third Interaction
#################################


## prepare the data
d<-read.dta(paste0(path,"Data/Clark_Golder_CPS_2006/rep_clark_2006c.dta"),convert.factor=FALSE)
name<-"clark_2006c"
head(d)
dim(d)
Y="enep1" 
D="eneg" 
X="logmag" 
Z<-c("uppertier" ,"enpres" ,"proximity1", "uppertier_eneg" ,"proximity1_enpres" )
Dlabel<-"ethnic heterogeneity"
Xlabel<-"log average district magnitude"
Ylabel<-"effective no. of parties"
vartype<-"cluster"
cl<-"country"
time<-NULL
table(d[,D]) 
table(d[,X])
quantile(d[,X],seq(0,1,0.02)) 
cuts<-c(0,0.1,2,7)
cuts2<-NULL
main<-"Clark & Golder (2006) \n CPS"



replicate(X=X, Y=Y, D=D, Z=Z, data = d)

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         smooth.cl=1, bandwidth=NULL,smooth.lim=c(-2,5.2))


#################################
### Fourth Interaction
#################################


##Table 2, Model 6 - 1946-2000 established democracies
##Figure 2

##stata code:
##regress enep1  eneg logmag uppertier enpres proximity1 logmag_eneg uppertier_eneg proximity1_enpres if old==1, robust cluster(country)


## prepare the data
d<-read.dta(paste0(path,"Data/Clark_Golder_CPS_2006/rep_clark_2006a.dta"),convert.factor=FALSE)
name<-"clark_2006d"
head(d)
dim(d)
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
table(d[,D]) 
quantile(d[,X],seq(0,1,0.02)) 
cuts<-c(0,0.1,2.5,7)
cuts2<-c(0,1,7)
main<-"Clark & Golder (2006) \n CPS"

##the treatment does not vary when X=0
mean(d[,X]==0)##59 percent of data have X=0
table(d[d[,X]==0,D])##no variation on treatment at this level of X
var(d[d[,X]==0,D], na.rm=T)##no variance

table(d[d[,Y]<cuts[2],D])
var(d[d[,Y]<cuts[2],D])
summary(   lm(d[,Y]~d[,D], d[d[,X]<cuts[2],])  )


replicate(d, X=X, Y=Y, D=D, Z=Z)



plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel= Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time,
         pairwise=TRUE,span=1, bandwidth=1, smooth.lim=c(-6,4))




