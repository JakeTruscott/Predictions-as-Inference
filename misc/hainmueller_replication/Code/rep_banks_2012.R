### How Much Should We Trust Estimates from Multiplicative Interaction Models?
### Jens Hainmueller, Jonathan Mummolo, Yiqing Xu

library(foreign)
library(interflex)

# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"

## set graph folder
graphpath<-paste(path,"Graphs/",sep="")
source(paste(path,"Code/hmx.R",sep=""))


## First interaction

##Stata: reg racpolicy racresent1 anger angerres1 fearres1 fear disgust disgusres1 ideology education income1 age south openissuejo1 if baddata2==0
##data are already trimmed to estimation sample
##the claimed interaction is between symbolic racism and anger ... anger is the treatment, SR is the conditioning variable
##treatment is dichotomous

d<-read.dta(paste0(path,"Data/Banks_Valentino_AJPS_2012/rep_banks_2012a.dta"), convert.factors=F)
name<-"banks_2012a"
head(d)
dim(d)
Y="racpolicy" 
D="anger" 
X="racresent1" 
Z<-c("fearres1","fear" ,"disgust", "disgusres1", "ideology" ,"education" ,"income1", "age", "south", "openissuejo1" )
Dlabel<-"Anger"
Xlabel<-"Symbolic Racism"
Ylabel<-"Policy Opinion"
vartype<-NULL
table(d[,D], exclude=NULL) 
table(d[,X], exclude=NULL) 
quantile(d[,X],probs=seq(0,1,1/3))
cl<-NULL; FE <- NULL;
time<-NULL
cuts<-NULL
main<-"Banks & Valentino (2012) \n AJPS"


## Table 2 Column 1
replicate(data=d, Y=Y,D=D,X=X,Z=Z)

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=1,
         Y=Y, D=D, X=X, Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=1,smooth.lim=c(-0.6,0.4))

pdf(paste(graphpath,name,"_raw.pdf",sep=""),height=7,width=5)
inter.raw(Y=Y,D=D,X=X,Ylabel=Ylabel, Xlabel=Xlabel, Dlabel=Dlabel,
              data=d, span=1)
graphics.off()



## second interaction

## Table 2 Column 2
##anger x Old Fashioned Racism (jim crow)
##same data as second one

##Stata: 
##reg racpolicy jimcrow13 disgust disgusjc13 angerjc13 anger fear fearjc13 ideology education income1 age south openissuejo1 if baddata2==0
##data are already trimmed to estimation sample
##the claimed interaction is between symbolic racism and anger ... anger is the treatment, SR is the conditioning variable
##treatment is dichotomous

d<-read.dta(paste0(path,"Data/Banks_Valentino_AJPS_2012/rep_banks_2012b.dta"), convert.factors=F)
name<-"banks_2012b"
head(d)
dim(d)
Y="racpolicy" 
D="anger" 
X="jimcrow13" 
Z<-c("disgusjc13" ,"disgust" ,"fear" ,"fearjc13" ,"ideology", "education" ,"income1" ,"age" ,"south", "openissuejo1")
Dlabel<-"Anger"
Xlabel<-"Old-fashioned Racism"
Ylabel<-"Policy Opinion"
vartype<-NULL
cl<-NULL
time<-NULL
table(d[,D], exclude=NULL) 
table(d[,X], exclude=NULL) 
#hist(d[,X])
quantile(d[,X],seq(0,1,0.05))
cuts<-c(0,0.09,0.25,1)
cuts2<-c(0,0.09,1)

replicate(data=d, Y=Y,D=D,X=X,Z=Z)

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=1,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel,
         Xlabel=Xlabel, cuts=cuts, cuts2=cuts2, vartype=vartype, span = 1,
         bandwidth=0.102, cl=cl, time=time, pairwise=TRUE, smooth.lim=c(-0.4,0.9))


## third interaction

##disgust x Old Fashioned Racism (jim crow)
##Stata: 
## reg racpolicy jimcrow13 disgust disgusjc13 angerjc13 anger fear fearjc13 ideology education income1 age south openissuejo1 if baddata2==0
##data are already trimmed to estimation sample
##the claimed interaction is between symbolic racism and anger ... anger is the treatment, SR is the conditioning variable

##treatment is dichotomous
d<-read.dta(paste0(path,"Data/Banks_Valentino_AJPS_2012/rep_banks_2012b.dta"), convert.factors=F)
name<-"banks_2012c"
head(d)
dim(d)
Y="racpolicy" 
D="disgust" 
X="jimcrow13" 
Z<-c("angerjc13" ,"anger" ,"fear" ,"fearjc13" ,"ideology", "education" ,"income1" ,"age" ,"south", "openissuejo1")
Dlabel<-"Disgust"
Xlabel<-"Old-fashioned Racism"
Ylabel<-"Policy Opinion"
vartype<-NULL
cl<-NULL
time<-NULL
table(d[,D], exclude=NULL) 
table(d[,X], exclude=NULL) 

##are cuts unique?
quantile(d[,X],seq(0,1,0.05))
cuts<-c(0,0.09,0.25,1)
cuts2<-c(0,0.09,1)
table(d[,D],d[,X])


## Table 2 Column 2
replicate(data=d, Y=Y,D=D,X=X,Z=Z)

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=1,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel,
         Xlabel=Xlabel, cuts=cuts, cuts2=cuts2, vartype=vartype,
         bandwidth=0.102, cl=cl, time=time, pairwise=TRUE, smooth.lim=c(-0.4,0.7))











