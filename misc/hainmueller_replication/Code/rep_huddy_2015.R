### How Much Should We Trust Estimates from Multiplicative Interaction Models?
### Jens Hainmueller, Jonathan Mummolo, Yiqing Xu


library(foreign)
library(interflex)
# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
## set graph folder
graphpath<-paste(path,"Graphs/",sep="")
source(paste(path,"Code/hmx.R",sep=""))


## Table 5 Models 2 and 4, there are two interactions, both get plotted (total of 4 plots)
## Model 2
## reg totangry c.pidstr2##i.threat c.pidentity##i.threat c.issuestr2##i.threat c.knowledge c.educ i.male c.age10 if miss3==0, robust
## A1  pidentity*threat (Table 5, column 2, Figure 2 A)


d<-read.dta(paste0(path,"Data/Huddy_APSR_2015/rep_huddy_2015a.dta"),convert.factor=FALSE)
dim(d)
name<-"huddy_2015a"
names(d)
dim(d)
d$pidstr2_threat<-d$pidstr2 * d$threat
d$issuestr2_threat<-d$issuestr2 * d$threat
Y="totangry" 
D="threat" 
X="pidentity"
Z<-c("issuestr2", "pidstr2", "pidstr2_threat" ,"issuestr2_threat", "knowledge" , "educ" , "male" , "age10" )
Dlabel<-"Threat"
Xlabel<-"Partisan Identity"
Ylabel<-"Anger"
vartype<-"robust"
table(d[,D], exclude=NULL) 
table(d[,X], exclude=NULL) 
main<-"Huddy et al. (2015) \n APSR"
cl<-cuts<-cuts2<-time<-NULL

replicate(d,X=X, Y=Y, D=D, Z=Z)

Z<-c("issuestr2", "pidstr2", "knowledge" , "educ" , "male" , "age10" )
plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=0.917,smooth.lim=c(-0.18,0.5))

pdf(paste(graphpath,name,"_raw.pdf",sep=""),height=7,width=5)
inter.raw(Y=Y,D=D,X=X,Ylabel=Ylabel, Xlabel=Xlabel, Dlabel=Dlabel,
              data=d, span=1, pos=c(-0.2,1.2))
graphics.off()


####B1 pidentity*i.support (Table 5 column 4 Figure 2B)
##reg totpos c.pidstr2##i.support c.pidentity##i.support c.issuestr2##i.support c.knowledge c.educ i.male c.age10 if miss3==0, robust

d<-read.dta(paste0(path,"Data/Huddy_APSR_2015/rep_huddy_2015b.dta"),convert.factor=FALSE)
dim(d)
head(d)
name<-"huddy_2015b"
names(d)
dim(d)
Y="totpos" 
D="support" 
X="pidentity"
d$issuestr2Xsupport=d$issuestr2*d$support
d$pidstr2Xsupport=d$pidstr2*d$support
Z<-c("pidstr2", "issuestr2","issuestr2Xsupport", "pidstr2Xsupport", "knowledge" , "educ","male" , "age10")
Dlabel<-"Support"
Xlabel<-"Partisan Identity"
Ylabel<-"Enthusiasm"
vartype<-"robust"
table(d[,D], exclude=NULL) 
table(d[,X], exclude=NULL) 
quantile(d[,X],probs=seq(0,1,by=1/3))
main<-"Huddy et al. (2015) \n APSR"
cuts<-cuts2<-time<-NULL

replicate(d,X=X, Y=Y, D=D, Z=Z)


Z<-c("pidstr2", "issuestr2","knowledge" , "educ","male" , "age10")
plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=0.746, smooth.lim=c(-0.1,0.55))

pdf(paste(graphpath,name,"_raw.pdf",sep=""),height=7,width=5)
inter.raw(Y=Y,D=D,X=X,Ylabel=Ylabel, Xlabel=Xlabel, Dlabel=Dlabel,
              data=d, span=1, pos=c(-0.2,1.2))
graphics.off()

