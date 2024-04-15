### How Much Should We Trust Estimates from Multiplicative Interaction Models?
### Jens Hainmueller, Jonathan Mummolo, Yiqing Xu


library(foreign)
library(interflex)
# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
## set graph folder
graphpath<-paste(path,"Graphs/",sep="")
source(paste(path,"Code/hmx.R",sep=""))


load(paste0(path,"Data/Vernby_AJPS_2013/AJPSReplicationdata.RData"))
ls()
d<-x
head(d)
dim(d)

##construct differences DVs
school1<-d[d$term==1,c("term","code","schooling","socialvard")]
school2<-d[d$term==2,c("term","code","schooling","socialvard")]
school1<-school1[order(school1$code),]
school2<-school2[order(school2$code),]
school.diff<-school2$schooling - school1$schooling
socialvard.diff<-school2$socialvard - school1$socialvard
diff.dvs<-cbind.data.frame(school.diff= school.diff, socialvard.diff= socialvard.diff, code=school1$code  )
first<-d[d$term==1, c("code","Taxbase2","pop","manu")]
first<-first[order(first$code),]
head(first)
second<-d[d$term==2, c("code","Taxbase2","pop","manu")]
second<-second[order(second$code),]
names(second)<-c("code","Taxbase2.2","pop.2","manu.2")

d2<-cbind.data.frame(diff.dvs, first, second)
head(d2)
##generate differenced interaction
noncitvotsh <-d$noncitvotsh[d$term==2]
noncit5 <-d$noncit5[d$term==2]
noncit5<-noncit5[order(d$code[d$term==2])]
noncit15 <-d$noncit15[d$term==2]
noncit15<-noncit15[order(d$code[d$term==2])]
noncitvotsh <-d$noncitvotsh[d$term==2]
noncitvotsh <-noncitvotsh[order(d$code[d$term==2])]
int<-d$noncitvotsh*d$noncit15
int<-int[order(d$code)]
int<-int[seq(2, length(int),2)]
d2$int<-int
int2<-d$noncitvotsh*d$noncit5
int2<-int2[order(d$code)]
int2<-int2[seq(2, length(int2),2)]
d2$int2<-int2
d2$noncit5<-noncit5
d2$noncit15<-noncit15
d2$noncitvotsh<-noncitvotsh


#######################
## First interaction
#######################

##Table 2, model 1
##successfully replicated, including standard errors
m1<-lm( school.diff~ noncitvotsh +noncit15 + int  +  Taxbase2 +  Taxbase2.2 + pop +pop.2 +   manu + manu.2, data=d2)
# install.packages("lmtest")
# install.packages("sandwich")
library(lmtest); library(sandwich)
coeftest(m1, vcovHC(m1, type="HC1"))


d<-d2
head(d)
dim(d)
Y="school.diff" 
D="noncitvotsh" 
X="noncit15" 
Z<-c("Taxbase2" ,  "Taxbase2.2" , "pop" , "pop.2" ,   "manu" , "manu.2")
Dlabel<-"Share Non-Citizens"
Xlabel<-"Prop. School-Aged Non-Citizens"
Ylabel<-"Change in Ed. Services"
vartype<-"robust"
cl<-NULL
table(d[,D], exclude=NULL) 
table(d[,X], exclude=NULL) 
quantile(d[,X],probs=seq(0,1,0.02))
d<-na.omit(d[,c(Y,X,D,Z)])
name<-"vernby_2013a"
main<-"Vernby (2013) \n AJPS"
cuts<-cuts2<-NULL
time<-NULL

##build formula for model
replicate(data=d, X=X, D=D, Y=Y, Z=Z)##this replicates

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X, Z=Z,Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=0.257,smooth.lim=c(-15,40))


#######################
## Second interaction
#######################


##Table 2, model 2
m2<-lm( socialvard.diff ~ noncitvotsh +noncit5 + int2  +  Taxbase2 +  Taxbase2.2 + pop +pop.2 +   manu + manu.2, data=d2)
coeftest(m2, vcovHC(m2, type="HC1"))##this replicates
dim(m2$model)



#########apply diagnostic functions (two continuous interactions)
d<-d2
head(d)
dim(d)
Y="socialvard.diff" 
D="noncitvotsh" 
X="noncit5" 

Z<-c("Taxbase2" ,  "Taxbase2.2" , "pop" , "pop.2" ,   "manu" , "manu.2")
Dlabel<-"Share Non-Citizens"
Xlabel<-"Prop. School-Aged Non-Citizens"
Ylabel<-"Change in Social Services"
vartype<-"robust"
cl<-NULL
table(d[,D], exclude=NULL) 
table(d[,X], exclude=NULL)
quantile(d[,X],probs=seq(0,1,1/3))
d<-na.omit(d[,c(Y,X,D,Z)])
name<-"vernby_2013b"
cuts<-cuts2<-NULL
time<-NULL
main<-"Vernby (2013) \n AJPS"
##build formula for model


replicate(data=d, X=X, D=D, Y=Y, Z=Z)

plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=T,
         Y=Y, D=D, X=X, Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=0.045, smooth.lim=c(-15,22))











