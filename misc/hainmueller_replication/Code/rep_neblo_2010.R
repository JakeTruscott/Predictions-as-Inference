### How Much Should We Trust Estimates from Multiplicative Interaction Models?
### Jens Hainmueller, Jonathan Mummolo, Yiqing Xu


library(foreign)
library(interflex)
# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
## set graph folder
graphpath<-paste(path,"Graphs/",sep="")
source(paste(path,"Code/hmx.R",sep=""))


d<-read.dta(paste0(path,"Data/Neblo_et_al_APSR_2010/rep_neblo_2010.dta"), convert.factors=F)
name<-"neblo_2010"
head(d)
dim(d)
Y="willing" 
D="treatcong2" 
X="stealth2_ct" 
Z<-c( "needjud_ct", "educ_ct", "income_ct", "interest_ct" ,"chur_ct",
     "gentrust_ct", "intxcon", "treathour2", "treatint" ,"treattop" ,
     "treatinc" , "gender2" ,"age_ct",  "conflict_ct" ,"needcog_ct" ,
     "sunshine_ct", "efficacy_ct" ,"pidcoll_ct" ,"empfull", "white" )
Dlabel<-"Congress Treatment"
Xlabel<-c("Stealth Democ.")
Ylabel<-"Willingness to Deliberate"
table(d[,D], exclude=NULL) ##dummy
table(d[,X], exclude=NULL) ##continuous
hist(as.numeric(d[,X])) 
dev.off()
cuts<-cuts2<-time<-NULL
vartype<-NULL
cl<-NULL
main<-"Neblo et al. (2010) \n APSR"


replicate(data=d, X=X, Y=Y, D=D, Z=Z)


plot.all(data=d, graphpath=graphpath, name=name, main=main,smooth=TRUE,
         Y=Y, D=D, X=X,  Z=Z, Ylabel=Ylabel, Dlabel=Dlabel, Xlabel=Xlabel,
         cuts=cuts, cuts2=cuts2, vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         bandwidth=1.756, smooth.lim=c(-0.5,0.7))

