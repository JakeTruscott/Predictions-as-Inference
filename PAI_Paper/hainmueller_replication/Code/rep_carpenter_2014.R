### How Much Should We Trust Estimates from Multiplicative Interaction Models?
### Jens Hainmueller, Jonathan Mummolo, Yiqing Xu


library(foreign)
library(interflex)
# ## set root directory of the replication file
# path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
## set graph folder
graphpath<-paste(path,"Graphs/",sep="")
source(paste(path,"Code/hmx.R",sep=""))


d<-read.dta(paste0(path,"Data/Carpenter_Moore_APSR_2014/rep_carpenter_2014.dta"))
dim(d)
head(d)
name<-"carpenter_2014"
names(d)
dim(d)
Y="totnamesper1000" 
D="pctpetwomen" 
X="pctfocgag"
Z<-c("pctfocdc", "pctpetsep", "pctfocterr", "pctfocnewst" ,"pctfocslavetrade")
FE<-c("countyid_number","congress")
Dlabel<-"women's only petition %"
Xlabel<-"focus gag rule %"
Ylabel<-"total names/1000"
vartype<-"cluster"
cl<-"countyid_number"
time<-NULL
table(d[,D], exclude=NULL) 
table(d[,X], exclude=NULL) 
main<-"Carpenter & Moore (2014) \n APSR"

##are cuts unique? 80% zero!
quantile(d[,X],probs=seq(0,1,by=0.025))
cuts<-c(0,10,27,85)
cuts2<-c(0,15,85)

replicate(d, X=X, Y=Y, D=D, Z=Z, FE=FE)

plot.all(data=d, graphpath=graphpath, name=name, main=main,
         smooth=TRUE, Y=Y, D=D, X=X, FE=FE, span=1, Ylabel=Ylabel,
         Dlabel=Dlabel, Xlabel=Xlabel, cuts=cuts, cuts2=cuts2,
         vartype=vartype, cl=cl, time=time, pairwise=TRUE,
         smooth.lim=c(-2,11),bandwidth = 36.571)

