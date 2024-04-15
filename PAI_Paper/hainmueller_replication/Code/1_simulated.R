## This file replicates Figures 1-3 in the main text and Figure A3 in the Appendix.

rm(list=ls())


## set root directory of the replication file
path <- "~/Dropbox/ProjectZ/Interaction Paper/Replication/"
## set graph folder
graphpath<-paste(path,"Graphs/main/",sep="")

source(paste(path,"Code/hmx.R",sep=""))
library(ggplot2)
library(interflex)

######### simulating data ######

set.seed(1234)
n<-200
d1<-rnorm(n,3,1)
d2<-sample(c(0,1),n,replace=TRUE)
x<-rnorm(n,3,1)
e<-rnorm(n,0,1)
y1<-5-4*x-9*d1+3*x*d1+2*e
y2<-5-4*x-9*d2+3*x*d2+2*e
s1<-cbind.data.frame(D=d1,X=x,Y=y1)
s2<-cbind.data.frame(D=d2,X=x,Y=y2)
head(s1)
head(s2)

## quadratic margianl effect
x3 <- runif(n, -3,3)
y3 <- d2*(x3^2-2.5) + (1-d2)*(-1*x3^2+2.5) + 2*e
s3 <- cbind.data.frame(D=d2, X=x3, Y=y3)

Y <- "Y"
D <- "D"
X <- "X"
Z <- NULL

###############################
## Figure 1: Raw Plots
###############################

## Figure 1(a): binary, linear
pdf(paste(graphpath,"fig1_a.pdf",sep=""),height=7,width=5)
inter.raw(Y="Y",D="D",X="X",data=s2)
graphics.off()

## Figure 1(b): binary, nonlinear
pdf(paste(graphpath,"fig1_b.pdf",sep=""),height=7,width=5)
inter.raw(Y="Y",D="D",X="X",data=s3)
graphics.off()

## Figure 1(c): continuous, linear
pdf(paste(graphpath,"fig1_c.pdf",sep=""),height=4,width=10)
inter.raw(Y="Y",D="D",X="X",data=s1)
graphics.off()

inter.raw(Y="Y",D="D",X="X",data=s1, Xlabel = "XX")



###############################
## Figure 2: Binning Estimates
###############################

out<-inter.binning(data=s2, Y="Y",D="D",X="X", Z=NULL, nbins=3, Xdistr = "hist")
pdf(paste(graphpath,"fig2_a.pdf",sep=""))
print(out$graph)
graphics.off()

out <- inter.binning(data=s3, Y="Y",D="D",X="X", Z=NULL,
                     nbins=3, Xdistr = "hist", ylim = c(-10, 13))
pdf(paste(graphpath,"fig2_b.pdf",sep=""))
print(out$graph)
graphics.off()

###############################
## Figure 3: Kernel Estimates
###############################

## linear example: bandwidth = 6.592 (max)
out<-inter.kernel(Y="Y",D="D",X="X",Z=NULL,data=s2, nboot=1000, cl=NULL, 
  bw = 6.592, Xdistr = "hist")
pdf(paste(graphpath,"fig3_a.pdf",sep=""))
p1<-out$graph
p1<-p1+geom_abline(slope=3,intercept=-9,colour="red",linetype="dashed")
plot(p1)
graphics.off()

## nonlinear example: bandwidth = 0.505
set.seed(02139)
out<-inter.kernel(Y="Y",D="D",X="X",Z=NULL,data=s3,  nboot=1000,cl=NULL,
                  Xdistr = "hist", bw = 0.505, ylim = c(-10, 13))
p1<-out$graph
dat <- data.frame(x = x3, y = 2*x3^2-5)
p1<-p1+geom_line(data = dat,aes(x = x, y = y),
                 colour="red",linetype="dashed")
pdf(paste(graphpath,"fig3_b.pdf",sep=""))
print(p1)
graphics.off()


###############################
## Figure A1: GAM plot
###############################

pdf(paste(graphpath,"figA1.pdf",sep=""))
inter.gam(Y="Y",D="D",X="X",Z=NULL,data=s1,SE=0)
graphics.off()



