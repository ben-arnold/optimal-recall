

#--------------------------------------
# recall-empirical-plots-diar.R
# Ben Arnold
# 
# plot bias^2, var, MSE by country,
# and risk factor
# 
#--------------------------------------

library(foreign)
library(RColorBrewer)
rm(list=ls())


#--------------------------------------
# Load base functions
#--------------------------------------
source("~/dropbox/articles/wsp-recall/programs/empirical/empirical-base-functions-v2.R")



#--------------------------------------
# Load base functions
#--------------------------------------
source("~/dropbox/articles/wsp-recall/programs/final/2-base-functions/empirical-base-functions-v2.R")



#--------------------------------------
# Load empirical output
#--------------------------------------

# Anemia
load("~/dropbox/articles/wsp-recall/programs/final/5-diar-analysis/recall-empirical-diar-anemia.RData")
aihp <- ihp
aimp <- imp
aind <- ind
aper <- per
asen <- sen

# HAZ < -2
load("~/dropbox/articles/wsp-recall/programs/final/5-diar-analysis/recall-empirical-diar-haz.RData")
hihp <- ihp
himp <- imp
hind <- ind
hper <- per
hsen <- sen

# WAZ < -2
load("~/dropbox/articles/wsp-recall/programs/final/5-diar-analysis/recall-empirical-diar-waz.RData")
wihp <- ihp
wimp <- imp
wind <- ind
wper <- per
wsen <- sen



#--------------------------------------
# Combined bias, var, MSE plot
#--------------------------------------

MSEplot <- function(B,V,main,xlab,ytics) {
	# plot bias^2, variance, MSE
	# B = bias; V = variance
	
	clabels <- c("MSE","Variance","Bias squared")
	ltys <- c(1,2,4)
	
	plot(1:14,1:14,type="n",
	main= main,
	bty="l",
	xlab=xlab,xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,labels=sprintf("%1.2f",ytics),las=1)
	# mtext("% Bias",side=3,line=0,at=-0.5,cex=0.75)
	segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	# segments(0,1,14,1,lwd=1)
	segments(0,ytics,14,ytics,col="gray80",lwd=1)
	
	lines(1:14,V+B^2,lty=ltys[1])
	lines(1:14,V,lty=ltys[2])
	lines(1:14,B^2,lty=ltys[3])
	
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")
	
}

MSEplot2 <- function(B,V,main,xlab,ytics,letlab,title) {
	# specific to the main text figure (no gridlines, legend title)
	# plot bias^2, variance, MSE
	# B = bias; V = variance
	
	clabels <- c("MSE","Variance","Bias Squared")
	ltys <- c(1,2,4)
	
	plot(1:14,1:14,type="n",
	main= main,
	bty="l",
	xlab=xlab,xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,labels=sprintf("%1.2f",ytics),las=1)
	mtext(letlab,side=3,line=1,at=1,cex=1)
	#segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	# segments(0,1,14,1,lwd=1)
	#segments(0,ytics,14,ytics,col="gray80",lwd=1)
	
	lines(1:14,V+B^2,lty=ltys[1])
	lines(1:14,V,lty=ltys[2])
	lines(1:14,B^2,lty=ltys[3])
	
	legend("topright",legend=clabels,lty=ltys)
	
}



#--------------------------------------
# Subset plot for the article main text
#--------------------------------------
pdf("~/dropbox/articles/wsp-recall/figures/diar/empirical-diar-bias-var-mse-PRpp-3countries.pdf",width=4,height=12)
op <- par(lwd=1.5,bty="l",cex.lab=1.25,lwd=1.5 )
lo <-layout(mat=matrix(1:3,ncol=1,nrow=3))
MSEplot2(wihp$bRRpp,wihp$RRppV,main="",letlab="",xlab="Recall Period, days",ytics= seq(0,0.5,by=0.05))
MSEplot2(wimp$bRRpp,wimp$RRppV,main="",letlab="",xlab="Recall Period, days",ytics= seq(0,0.08,by=0.01))
MSEplot2(wind$bRRpp,wind$RRppV,main="",letlab="",xlab="Recall Period, days",ytics= seq(0,0.2,by=0.02))
par(op)
dev.off()

#--------------------------------------
# PR, period prevalence
#--------------------------------------

pdf("~/dropbox/articles/wsp-recall/figures/diar/empirical-diar-bias-var-mse-PRpp.pdf",width=17,height=20)
op <- par(lwd=1.5,bty="l",cex.lab=1,mar=c(5,6,4,2)+0.1)
lo <- layout(mat=matrix(1:20,nrow=5,ncol=4),widths=rep(c(0.4,1,1,1),5))


ihpy <- seq(0,0.5,by=0.05)
impy <- seq(0,0.24,by=0.02)
indy <- seq(0,0.24,by=0.02)
pery <- seq(0,0.14,by=0.02)
seny <- seq(0,0.3,by=0.02)

labcex <- 1.5
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("India HP"),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("India MP"),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Indonesia"),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Peru"),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Senegal"),bty="n",cex=labcex)

# Anemia
MSEplot(aihp$bRRpp,aihp$RRppV,main="Anemic\nHb < 110 g/l",xlab="",ytics=ihpy)
MSEplot(aimp$bRRpp,aimp$RRppV,main="",xlab="",ytics=impy)
MSEplot(aind$bRRpp,aind$RRppV,main="",xlab="",ytics=indy)
MSEplot(aper$bRRpp,aper$RRppV,main="",xlab="",ytics=pery)
MSEplot(asen$bRRpp,asen$RRppV,main="",xlab="Recall period (days)",ytics=seny)

# HAZ
MSEplot(hihp$bRRpp,hihp$RRppV,main="Stunted\nHAZ < -2",xlab="",ytics=ihpy)
MSEplot(himp$bRRpp,himp$RRppV,main="",xlab="",ytics=impy)
MSEplot(hind$bRRpp,hind$RRppV,main="",xlab="",ytics=indy)
MSEplot(hper$bRRpp,hper$RRppV,main="",xlab="",ytics=pery)
MSEplot(hsen$bRRpp,hsen$RRppV,main="",xlab="Recall period (days)",ytics=seny)

# WAZ
MSEplot(wihp$bRRpp,wihp$RRppV,main="Underweight\nWAZ < -2",xlab="",ytics=ihpy)
MSEplot(wimp$bRRpp,wimp$RRppV,main="",xlab="",ytics=impy)
MSEplot(wind$bRRpp,wind$RRppV,main="",xlab="",ytics=indy)
MSEplot(wper$bRRpp,wper$RRppV,main="",xlab="",ytics=pery)
MSEplot(wsen$bRRpp,wsen$RRppV,main="",xlab="Recall period (days)",ytics=seny)


par(op)
dev.off()





