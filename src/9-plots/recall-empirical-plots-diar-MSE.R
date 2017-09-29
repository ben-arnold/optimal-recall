

#--------------------------------------
# recall-empirical-plots-diar-MSE.R
# Ben Arnold
# 20 Oct 2011
#
# Make plots from different scenarios
#--------------------------------------

library(foreign)
library(RColorBrewer)
rm(list=ls())


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
# Global parameters for the MSE plots
#--------------------------------------

#cols <- brewer.pal(7,"Set1")[c(1:5,7)]
cols <- c(brewer.pal(12,"Paired")[seq(2,10,by=2)],brewer.pal(9,"Greys")[8])
clabels <- c("India HP","India MP","Indonesia","Peru","Senegal")
ltys <- c(1,2,4,5,6)
#ytics <- seq(0.4,2.4,by=0.2)
ytics <- c(0.1,0.5,1,2,10)
refday <- 2

#--------------------------------------
# Summary Plots of Relative MSE RD
# Daily prevalence
#--------------------------------------

pdf("~/dropbox/articles/wsp-recall/figures/diar/empirical-diar-MSE-RD.pdf",width=15,height=5)
op <- par(lwd=1.5,bty="l",cex.lab=1,mar=c(5,6,4,2)+0.1)
lo <- layout(mat=matrix(1:3,nrow=1,ncol=3))

# RD MSE, ANEMIA
plot(1:14,1:14,type="n",
	main= "Hb < 110 g/l",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),ylog=TRUE,log="y",
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,ylog=TRUE,labels=sprintf("%1.1f",ytics),las=1)
	mtext("Relative\nMSE",side=3,line=0,at=-1,cex=0.75)
	mtext("Day 2 = Reference",side=2,line=3,cex=0.75)
	segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	segments(0,1,14,1,lwd=1)
	lines(1:14,rMSE(aihp$bRDdp,aihp$RDdpV,refday),lty=ltys[1])
	lines(1:14,rMSE(aimp$bRDdp,aimp$RDdpV,refday),lty=ltys[2])
	lines(1:14,rMSE(aind$bRDdp,aind$RDdpV,refday),lty=ltys[3])
	lines(1:14,rMSE(aper$bRDdp,aper$RDdpV,refday),lty=ltys[4])
	lines(1:14,rMSE(asen$bRDdp,asen$RDdpV,refday),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,bty="n")

# RD MSE, HAZ
plot(1:14,1:14,type="n",
	main= "HAZ < -2",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),ylog=TRUE,log="y",
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,ylog=TRUE,labels=sprintf("%1.1f",ytics),las=1)
	mtext("Relative\nMSE",side=3,line=0,at=-1,cex=0.75)
	mtext("Day 2 = Reference",side=2,line=3,cex=0.75)
	segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	segments(0,1,14,1,lwd=1)
	lines(1:14,rMSE(hihp$bRDdp,hihp$RDdpV,refday),lty=ltys[1])
	lines(1:14,rMSE(himp$bRDdp,himp$RDdpV,refday),lty=ltys[2])
	lines(1:14,rMSE(hind$bRDdp,hind$RDdpV,refday),lty=ltys[3])
	lines(1:14,rMSE(hper$bRDdp,hper$RDdpV,refday),lty=ltys[4])
	lines(1:14,rMSE(hsen$bRDdp,hsen$RDdpV,refday),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,bty="n")
	
# RD MSE, WAZ
plot(1:14,1:14,type="n",
	main= "WAZ < -2",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),ylog=TRUE,log="y",
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,ylog=TRUE,labels=sprintf("%1.1f",ytics),las=1)
	mtext("Relative\nMSE",side=3,line=0,at=-1,cex=0.75)
	mtext("Day 2 = Reference",side=2,line=3,cex=0.75)
	segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	segments(0,1,14,1,lwd=1)
	lines(1:14,rMSE(wihp$bRDdp,wihp$RDdpV,refday),lty=ltys[1])
	lines(1:14,rMSE(wimp$bRDdp,wimp$RDdpV,refday),lty=ltys[2])
	lines(1:14,rMSE(wind$bRDdp,wind$RDdpV,refday),lty=ltys[3])
	lines(1:14,rMSE(wper$bRDdp,wper$RDdpV,refday),lty=ltys[4])
	lines(1:14,rMSE(wsen$bRDdp,wsen$RDdpV,refday),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,bty="n")

par(op)
dev.off()


#--------------------------------------
# Summary Plots of Relative MSE RR
# Daily prevalence
#--------------------------------------


pdf("~/dropbox/articles/wsp-recall/figures/diar/empirical-diar-MSE-RRdp.pdf",width=15,height=5)
op <- par(lwd=1.5,bty="l",cex.lab=1,mar=c(5,6,4,2)+0.1)
lo <- layout(mat=matrix(1:3,nrow=1,ncol=3))

# RR MSE, ANEMIA
plot(1:14,1:14,type="n",
	main= "Hb < 110 g/l",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),ylog=TRUE,log="y",
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,ylog=TRUE,labels=sprintf("%1.1f",ytics),las=1)
	mtext("Relative\nMSE",side=3,line=0,at=-1,cex=0.75)
	mtext("Day 2 = Reference",side=2,line=3,cex=0.75)
	segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	segments(0,1,14,1,lwd=1)
	lines(1:14,rMSE(aihp$bRRdp,aihp$RRdpV,refday),lty=ltys[1])
	lines(1:14,rMSE(aimp$bRRdp,aimp$RRdpV,refday),lty=ltys[2])
	lines(1:14,rMSE(aind$bRRdp,aind$RRdpV,refday),lty=ltys[3])
	lines(1:14,rMSE(aper$bRRdp,aper$RRdpV,refday),lty=ltys[4])
	lines(1:14,rMSE(asen$bRRdp,asen$RRdpV,refday),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,bty="n")

# RR MSE, HAZ
plot(1:14,1:14,type="n",
	main= "HAZ < -2",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),ylog=TRUE,log="y",
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,ylog=TRUE,labels=sprintf("%1.1f",ytics),las=1)
	mtext("Relative\nMSE",side=3,line=0,at=-1,cex=0.75)
	mtext("Day 2 = Reference",side=2,line=3,cex=0.75)
	segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	segments(0,1,14,1,lwd=1)
	lines(1:14,rMSE(hihp$bRRdp,hihp$RRdpV,refday),lty=ltys[1])
	lines(1:14,rMSE(himp$bRRdp,himp$RRdpV,refday),lty=ltys[2])
	lines(1:14,rMSE(hind$bRRdp,hind$RRdpV,refday),lty=ltys[3])
	lines(1:14,rMSE(hper$bRRdp,hper$RRdpV,refday),lty=ltys[4])
	lines(1:14,rMSE(hsen$bRRdp,hsen$RRdpV,refday),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,bty="n")
	
# RR MSE, WAZ
plot(1:14,1:14,type="n",
	main= "WAZ < -2",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),ylog=TRUE,log="y",
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,ylog=TRUE,labels=sprintf("%1.1f",ytics),las=1)
	mtext("Relative\nMSE",side=3,line=0,at=-1,cex=0.75)
	mtext("Day 2 = Reference",side=2,line=3,cex=0.75)
	segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	segments(0,1,14,1,lwd=1)
	lines(1:14,rMSE(wihp$bRRdp,wihp$RRdpV,refday),lty=ltys[1])
	lines(1:14,rMSE(wimp$bRRdp,wimp$RRdpV,refday),lty=ltys[2])
	lines(1:14,rMSE(wind$bRRdp,wind$RRdpV,refday),lty=ltys[3])
	lines(1:14,rMSE(wper$bRRdp,wper$RRdpV,refday),lty=ltys[4])
	lines(1:14,rMSE(wsen$bRRdp,wsen$RRdpV,refday),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,bty="n")

par(op)
dev.off()

#--------------------------------------
# Summary Plots of Relative MSE RR
# Period prevalence
#--------------------------------------

pdf("~/dropbox/articles/wsp-recall/figures/diar/empirical-diar-MSE-RRpp.pdf",width=16,height=4)
op <- par(lwd=1.5,bty="l",cex.lab=1,mar=c(5,6,4,2)+0.1,xpd=TRUE)
lo <- layout(mat=matrix(1:4,nrow=1,ncol=4),widths=c(0.6,1,1,1))


plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Diarrhea","Relative MSE\n(Day 2 = Reference)"),bty="n",cex=1.5)



# RR MSE, ANEMIA
plot(1:14,1:14,type="n",
	main= "Hb < 110 g/l",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),ylog=TRUE,log="y",
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,ylog=TRUE,labels=sprintf("%1.1f",ytics),las=1)
	# mtext("Relative\nMSE",side=3,line=0,at=-1,cex=0.75)
	# mtext("Day 2 = Reference",side=2,line=3,cex=0.75)
	segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	segments(0,1,14,1,lwd=1)
	lines(1:14,rMSE(aihp$bRRpp,aihp$RRppV,refday),lty=ltys[1])
	lines(1:14,rMSE(aimp$bRRpp,aimp$RRppV,refday),lty=ltys[2])
	lines(1:14,rMSE(aind$bRRpp,aind$RRppV,refday),lty=ltys[3])
	lines(1:14,rMSE(aper$bRRpp,aper$RRppV,refday),lty=ltys[4])
	lines(1:14,rMSE(asen$bRRpp,asen$RRppV,refday),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,bty="n")

# RR MSE, HAZ
plot(1:14,1:14,type="n",
	main= "HAZ < -2",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),ylog=TRUE,log="y",
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,ylog=TRUE,labels=sprintf("%1.1f",ytics),las=1)
	# mtext("Relative\nMSE",side=3,line=0,at=-1,cex=0.75)
	# mtext("Day 2 = Reference",side=2,line=3,cex=0.75)
	segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	segments(0,1,14,1,lwd=1)
	lines(1:14,rMSE(hihp$bRRpp,hihp$RRppV,refday),lty=ltys[1])
	lines(1:14,rMSE(himp$bRRpp,himp$RRppV,refday),lty=ltys[2])
	lines(1:14,rMSE(hind$bRRpp,hind$RRppV,refday),lty=ltys[3])
	lines(1:14,rMSE(hper$bRRpp,hper$RRppV,refday),lty=ltys[4])
	lines(1:14,rMSE(hsen$bRRpp,hsen$RRppV,refday),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,bty="n")
	
# RR MSE, WAZ
plot(1:14,1:14,type="n",
	main= "WAZ < -2",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),ylog=TRUE,log="y",
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,ylog=TRUE,labels=sprintf("%1.1f",ytics),las=1)
	# mtext("Relative\nMSE",side=3,line=0,at=-1,cex=0.75)
	# mtext("Day 2 = Reference",side=2,line=3,cex=0.75)
	segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	segments(0,1,14,1,lwd=1)
	lines(1:14,rMSE(wihp$bRRpp,wihp$RRppV,refday),lty=ltys[1])
	lines(1:14,rMSE(wimp$bRRpp,wimp$RRppV,refday),lty=ltys[2])
	lines(1:14,rMSE(wind$bRRpp,wind$RRppV,refday),lty=ltys[3])
	lines(1:14,rMSE(wper$bRRpp,wper$RRppV,refday),lty=ltys[4])
	lines(1:14,rMSE(wsen$bRRpp,wsen$RRppV,refday),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,bty="n")

par(op)
dev.off()


#--------------------------------------
# Summary Plots of Relative MSE RR
# Period prevalence,
# STRATIFIED BY APPROX DIFF VS NON-DIFF
# REPORTING
#--------------------------------------

# diarrhea only

# differential:
#

lo <- layout(mat=matrix(1:2,ncol=2,nrow=1))

plot(1:14,1:14,type="n",
	main= "Differential Reporting",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),ylog=TRUE,log="y",
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,ylog=TRUE,labels=sprintf("%1.1f",ytics),las=1)
	mtext("Relative\nMSE",side=3,line=0,at=-1,cex=0.75)
	mtext("Day 2 = Reference",side=2,line=3,cex=0.75)
	segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	segments(0,1,14,1,lwd=1)
	
	lines(1:14,rMSE(hihp$bRRpp,hihp$RRppV,refday),lty=ltys[1])
	lines(1:14,rMSE(wihp$bRRpp,wihp$RRppV,refday),lty=ltys[1])
	
	lines(1:14,rMSE(aimp$bRRpp,aimp$RRppV,refday),lty=ltys[1]) # 
	
	lines(1:14,rMSE(aind$bRRpp,aind$RRppV,refday),lty=ltys[1])
	lines(1:14,rMSE(hind$bRRpp,hind$RRppV,refday),lty=ltys[1])
	
	lines(1:14,rMSE(wper$bRRpp,wper$RRppV,refday),lty=ltys[1])
	
	lines(1:14,rMSE(asen$bRRpp,asen$RRppV,refday),lty=ltys[1])
	
	lines(1:14,rMSE(hsen$bRRpp,hsen$RRppV,refday),lty=ltys[1]) #
	lines(1:14,rMSE(wsen$bRRpp,wsen$RRppV,refday),lty=ltys[1]) #

plot(1:14,1:14,type="n",
	main= "Non-Differential Reporting",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),ylog=TRUE,log="y",
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,ylog=TRUE,labels=sprintf("%1.1f",ytics),las=1)
	mtext("Relative\nMSE",side=3,line=0,at=-1,cex=0.75)
	mtext("Day 2 = Reference",side=2,line=3,cex=0.75)
	segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	segments(0,1,14,1,lwd=1)
	
	lines(1:14,rMSE(aihp$bRRpp,aihp$RRppV,refday),lty=ltys[1])

	lines(1:14,rMSE(himp$bRRpp,himp$RRppV,refday),lty=ltys[1])
	lines(1:14,rMSE(wimp$bRRpp,wimp$RRppV,refday),lty=ltys[1])
	
	lines(1:14,rMSE(wind$bRRpp,wind$RRppV,refday),lty=ltys[1])
	
	lines(1:14,rMSE(aper$bRRpp,aper$RRppV,refday),lty=ltys[1])
	lines(1:14,rMSE(hper$bRRpp,hper$RRppV,refday),lty=ltys[1]) #
	
	




