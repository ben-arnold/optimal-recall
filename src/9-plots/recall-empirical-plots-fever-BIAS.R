

#--------------------------------------
# recall-empirical-plots-fever-BIAS.R
# Ben Arnold
# 4 Nov 2011
#
# Summarize the bias over different
# recall periods
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
load("~/dropbox/articles/wsp-recall/programs/final/6-fever-analysis/recall-empirical-fever-anemia.RData")
aihp <- ihp
aimp <- imp
aind <- ind
aper <- per
asen <- sen

# HAZ < -2
load("~/dropbox/articles/wsp-recall/programs/final/6-fever-analysis/recall-empirical-fever-haz.RData")
hihp <- ihp
himp <- imp
hind <- ind
hper <- per
hsen <- sen

# WAZ < -2
load("~/dropbox/articles/wsp-recall/programs/final/6-fever-analysis/recall-empirical-fever-waz.RData")
wihp <- ihp
wimp <- imp
wind <- ind
wper <- per
wsen <- sen


#--------------------------------------
# Global parameters for the bias plots
#--------------------------------------
#cols <- brewer.pal(7,"Set1")[c(1:5,7)]
cols <- c(brewer.pal(12,"Paired")[seq(2,10,by=2)],brewer.pal(9,"Greys")[8])
clabels <- c("India HP","India MP","Indonesia","Peru","Senegal")
ltys <- c(1,2,4,5,6)
ytics <- seq(0,50,by=10)


#--------------------------------------
# % Bias for RR period prevalence
#--------------------------------------

pdf("~/dropbox/articles/wsp-recall/figures/fever/empirical-fever-BIAS-RRpp.pdf",width=15,height=5)
op <- par(lwd=1.5,bty="l",cex.lab=1,mar=c(5,6,4,2)+0.1)
lo <- layout(mat=matrix(1:3,nrow=1,ncol=3))

# RR Bias, ANEMIA
plot(1:14,1:14,type="n",
	main= "Hb < 110 g/l",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,labels=sprintf("%1.0f",ytics),las=1)
	mtext("% Bias",side=3,line=0,at=-0.5,cex=0.75)
	# segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	# segments(0,1,14,1,lwd=1)
	segments(0,ytics,14,ytics,col="gray80",lwd=1)
	lines(1:14,pctbias(aihp$RRpp,aihp$bRRpp),lty=ltys[1])
	lines(1:14,pctbias(aimp$RRpp,aimp$bRRpp),lty=ltys[2])
	lines(1:14,pctbias(aind$RRpp,aind$bRRpp),lty=ltys[3])
	lines(1:14,pctbias(aper$RRpp,aper$bRRpp),lty=ltys[4])
	lines(1:14,pctbias(asen$RRpp,asen$bRRpp),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")

# RR Bias, HAZ
plot(1:14,1:14,type="n",
	main= "HAZ < -2",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,labels=sprintf("%1.0f",ytics),las=1)
	mtext("% Bias",side=3,line=0,at=-0.5,cex=0.75)
	# segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	# segments(0,1,14,1,lwd=1)
	segments(0,ytics,14,ytics,col="gray80",lwd=1)
	lines(1:14,pctbias(hihp$RRpp,hihp$bRRpp),lty=ltys[1])
	lines(1:14,pctbias(himp$RRpp,himp$bRRpp),lty=ltys[2])
	lines(1:14,pctbias(hind$RRpp,hind$bRRpp),lty=ltys[3])
	lines(1:14,pctbias(hper$RRpp,hper$bRRpp),lty=ltys[4])
	lines(1:14,pctbias(hsen$RRpp,hsen$bRRpp),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")


# RR Bias, WAZ
plot(1:14,1:14,type="n",
	main= "WAZ < -2",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,labels=sprintf("%1.0f",ytics),las=1)
	mtext("% Bias",side=3,line=0,at=-0.5,cex=0.75)
	# segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	# segments(0,1,14,1,lwd=1)
	segments(0,ytics,14,ytics,col="gray80",lwd=1)
	lines(1:14,pctbias(wihp$RRpp,wihp$bRRpp),lty=ltys[1])
	lines(1:14,pctbias(wimp$RRpp,wimp$bRRpp),lty=ltys[2])
	lines(1:14,pctbias(wind$RRpp,wind$bRRpp),lty=ltys[3])
	lines(1:14,pctbias(wper$RRpp,wper$bRRpp),lty=ltys[4])
	lines(1:14,pctbias(wsen$RRpp,wsen$bRRpp),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")


par(op)
dev.off()



#--------------------------------------
# % Bias for RR daily prevalence
#--------------------------------------

pdf("~/dropbox/articles/wsp-recall/figures/fever/empirical-fever-BIAS-RRdp.pdf",width=15,height=5)
op <- par(lwd=1.5,bty="l",cex.lab=1,mar=c(5,6,4,2)+0.1)
lo <- layout(mat=matrix(1:3,nrow=1,ncol=3))

# RR Bias, ANEMIA
plot(1:14,1:14,type="n",
	main= "Hb < 110 g/l",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,labels=sprintf("%1.0f",ytics),las=1)
	mtext("% Bias",side=3,line=0,at=-0.5,cex=0.75)
	# segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	# segments(0,1,14,1,lwd=1)
	segments(0,ytics,14,ytics,col="gray80",lwd=1)
	lines(1:14,pctbias(aihp$RRdp,aihp$bRRdp),lty=ltys[1])
	lines(1:14,pctbias(aimp$RRdp,aimp$bRRdp),lty=ltys[2])
	lines(1:14,pctbias(aind$RRdp,aind$bRRdp),lty=ltys[3])
	lines(1:14,pctbias(aper$RRdp,aper$bRRdp),lty=ltys[4])
	lines(1:14,pctbias(asen$RRdp,asen$bRRdp),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")

# RR Bias, HAZ
plot(1:14,1:14,type="n",
	main= "HAZ < -2",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,labels=sprintf("%1.0f",ytics),las=1)
	mtext("% Bias",side=3,line=0,at=-0.5,cex=0.75)
	# segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	# segments(0,1,14,1,lwd=1)
	segments(0,ytics,14,ytics,col="gray80",lwd=1)
	lines(1:14,pctbias(hihp$RRdp,hihp$bRRdp),lty=ltys[1])
	lines(1:14,pctbias(himp$RRdp,himp$bRRdp),lty=ltys[2])
	lines(1:14,pctbias(hind$RRdp,hind$bRRdp),lty=ltys[3])
	lines(1:14,pctbias(hper$RRdp,hper$bRRdp),lty=ltys[4])
	lines(1:14,pctbias(hsen$RRdp,hsen$bRRdp),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")


# RR Bias, WAZ
plot(1:14,1:14,type="n",
	main= "WAZ < -2",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,labels=sprintf("%1.0f",ytics),las=1)
	mtext("% Bias",side=3,line=0,at=-0.5,cex=0.75)
	# segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	# segments(0,1,14,1,lwd=1)
	segments(0,ytics,14,ytics,col="gray80",lwd=1)
	lines(1:14,pctbias(wihp$RRdp,wihp$bRRdp),lty=ltys[1])
	lines(1:14,pctbias(wimp$RRdp,wimp$bRRdp),lty=ltys[2])
	lines(1:14,pctbias(wind$RRdp,wind$bRRdp),lty=ltys[3])
	lines(1:14,pctbias(wper$RRdp,wper$bRRdp),lty=ltys[4])
	lines(1:14,pctbias(wsen$RRdp,wsen$bRRdp),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")


par(op)
dev.off()


#--------------------------------------
# Bias for RD daily prevalence
#--------------------------------------

# NOT USED: % BIAS DOESN'T SCALE B/C BASE RISKS ARE SO SMALL
# ABSOLUTE BIAS MAKES MORE SENSE FOR THE RD

# # pdf("~/dropbox/articles/wsp-recall/figures/fever/empirical-fever-BIAS-RDdp.pdf",width=15,height=5)
# op <- par(lwd=1.5,bty="l",cex.lab=1,mar=c(5,6,4,2)+0.1)
# lo <- layout(mat=matrix(1:3,nrow=1,ncol=3))

# # Bias, ANEMIA
# plot(1:14,1:14,type="n",
	# main= "Hb < 110 g/l",
	# bty="l",
	# xlab="Recall period (days)",xaxt="n",
	# ylab="",yaxt="n",ylim=range(ytics),
	# las=1
	# )
	# axis(1,at=1:14)
	# axis(2,at=ytics,labels=sprintf("%1.0f",ytics),las=1)
	# mtext("% Bias",side=3,line=0,at=-0.5,cex=0.75)
	# # segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	# # segments(0,1,14,1,lwd=1)
	# segments(0,ytics,14,ytics,col="gray80",lwd=1)
	# lines(1:14,pctbias(aihp$RDdp,aihp$bRDdp),lty=ltys[1])
	# lines(1:14,pctbias(aimp$RDdp,aimp$bRDdp),lty=ltys[2])
	# lines(1:14,pctbias(aind$RDdp,aind$bRDdp),lty=ltys[3])
	# lines(1:14,pctbias(aper$RDdp,aper$bRDdp),lty=ltys[4])
	# lines(1:14,pctbias(asen$RDdp,asen$bRDdp),lty=ltys[5])
	# legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")

# # Bias, HAZ
# plot(1:14,1:14,type="n",
	# main= "HAZ < -2",
	# bty="l",
	# xlab="Recall period (days)",xaxt="n",
	# ylab="",yaxt="n",ylim=range(ytics),
	# las=1
	# )
	# axis(1,at=1:14)
	# axis(2,at=ytics,labels=sprintf("%1.0f",ytics),las=1)
	# mtext("% Bias",side=3,line=0,at=-0.5,cex=0.75)
	# # segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	# # segments(0,1,14,1,lwd=1)
	# segments(0,ytics,14,ytics,col="gray80",lwd=1)
	# lines(1:14,pctbias(hihp$RDdp,hihp$bRDdp),lty=ltys[1])
	# lines(1:14,pctbias(himp$RDdp,himp$bRDdp),lty=ltys[2])
	# lines(1:14,pctbias(hind$RDdp,hind$bRDdp),lty=ltys[3])
	# lines(1:14,pctbias(hper$RDdp,hper$bRDdp),lty=ltys[4])
	# lines(1:14,pctbias(hsen$RDdp,hsen$bRDdp),lty=ltys[5])
	# legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")


# # Bias, WAZ
# plot(1:14,1:14,type="n",
	# main= "WAZ < -2",
	# bty="l",
	# xlab="Recall period (days)",xaxt="n",
	# ylab="",yaxt="n",ylim=range(ytics),
	# las=1
	# )
	# axis(1,at=1:14)
	# axis(2,at=ytics,labels=sprintf("%1.0f",ytics),las=1)
	# mtext("% Bias",side=3,line=0,at=-0.5,cex=0.75)
	# # segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	# # segments(0,1,14,1,lwd=1)
	# segments(0,ytics,14,ytics,col="gray80",lwd=1)
	# lines(1:14,pctbias(wihp$RDdp,wihp$bRDdp),lty=ltys[1])
	# lines(1:14,pctbias(wimp$RDdp,wimp$bRDdp),lty=ltys[2])
	# lines(1:14,pctbias(wind$RDdp,wind$bRDdp),lty=ltys[3])
	# lines(1:14,pctbias(wper$RDdp,wper$bRDdp),lty=ltys[4])
	# lines(1:14,pctbias(wsen$RDdp,wsen$bRDdp),lty=ltys[5])
	# legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")


# par(op)
# dev.off()





