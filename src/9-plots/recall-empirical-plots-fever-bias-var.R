

#--------------------------------------
# recall-empirical-plots-fever-BIAS.R
# Ben Arnold
# 25 April 2012
#
# Summarize the % bias and % change
# in SE over different follow-up periods
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
#cols <- c(brewer.pal(12,"Paired")[seq(2,10,by=2)],brewer.pal(9,"Greys")[8])
clabels <- c("India HP","India MP","Indonesia","Peru","Senegal")
ltys <- c(1,2,4,5,6)
bytics <- seq(0,30,by=5)
seytics <- seq(-70,50,by=10)


#--------------------------------------
# % Bias for RR period prevalence
#--------------------------------------

pdf("~/dropbox/articles/wsp-recall/figures/fever/empirical-fever-bias-var-RRpp.pdf",width=15,height=10)
op <- par(lwd=1.5,bty="l",cex.lab=1,mar=c(5,6,4,2)+0.1)
lo <- layout(mat=matrix(1:6,nrow=2,ncol=3),widths=c(1,1,1))

# PR % Bias, ANEMIA
plot(1:14,1:14,type="n",
	main= "Anemic\nHb < 110 g/l",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="Absolute Percent Bias in PR (%)\n(Day 2 = Reference)",yaxt="n",ylim=range(bytics),
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=bytics,labels=sprintf("%1.0f",bytics),las=1)
	# mtext("% Bias",side=3,line=0,at=-0.5,cex=0.75)
	segments(c(2,7),min(bytics),c(2,7),max(bytics),col="gray60",lwd=1)
	# segments(0,1,14,1,lwd=1)
	segments(0,bytics,14,bytics,col="gray80",lwd=1)
	lines(1:14,pctbias(aihp$RRpp,aihp$bRRpp),lty=ltys[1])
	lines(1:14,pctbias(aimp$RRpp,aimp$bRRpp),lty=ltys[2])
	lines(1:14,pctbias(aind$RRpp,aind$bRRpp),lty=ltys[3])
	lines(1:14,pctbias(aper$RRpp,aper$bRRpp),lty=ltys[4])
	lines(1:14,pctbias(asen$RRpp,asen$bRRpp),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")
	
# PR, % change SE, ANEMIA
plot(1:14,1:14,type="n",
	main= "",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="Percent change in PR SE (%)\n(Day 2 = Reference)",yaxt="n",ylim=range(seytics),
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=seytics,labels=sprintf("%1.0f",seytics),las=1)
	# mtext("% Change SE",side=3,line=0,at=-0.5,cex=0.75)
	segments(c(2,7),min(seytics),c(2,7),max(seytics),col="gray60",lwd=1)
	# segments(0,1,14,1,lwd=1)
	segments(0,seytics,14,seytics,col="gray80",lwd=1)
	lines(1:14,100*(aihp$RRppSE/aihp$RRppSE[2] - 1),lty=ltys[1])
	lines(1:14,100*(aimp$RRppSE/aimp$RRppSE[2] - 1),lty=ltys[2])
	lines(1:14,100*(aind$RRppSE/aind$RRppSE[2] - 1),lty=ltys[3])
	lines(1:14,100*(aper$RRppSE/aper$RRppSE[2] - 1),lty=ltys[4])
	lines(1:14,100*(asen$RRppSE/asen$RRppSE[2] - 1),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")



# PR % Bias, HAZ
plot(1:14,1:14,type="n",
	main= "Stunted\nHAZ < -2",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(bytics),
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=bytics,labels=sprintf("%1.0f",bytics),las=1)
	# mtext("% Bias",side=3,line=0,at=-0.5,cex=0.75)
	segments(c(2,7),min(bytics),c(2,7),max(bytics),col="gray60",lwd=1)
	# segments(0,1,14,1,lwd=1)
	segments(0,bytics,14,bytics,col="gray80",lwd=1)
	lines(1:14,pctbias(hihp$RRpp,hihp$bRRpp),lty=ltys[1])
	lines(1:14,pctbias(himp$RRpp,himp$bRRpp),lty=ltys[2])
	lines(1:14,pctbias(hind$RRpp,hind$bRRpp),lty=ltys[3])
	lines(1:14,pctbias(hper$RRpp,hper$bRRpp),lty=ltys[4])
	lines(1:14,pctbias(hsen$RRpp,hsen$bRRpp),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")

# PR, % change SE, HAZ
plot(1:14,1:14,type="n",
	main= "",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(seytics),
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=seytics,labels=sprintf("%1.0f",seytics),las=1)
	#mtext("% Change SE",side=3,line=0,at=-0.5,cex=0.75)
	segments(c(2,7),min(seytics),c(2,7),max(seytics),col="gray60",lwd=1)
	# segments(0,1,14,1,lwd=1)
	segments(0,seytics,14,seytics,col="gray80",lwd=1)
	lines(1:14,100*(hihp$RRppSE/hihp$RRppSE[2] - 1),lty=ltys[1])
	lines(1:14,100*(himp$RRppSE/himp$RRppSE[2] - 1),lty=ltys[2])
	lines(1:14,100*(hind$RRppSE/hind$RRppSE[2] - 1),lty=ltys[3])
	lines(1:14,100*(hper$RRppSE/hper$RRppSE[2] - 1),lty=ltys[4])
	lines(1:14,100*(hsen$RRppSE/hsen$RRppSE[2] - 1),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")



# PR % Bias, WAZ
plot(1:14,1:14,type="n",
	main= "Underweight\nWAZ < -2",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(bytics),
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=bytics,labels=sprintf("%1.0f",bytics),las=1)
	# mtext("% Bias",side=3,line=0,at=-0.5,cex=0.75)
	segments(c(2,7),min(bytics),c(2,7),max(bytics),col="gray60",lwd=1)
	# segments(0,1,14,1,lwd=1)
	segments(0,bytics,14,bytics,col="gray80",lwd=1)
	lines(1:14,pctbias(wihp$RRpp,wihp$bRRpp),lty=ltys[1])
	lines(1:14,pctbias(wimp$RRpp,wimp$bRRpp),lty=ltys[2])
	lines(1:14,pctbias(wind$RRpp,wind$bRRpp),lty=ltys[3])
	lines(1:14,pctbias(wper$RRpp,wper$bRRpp),lty=ltys[4])
	lines(1:14,pctbias(wsen$RRpp,wsen$bRRpp),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")
	
# PR, % change SE, WAZ
plot(1:14,1:14,type="n",
	main= "",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(seytics),
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=seytics,labels=sprintf("%1.0f",seytics),las=1)
	#mtext("% Change SE",side=3,line=0,at=-0.5,cex=0.75)
	segments(c(2,7),min(seytics),c(2,7),max(seytics),col="gray60",lwd=1)
	# segments(0,1,14,1,lwd=1)
	segments(0,seytics,14,seytics,col="gray80",lwd=1)
	lines(1:14,100*(wihp$RRppSE/wihp$RRppSE[2] - 1),lty=ltys[1])
	lines(1:14,100*(wimp$RRppSE/wimp$RRppSE[2] - 1),lty=ltys[2])
	lines(1:14,100*(wind$RRppSE/wind$RRppSE[2] - 1),lty=ltys[3])
	lines(1:14,100*(wper$RRppSE/wper$RRppSE[2] - 1),lty=ltys[4])
	lines(1:14,100*(wsen$RRppSE/wsen$RRppSE[2] - 1),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")



par(op)
dev.off()




