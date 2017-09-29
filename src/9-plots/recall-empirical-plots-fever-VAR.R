

#--------------------------------------
# recall-empirical-plots-fever-VAR.R
# Ben Arnold
# 22 April 2012
#
# Summarize the % change in SE
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
# Global parameters for the % change SE plots
#--------------------------------------
#cols <- brewer.pal(7,"Set1")[c(1:5,7)]
#cols <- c(brewer.pal(12,"Paired")[seq(2,10,by=2)],brewer.pal(9,"Greys")[8])
clabels <- c("India HP","India MP","Indonesia","Peru","Senegal")
ltys <- c(1,2,4,5,6)
ytics <- seq(-70,40,by=10)


#--------------------------------------
# % change in SE for PR, period prevalence
#--------------------------------------

pdf("~/dropbox/articles/wsp-recall/figures/fever/empirical-fever-VAR-RRpp.pdf",width=15,height=5)
op <- par(lwd=1.5,bty="l",cex.lab=1,mar=c(5,6,4,2)+0.1)
lo <- layout(mat=matrix(1:3,nrow=1,ncol=3),widths=c(1,1,1))

# plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	# legend("center",legend=c("% Change in SE\n(Day 2 = Reference)"),bty="n",cex=1.5)

# PR, % change SE, ANEMIA
plot(1:14,1:14,type="n",
	main= "Hb < 110 g/l",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="Percent change in SE (Day 2 = Reference)",yaxt="n",ylim=range(ytics),
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,labels=sprintf("%1.0f",ytics),las=1)
	# mtext("% Change SE",side=3,line=0,at=-0.5,cex=0.75)
	segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	segments(0,0,14,0,lwd=1)
	segments(0,ytics,14,ytics,col="gray80",lwd=1)
	lines(1:14,100*(aihp$RRppSE/aihp$RRppSE[2] - 1),lty=ltys[1])
	lines(1:14,100*(aimp$RRppSE/aimp$RRppSE[2] - 1),lty=ltys[2])
	lines(1:14,100*(aind$RRppSE/aind$RRppSE[2] - 1),lty=ltys[3])
	lines(1:14,100*(aper$RRppSE/aper$RRppSE[2] - 1),lty=ltys[4])
	lines(1:14,100*(asen$RRppSE/asen$RRppSE[2] - 1),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")

# PR, % change SE, HAZ
plot(1:14,1:14,type="n",
	main= "HAZ < -2",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,labels=sprintf("%1.0f",ytics),las=1)
	#mtext("% Change SE",side=3,line=0,at=-0.5,cex=0.75)
	segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	segments(0,0,14,0,lwd=1)
	segments(0,ytics,14,ytics,col="gray80",lwd=1)
	lines(1:14,100*(hihp$RRppSE/hihp$RRppSE[2] - 1),lty=ltys[1])
	lines(1:14,100*(himp$RRppSE/himp$RRppSE[2] - 1),lty=ltys[2])
	lines(1:14,100*(hind$RRppSE/hind$RRppSE[2] - 1),lty=ltys[3])
	lines(1:14,100*(hper$RRppSE/hper$RRppSE[2] - 1),lty=ltys[4])
	lines(1:14,100*(hsen$RRppSE/hsen$RRppSE[2] - 1),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")


# PR, % change SE, WAZ
plot(1:14,1:14,type="n",
	main= "WAZ < -2",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,labels=sprintf("%1.0f",ytics),las=1)
	#mtext("% Change SE",side=3,line=0,at=-0.5,cex=0.75)
	segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	segments(0,0,14,0,lwd=1)
	segments(0,ytics,14,ytics,col="gray80",lwd=1)
	lines(1:14,100*(wihp$RRppSE/wihp$RRppSE[2] - 1),lty=ltys[1])
	lines(1:14,100*(wimp$RRppSE/wimp$RRppSE[2] - 1),lty=ltys[2])
	lines(1:14,100*(wind$RRppSE/wind$RRppSE[2] - 1),lty=ltys[3])
	lines(1:14,100*(wper$RRppSE/wper$RRppSE[2] - 1),lty=ltys[4])
	lines(1:14,100*(wsen$RRppSE/wsen$RRppSE[2] - 1),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")


par(op)
dev.off()





