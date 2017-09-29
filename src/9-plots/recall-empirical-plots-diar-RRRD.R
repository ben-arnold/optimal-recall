

#--------------------------------------
# recall-empirical-plots-diar-RRRD.R
# Ben Arnold
# 28 Dec 2011
#
# Plot the empirical RR and RD over
# different recall periods for all
# countries and subgroups
#--------------------------------------

library(foreign)
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
# Plot of RRs & CIs by recall period
#--------------------------------------

RRci <- function(p1,p0) {
	RR <- p1/p0
	apply(RR,2,function(x) quantile(x,probs=c(0.025,0.975)))
}

RRplot <- function(p1,p0,RR,ytics,main="",xlab="") {
	
	cis <- RRci(p1,p0)
	plot(1:14,1:14,type="n",
		main=main,
		bty="l",
		xlab=xlab,xaxt="n",
		ylab="",yaxt="n",ylim=range(ytics),log="y",ylog=TRUE,
		las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,ylog=TRUE,labels=sprintf("%1.1f",ytics),las=1)
	segments(0,1,14,1,lty=2,lwd=1)

	segments(0,RR[2],14,RR[2],lwd=1.5)
	
	segments(1:14,cis[1,],1:14,cis[2,])
	points(1:14,RR,pch=21,bg="white")
	
}

RDci <- function(p1,p0) {
	RD <- p1-p0
	apply(RD,2,function(x) quantile(x,probs=c(0.025,0.975)))
}

RDplot <- function(p1,p0,RD,ytics,main="",xlab="") {
	
	cis <- RDci(p1,p0)
	plot(1:14,1:14,type="n",
		main=main,
		bty="l",
		xlab=xlab,xaxt="n",
		ylab="",yaxt="n",ylim=range(ytics),
		las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,labels=sprintf("%1.2f",ytics),las=1)
	segments(0,0,14,0,lty=2,lwd=1)

	segments(0,RD[2],14,RD[2],lwd=1.5)
	
	segments(1:14,cis[1,],1:14,cis[2,])
	points(1:14,RD,pch=21,bg="white")
	
}


#--------------------------------------
# RR, daily prevalence
#--------------------------------------

pdf("~/dropbox/articles/wsp-recall/figures/diar/empirical-diar-RRdp.pdf",width=12,height=14)
lo <- layout(mat=matrix(1:20,nrow=5,ncol=4),widths=rep(c(0.4,1,1,1),5) )
op <- par(mar=c(4,2,2,1)+0.1)
ytics <- c(0.5,1,2,4)

labcex <- 1.2
RRtext <- "Prevalence Ratio"
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("India HP",RRtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("India MP", RRtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Indonesia", RRtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Peru", RRtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Senegal", RRtext),bty="n",cex=labcex)

RRplot(p1=aihp$mdp1,p0=aihp$mdp0,RR=aihp$RRdp,ytics,main="Hb < 110 g/l")
RRplot(p1=aimp$mdp1,p0=aimp$mdp0,RR=aimp$RRdp,ytics,main="")
RRplot(p1=aind$mdp1,p0=aind$mdp0,RR=aind$RRdp,ytics,main="")
RRplot(p1=aper$mdp1,p0=aper$mdp0,RR=aper$RRdp,ytics,main="")
RRplot(p1=asen$mdp1,p0=asen$mdp0,RR=asen$RRdp,ytics,main="",xlab="Recall period (days)")

RRplot(p1=hihp$mdp1,p0=hihp$mdp0,RR=hihp$RRdp,ytics,main="HAZ < -2")
RRplot(p1=himp$mdp1,p0=himp$mdp0,RR=himp$RRdp,ytics,main="")
RRplot(p1=hind$mdp1,p0=hind$mdp0,RR=hind$RRdp,ytics,main="")
RRplot(p1=hper$mdp1,p0=hper$mdp0,RR=hper$RRdp,ytics,main="")
RRplot(p1=hsen$mdp1,p0=hsen$mdp0,RR=hsen$RRdp,ytics,main="",xlab="Recall period (days)")

RRplot(p1=wihp$mdp1,p0=wihp$mdp0,RR=wihp$RRdp,ytics,main="WAZ < -2")
RRplot(p1=wimp$mdp1,p0=wimp$mdp0,RR=wimp$RRdp,ytics,main="")
RRplot(p1=wind$mdp1,p0=wind$mdp0,RR=wind$RRdp,ytics,main="")
RRplot(p1=wper$mdp1,p0=wper$mdp0,RR=wper$RRdp,ytics,main="")
RRplot(p1=wsen$mdp1,p0=wsen$mdp0,RR=wsen$RRdp,ytics,main="",xlab="Recall period (days)")


par(op)
dev.off()


#--------------------------------------
# RR, period prevalence
#--------------------------------------

pdf("~/dropbox/articles/wsp-recall/figures/diar/empirical-diar-RRpp.pdf",width=12,height=14)
lo <- layout(mat=matrix(1:20,nrow=5,ncol=4),widths=rep(c(0.4,1,1,1),5) )
op <- par(mar=c(4,2,2,1)+0.1)
ytics <- c(0.5,1,2,4)

labcex <- 1.2
RRtext <- "PR"
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("India HP",RRtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("India MP", RRtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Indonesia", RRtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Peru", RRtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Senegal", RRtext),bty="n",cex=labcex)

RRplot(p1=aihp$mpp1,p0=aihp$mpp0,RR=aihp$RRpp,ytics,main="Hb < 110 g/l")
RRplot(p1=aimp$mpp1,p0=aimp$mpp0,RR=aimp$RRpp,ytics,main="")
RRplot(p1=aind$mpp1,p0=aind$mpp0,RR=aind$RRpp,ytics,main="")
RRplot(p1=aper$mpp1,p0=aper$mpp0,RR=aper$RRpp,ytics,main="")
RRplot(p1=asen$mpp1,p0=asen$mpp0,RR=asen$RRpp,ytics,main="",xlab="Recall period (days)")

RRplot(p1=hihp$mpp1,p0=hihp$mpp0,RR=hihp$RRpp,ytics,main="HAZ < -2")
RRplot(p1=himp$mpp1,p0=himp$mpp0,RR=himp$RRpp,ytics,main="")
RRplot(p1=hind$mpp1,p0=hind$mpp0,RR=hind$RRpp,ytics,main="")
RRplot(p1=hper$mpp1,p0=hper$mpp0,RR=hper$RRpp,ytics,main="")
RRplot(p1=hsen$mpp1,p0=hsen$mpp0,RR=hsen$RRpp,ytics,main="",xlab="Recall period (days)")

RRplot(p1=wihp$mpp1,p0=wihp$mpp0,RR=wihp$RRpp,ytics,main="WAZ < -2")
RRplot(p1=wimp$mpp1,p0=wimp$mpp0,RR=wimp$RRpp,ytics,main="")
RRplot(p1=wind$mpp1,p0=wind$mpp0,RR=wind$RRpp,ytics,main="")
RRplot(p1=wper$mpp1,p0=wper$mpp0,RR=wper$RRpp,ytics,main="")
RRplot(p1=wsen$mpp1,p0=wsen$mpp0,RR=wsen$RRpp,ytics,main="",xlab="Recall period (days)")


par(op)
dev.off()


#--------------------------------------
# RD, daily prevalence
#--------------------------------------

pdf("~/dropbox/articles/wsp-recall/figures/diar/empirical-diar-RDdp.pdf",width=12,height=14)
lo <- layout(mat=matrix(1:20,nrow=5,ncol=4),widths=rep(c(0.4,1,1,1),5) )
op <- par(mar=c(4,2,2,1)+0.1)
ytics <- seq(-0.04,0.1,by=0.02)

labcex <- 1.2
RDtext <- "Risk Difference"
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("India HP",RDtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("India MP", RDtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Indonesia", RDtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Peru", RDtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Senegal", RDtext),bty="n",cex=labcex)

RDplot(p1=aihp$mdp1,p0=aihp$mdp0,RD=aihp$RDdp,ytics,main="Hb < 110 g/l")
RDplot(p1=aimp$mdp1,p0=aimp$mdp0,RD=aimp$RDdp,ytics,main="")
RDplot(p1=aind$mdp1,p0=aind$mdp0,RD=aind$RDdp,ytics,main="")
RDplot(p1=aper$mdp1,p0=aper$mdp0,RD=aper$RDdp,ytics,main="")
RDplot(p1=asen$mdp1,p0=asen$mdp0,RD=asen$RDdp,ytics,main="",xlab="Recall period (days)")

RDplot(p1=hihp$mdp1,p0=hihp$mdp0,RD=hihp$RDdp,ytics,main="HAZ < -2")
RDplot(p1=himp$mdp1,p0=himp$mdp0,RD=himp$RDdp,ytics,main="")
RDplot(p1=hind$mdp1,p0=hind$mdp0,RD=hind$RDdp,ytics,main="")
RDplot(p1=hper$mdp1,p0=hper$mdp0,RD=hper$RDdp,ytics,main="")
RDplot(p1=hsen$mdp1,p0=hsen$mdp0,RD=hsen$RDdp,ytics,main="",xlab="Recall period (days)")

RDplot(p1=wihp$mdp1,p0=wihp$mdp0,RD=wihp$RDdp,ytics,main="WAZ < -2")
RDplot(p1=wimp$mdp1,p0=wimp$mdp0,RD=wimp$RDdp,ytics,main="")
RDplot(p1=wind$mdp1,p0=wind$mdp0,RD=wind$RDdp,ytics,main="")
RDplot(p1=wper$mdp1,p0=wper$mdp0,RD=wper$RDdp,ytics,main="")
RDplot(p1=wsen$mdp1,p0=wsen$mdp0,RD=wsen$RDdp,ytics,main="",xlab="Recall period (days)")


par(op)
dev.off()

#--------------------------------------
# RD, period prevalence
#--------------------------------------

pdf("~/dropbox/articles/wsp-recall/figures/diar/empirical-diar-RDpp.pdf",width=12,height=14)
lo <- layout(mat=matrix(1:20,nrow=5,ncol=4),widths=rep(c(0.4,1,1,1),5) )
op <- par(mar=c(4,2,2,1)+0.1)
ytics <- seq(-0.05,0.15,by=0.05)

labcex <- 1.2
RDtext <- "Risk Difference"
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("India HP",RDtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("India MP", RDtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Indonesia", RDtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Peru", RDtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Senegal", RDtext),bty="n",cex=labcex)

RDplot(p1=aihp$mpp1,p0=aihp$mpp0,RD=aihp$RDpp,ytics,main="Hb < 110 g/l")
RDplot(p1=aimp$mpp1,p0=aimp$mpp0,RD=aimp$RDpp,ytics,main="")
RDplot(p1=aind$mpp1,p0=aind$mpp0,RD=aind$RDpp,ytics,main="")
RDplot(p1=aper$mpp1,p0=aper$mpp0,RD=aper$RDpp,ytics,main="")
RDplot(p1=asen$mpp1,p0=asen$mpp0,RD=asen$RDpp,ytics,main="",xlab="Recall period (days)")

RDplot(p1=hihp$mpp1,p0=hihp$mpp0,RD=hihp$RDpp,ytics,main="HAZ < -2")
RDplot(p1=himp$mpp1,p0=himp$mpp0,RD=himp$RDpp,ytics,main="")
RDplot(p1=hind$mpp1,p0=hind$mpp0,RD=hind$RDpp,ytics,main="")
RDplot(p1=hper$mpp1,p0=hper$mpp0,RD=hper$RDpp,ytics,main="")
RDplot(p1=hsen$mpp1,p0=hsen$mpp0,RD=hsen$RDpp,ytics,main="",xlab="Recall period (days)")

RDplot(p1=wihp$mpp1,p0=wihp$mpp0,RD=wihp$RDpp,ytics,main="WAZ < -2")
RDplot(p1=wimp$mpp1,p0=wimp$mpp0,RD=wimp$RDpp,ytics,main="")
RDplot(p1=wind$mpp1,p0=wind$mpp0,RD=wind$RDpp,ytics,main="")
RDplot(p1=wper$mpp1,p0=wper$mpp0,RD=wper$RDpp,ytics,main="")
RDplot(p1=wsen$mpp1,p0=wsen$mpp0,RD=wsen$RDpp,ytics,main="",xlab="Recall period (days)")


par(op)
dev.off()



