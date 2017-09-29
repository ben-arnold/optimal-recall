

#--------------------------------------
# recall-empirical-plots-diar-bygroup.R
# Ben Arnold
# 28 Dec 2011
#
# Plot the daily prevalence over different
# recall periods, stratified by group
# (anemia, HAZ, WAZ)
#
# Also plot the relative daily reporting
# over different recall periods
#
# Version 2 (25 April 2012)
# Changed relative daily reporting to use 1-2 days as base
#
#--------------------------------------

library(foreign)
rm(list=ls())

#--------------------------------------
# Load analysis dataset
#--------------------------------------

d <- read.dta("~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta")



#--------------------------------------
# Calculate daily means by group
#--------------------------------------

dataset <- d$dataset
hb <- d$anemia
haz <- d$haz2
waz <- d$waz2
X <- d[,c(paste("diar0",1:9,sep=""),paste("diar",10:14,sep=""))]

matmeans <- function(days,country,char) {
	# calculate matrix means for a matrix (days), by country & characteristic (char)
	# the country relies on the variable 'dataset' above, and the characteristic is binary 0/1
	matrix(c(colMeans(days[dataset==country & char==0,],na.rm=TRUE),colMeans(X[dataset==country & char==1,],na.rm=TRUE)),nrow=2,ncol=14,byrow=TRUE)
}


#--------------------------------------
# Anemia
#--------------------------------------
hb.ihp <- matmeans(X,"India HP",hb)
hb.imp <- matmeans(X,"India MP",hb)
hb.ind <- matmeans(X,"Indonesia",hb)
hb.per <- matmeans(X,"Peru",hb)
hb.sen <- matmeans(X,"Senegal",hb)

#--------------------------------------
# Stunting
#--------------------------------------
haz.ihp <- matmeans(X,"India HP",haz)
haz.imp <- matmeans(X,"India MP",haz)
haz.ind <- matmeans(X,"Indonesia",haz)
haz.per <- matmeans(X,"Peru",haz)
haz.sen <- matmeans(X,"Senegal",haz)

#--------------------------------------
# Underweight
#--------------------------------------
waz.ihp <- matmeans(X,"India HP",waz)
waz.imp <- matmeans(X,"India MP",waz)
waz.ind <- matmeans(X,"Indonesia",waz)
waz.per <- matmeans(X,"Peru",waz)
waz.sen <- matmeans(X,"Senegal",waz)


#--------------------------------------
# Calculate relative daily reporting by group
#--------------------------------------

RDR <- function(pmat) {
	# calculate relative daily reporting for a matrix of daily prevalence values (pmat)
	# calculates relative reporting compared to the day 1-3 average
	avg <- rowMeans(pmat[,1:2])
	rdr0 <- pmat[1,]/avg[1]
	rdr1 <- pmat[2,]/avg[2]
	matrix(c(rdr0,rdr1),nrow=2,ncol=14,byrow=TRUE)
	
}

#--------------------------------------
# Anemia
#--------------------------------------
hb.rdr.ihp <- RDR(hb.ihp)
hb.rdr.imp <- RDR(hb.imp)
hb.rdr.ind <- RDR(hb.ind)
hb.rdr.per <- RDR(hb.per)
hb.rdr.sen <- RDR(hb.sen)

#--------------------------------------
# Stunting
#--------------------------------------
haz.rdr.ihp <- RDR(haz.ihp)
haz.rdr.imp <- RDR(haz.imp)
haz.rdr.ind <- RDR(haz.ind)
haz.rdr.per <- RDR(haz.per)
haz.rdr.sen <- RDR(haz.sen)

#--------------------------------------
# Underweight
#--------------------------------------
waz.rdr.ihp <- RDR(waz.ihp)
waz.rdr.imp <- RDR(waz.imp)
waz.rdr.ind <- RDR(waz.ind)
waz.rdr.per <- RDR(waz.per)
waz.rdr.sen <- RDR(waz.sen)




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
# Calculate bootstrapped 95% CIs
# for the difference between groups
# in relative daily reporting
#--------------------------------------
RDRdiff <- function(pmat1,pmat0) {
	# calculate difference in relative daily reporting
	# and bootstrapped 95% CI from the empirical output
	avg1 <- rowMeans(pmat1[,1:2])
	avg0 <- rowMeans(pmat0[,1:2])
	rdr1 <- pmat1/avg1
	rdr0 <- pmat0/avg0
	
	rdrdiff <- rdr1-rdr0
	rdrmudiff <- colMeans(rdrdiff)
	rdrdiffci <- apply(rdrdiff,2,function(x) quantile(x,probs=c(0.025,0.975)) )
	list(diff=rdrmudiff,ci=rdrdiffci)
}

hb.rdrd.ihp <- RDRdiff(aihp$mp1,aihp$mp0)
hb.rdrd.imp <- RDRdiff(aimp$mp1,aimp$mp0)
hb.rdrd.ind <- RDRdiff(aind$mp1,aind$mp0)
hb.rdrd.per <- RDRdiff(aper$mp1,aper$mp0)
hb.rdrd.sen <- RDRdiff(asen$mp1,asen$mp0)

haz.rdrd.ihp <- RDRdiff(hihp$mp1,hihp$mp0)
haz.rdrd.imp <- RDRdiff(himp$mp1,himp$mp0)
haz.rdrd.ind <- RDRdiff(hind$mp1,hind$mp0)
haz.rdrd.per <- RDRdiff(hper$mp1,hper$mp0)
haz.rdrd.sen <- RDRdiff(hsen$mp1,hsen$mp0)

waz.rdrd.ihp <- RDRdiff(wihp$mp1,wihp$mp0)
waz.rdrd.imp <- RDRdiff(wimp$mp1,wimp$mp0)
waz.rdrd.ind <- RDRdiff(wind$mp1,wind$mp0)
waz.rdrd.per <- RDRdiff(wper$mp1,wper$mp0)
waz.rdrd.sen <- RDRdiff(wsen$mp1,wsen$mp0)


#RDRdplot(haz.rdrd.ind$diff,haz.rdrd.ind$ci)


#--------------------------------------
# Prevalence and Relative Daily Reporting
# Plotting functions
#--------------------------------------


Pplot <- function(p,ytics,clabels=NULL,main="",xlab="") {
	
	plot(1:14,1:14,type="n",
		main=main,
		bty="l",
		xlab=xlab,xaxt="n",
		ylab="",yaxt="n",ylim=range(ytics),
		las=1
	)
	
	axis(1,at=1:14)
	axis(2,at=ytics,ylog=TRUE,labels=sprintf("%1.0f",ytics),las=1)
	segments(1,ytics,14,ytics,col="gray80")
	#mtext("%",side=3,line=0,at=-0.5)
	
	lines(1:14,p[1,]*100,lty=ltys[1])
	lines(1:14,p[2,]*100,lty=ltys[2])

	if(!is.null(clabels)) legend(1,max(ytics)+2.5, legend=clabels,cex=1.5,lty=ltys,bty="n",horiz=TRUE)

}



RDRplot <- function(rdr,ytics,clabels=NULL,main="",xlab="",ylab="") {
	
	plot(1:14,1:14,type="n",
		main=main,
		bty="n",
		xlab=xlab,xaxt="n",
		ylab=ylab,yaxt="n",ylim=range(ytics),
		las=1
	)
	
	axis(1,at=1:14,lwd=0,line=-1.5,cex.axis=1)
	axis(2,at=ytics,ylog=TRUE,labels=sprintf("%1.1f",ytics),las=1)
	segments(1,ytics,14,ytics,col="gray80")
	
	lines(1:14,rdr[1,],lty=ltys[1])
	lines(1:14,rdr[2,],lty=ltys[2])

	if(!is.null(clabels)) legend(1,max(ytics)+0.3, legend=clabels,cex=1.25,lty=ltys,bty="n",horiz=TRUE)

}

RDRdplot <- function(diff,ci,ytics,xlab="",ylab="") {
	plot(1:14,1:14,type="n",
		ylim=range(ytics),ylab=ylab,yaxt="n",
		xlab=xlab,xaxt="n",
		bty="n",
		las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,labels=sprintf("%1.1f",ytics),las=1)
	segments(1,0,14,0,lty=1,lwd=1.5,col="gray60")
	segments(1:14,ci[1,],1:14,ci[2,])
	points(1:14,diff,pch=21,bg="white")	
}



#--------------------------------------
# Relative Daily Reporting, Diarreha
#--------------------------------------

lo <- layout(mat=matrix(1:2,nrow=2,ncol=1),height=c(1,0.5))
op <- par(mar=c(1,4,2,1)+0.1,xpd=TRUE,lwd=1.5)
RDRplot(hb.rdr.ihp,ytics,main="",ylab="Relative Daily Reporting (days 1-2 as base)",clabels=c("Hb >= 110 g/l","Hb < 110 g/l"))
op <- par(mar=c(4,4,0,1)+0.1,xpd=TRUE,lwd=1.5)
RDRdplot(hb.rdrd.ihp$diff,hb.rdrd.ihp$ci,ytics=seq(-1,2,by=1),xlab="Day of Recall (0 = day of survey)")
par(op)




pdf("~/dropbox/articles/wsp-recall/figures/diar/empirical-diar-RDR-bygroup.pdf",width=12,height=15)
lo <- layout(mat=matrix(1:40,nrow=10,ncol=4,byrow=TRUE),widths=c(0.5,1,1,1),height=rep(c(1,0.75),5) )
ytics=seq(0,1.4,by=0.2)
dytics <-seq(-1,1,by=0.5)
ltys <- c(1,2)

labcex <- 1.2
RDRtext <- "Diarrhea"

# India HP
op <- par(mar=c(1,5,2,1)+0.1,xpd=TRUE,lwd=1.5)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("India HP",RDRtext),bty="n",cex=labcex)
RDRplot(hb.rdr.ihp,ytics,main="",clabels=c("Hb >= 110 g/l","Hb < 110 g/l"),ylab="Relative Daily Reporting\n(days 1-2 as base)")
RDRplot(haz.rdr.ihp,ytics,main="",clabels=c("HAZ >= -2","HAZ < -2"))
RDRplot(waz.rdr.ihp,ytics,main="",clabels=c("WAZ >= -2","WAZ < -2"))

op <- par(mar=c(4,5,0,1)+0.1,xpd=TRUE,lwd=1.5)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
RDRdplot(hb.rdrd.ihp$diff,hb.rdrd.ihp$ci,ytics=dytics,xlab="Day of Recall (0 = day of survey)",ylab="Difference")
RDRdplot(haz.rdrd.ihp$diff,haz.rdrd.ihp$ci,ytics=dytics,xlab="Day of Recall (0 = day of survey)")
RDRdplot(waz.rdrd.ihp$diff,waz.rdrd.ihp$ci,ytics=dytics,xlab="Day of Recall (0 = day of survey)")
	
# India MP
op <- par(mar=c(1,5,2,1)+0.1,xpd=TRUE,lwd=1.5)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("India MP", RDRtext),bty="n",cex=labcex)
RDRplot(hb.rdr.imp,ytics,main="",ylab="Relative Daily Reporting\n(days 1-2 as base)")
RDRplot(haz.rdr.imp,ytics,main="")
RDRplot(waz.rdr.imp,ytics,main="")

op <- par(mar=c(4,5,0,1)+0.1,xpd=TRUE,lwd=1.5)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
RDRdplot(hb.rdrd.imp$diff,hb.rdrd.imp$ci,ytics=dytics,xlab="Day of Recall (0 = day of survey)",ylab="Difference")
RDRdplot(haz.rdrd.imp$diff,haz.rdrd.imp$ci,ytics=dytics,xlab="Day of Recall (0 = day of survey)")
RDRdplot(waz.rdrd.imp$diff,waz.rdrd.imp$ci,ytics=dytics,xlab="Day of Recall (0 = day of survey)")

# Indonesia
op <- par(mar=c(1,5,2,1)+0.1,xpd=TRUE,lwd=1.5)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Indonesia", RDRtext),bty="n",cex=labcex)
RDRplot(hb.rdr.ind,ytics,main="",ylab="Relative Daily Reporting\n(days 1-2 as base)")
RDRplot(haz.rdr.ind,ytics,main="")
RDRplot(waz.rdr.ind,ytics,main="")

op <- par(mar=c(4,5,0,1)+0.1,xpd=TRUE,lwd=1.5)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
RDRdplot(hb.rdrd.ind$diff,hb.rdrd.ind$ci,ytics=dytics,xlab="Day of Recall (0 = day of survey)",ylab="Difference")
RDRdplot(haz.rdrd.ind$diff,haz.rdrd.ind$ci,ytics=dytics,xlab="Day of Recall (0 = day of survey)")
RDRdplot(waz.rdrd.ind$diff,waz.rdrd.ind$ci,ytics=dytics,xlab="Day of Recall (0 = day of survey)")

# Peru
op <- par(mar=c(1,5,2,1)+0.1,xpd=TRUE,lwd=1.5)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Peru", RDRtext),bty="n",cex=labcex)
RDRplot(hb.rdr.per,ytics,main="",ylab="Relative Daily Reporting\n(days 1-2 as base)")	
RDRplot(haz.rdr.per,ytics,main="")
RDRplot(waz.rdr.per,ytics,main="")

op <- par(mar=c(4,5,0,1)+0.1,xpd=TRUE,lwd=1.5)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
RDRdplot(hb.rdrd.per$diff,hb.rdrd.per$ci,ytics=dytics,xlab="Day of Recall (0 = day of survey)",ylab="Difference")
RDRdplot(haz.rdrd.per$diff,haz.rdrd.per$ci,ytics=dytics,xlab="Day of Recall (0 = day of survey)")
RDRdplot(waz.rdrd.per$diff,waz.rdrd.per$ci,ytics=dytics,xlab="Day of Recall (0 = day of survey)")

# Senegal
op <- par(mar=c(1,5,2,1)+0.1,xpd=TRUE,lwd=1.5)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Senegal", RDRtext),bty="n",cex=labcex)
RDRplot(hb.rdr.sen,ytics,main="",ylab="Relative Daily Reporting\n(days 1-2 as base)")
RDRplot(haz.rdr.sen,ytics,main="")
RDRplot(waz.rdr.sen,ytics,main="")

op <- par(mar=c(4,5,0,1)+0.1,xpd=TRUE,lwd=1.5)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
RDRdplot(hb.rdrd.sen$diff,hb.rdrd.sen$ci,ytics=dytics,xlab="Day of Recall (0 = day of survey)",ylab="Difference")
RDRdplot(haz.rdrd.sen$diff,haz.rdrd.sen$ci,ytics=dytics,xlab="Day of Recall (0 = day of survey)")
RDRdplot(waz.rdrd.sen$diff,waz.rdrd.sen$ci,ytics=dytics,xlab="Day of Recall (0 = day of survey)")


par(op)
dev.off()


#--------------------------------------
# Daily Point Prevalence, Diarreha
#--------------------------------------


pdf("~/dropbox/articles/wsp-recall/figures/diar/empirical-diar-prev-bygroup.pdf",width=13,height=14)
lo <- layout(mat=matrix(1:20,nrow=5,ncol=4),widths=rep(c(0.5,1,1,1),5) )
op <- par(mar=c(4,2,2,1)+0.1,xpd=TRUE,lwd=1.5)
ytics=seq(0,14,by=2)
ltys <- c(1,2)

labcex <- 1.2
RDRtext <- "Diarrhea\nDaily Point\nPrevalence (%)"
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("India HP",RDRtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("India MP", RDRtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Indonesia", RDRtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Peru", RDRtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Senegal", RDRtext),bty="n",cex=labcex)

Pplot(hb.ihp,ytics,main="",clabels=c("Hb >= 110 g/l","Hb < 110 g/l"))
Pplot(hb.imp,ytics,main="")
Pplot(hb.ind,ytics,main="")
Pplot(hb.per,ytics,main="")
Pplot(hb.sen,ytics,main="",xlab="Day of Recall (0 = day of survey)")

Pplot(haz.ihp,ytics,main="",clabels=c("HAZ >= -2","HAZ < -2"))
Pplot(haz.imp,ytics,main="")
Pplot(haz.ind,ytics,main="")
Pplot(haz.per,ytics,main="")
Pplot(haz.sen,ytics,main="",xlab="Day of Recall (0 = day of survey)")

Pplot(waz.ihp,ytics,main="",clabels=c("WAZ >= -2","WAZ < -2"))
Pplot(waz.imp,ytics,main="")
Pplot(waz.ind,ytics,main="")
Pplot(waz.per,ytics,main="")
Pplot(waz.sen,ytics,main="",xlab="Day of Recall (0 = day of survey)")


par(op)
dev.off()



