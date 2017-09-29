#-----------------------------------------
# Programmer:    Ben Arnold
# Program:       empirical-wsp-adjusted-PRs-figures.R
# 
# description :
#
# summarize results from the estimation
# of unadjusted and adjusted PRs using
# log-binomial regression
#
# Version 1 (21 Apr 2012)
#-----------------------------------------

#-----------------------------------------
# preamble
#-----------------------------------------
rm(list=ls())
library(foreign)

#--------------------------------------
# Load base functions
#--------------------------------------
source("~/dropbox/manuscripts/wsp-recall/programs/empirical/empirical-base-functions-v2.R")


#--------------------------------------
# Load diarrhea WAZ empirical output
# (for comparison)
#--------------------------------------


# WAZ < -2
load("~/dropbox/manuscripts/wsp-recall/programs/empirical/diar/recall-empirical-diar-waz.RData")
wihp <- ihp
wimp <- imp
wind <- ind
wper <- per
wsen <- sen


#-----------------------------------------
# read in regression output
#-----------------------------------------


d <- read.dta("~/dropbox/manuscripts/wsp-recall/tables/rawoutput/PRs-unadj-adj.dta")

#-----------------------------------------
# calculate % bias
#-----------------------------------------

d$pr_pbias <- pctbias(d$pr,d$pr_bias)
d$apr_pbias <- pctbias(d$apr,d$apr_bias)

#-----------------------------------------
# calculate MSE
#-----------------------------------------

d$pr_mse <- d$pr_bias^2 + d$pr_se^2
d$apr_mse <- d$apr_bias^2 + d$apr_se^2

#-----------------------------------------
# Diarrea, WAZ detailed PR plot
#-----------------------------------------

# restrict output to diarrhea and WAZ
diarid <- grep("diar",d$outcome)
waz2id <- grep("waz2",d$exposure)
pd <- d[intersect(diarid,waz2id),]

# country-specific dataseries
ihp <- pd[pd$dataset=="India HP",]
imp <- pd[pd$dataset=="India MP",]
ind <- pd[pd$dataset=="Indonesia",]
per <- pd[pd$dataset=="Peru",]
sen <- pd[pd$dataset=="Senegal",]

# PR plot function
PRplot <- function(PR,PRlb,PRub,ytics,main="",xlab="") {
	
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

	segments(0,PR[2],14,PR[2],lwd=1.5)
	
	segments(1:14,PRlb,1:14,PRub)
	points(1:14,PR,pch=21,bg="white")
	
}

# RR plot function (for non-parametric output)
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


# make plots
pdf("~/dropbox/manuscripts/wsp-recall/figures/diar/empirical-diar-PRpp-adj.pdf",width=12,height=14)
lo <- layout(mat=matrix(1:20,nrow=5,ncol=4),widths=rep(c(0.3,1,1,1),5) )
op <- par(mar=c(4,2,2,1)+0.1)
ytics <- c(0.5,1,2,4)

labcex <- 1.2
PRtext <- "PR"
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("India HP",PRtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("India MP", PRtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Indonesia", PRtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Peru", PRtext),bty="n",cex=labcex)
plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Senegal", PRtext),bty="n",cex=labcex)
	
RRplot(p1=wihp$mdp1,p0=wihp$mdp0,RR=wihp$RRdp,ytics,main="Unadjusted\nNon-Parametric")
RRplot(p1=wimp$mdp1,p0=wimp$mdp0,RR=wimp$RRdp,ytics,main="")
RRplot(p1=wind$mdp1,p0=wind$mdp0,RR=wind$RRdp,ytics,main="")
RRplot(p1=wper$mdp1,p0=wper$mdp0,RR=wper$RRdp,ytics,main="")
RRplot(p1=wsen$mdp1,p0=wsen$mdp0,RR=wsen$RRdp,ytics,main="",xlab="Recall period (days)")	


PRplot(ihp$pr,ihp$pr_lb,ihp$pr_ub,ytics=c(0.5,1,2,4),main="Unadjusted\nLog-binomial model",xlab="")
PRplot(imp$pr,imp$pr_lb,imp$pr_ub,ytics=c(0.5,1,2,4),main="",xlab="")
PRplot(ind$pr,ind$pr_lb,ind$pr_ub,ytics=c(0.5,1,2,4),main="",xlab="")
PRplot(per$pr,per$pr_lb,per$pr_ub,ytics=c(0.5,1,2,4),main="",xlab="")
PRplot(sen$pr,sen$pr_lb,sen$pr_ub,ytics=c(0.5,1,2,4),main="",xlab="Recall Period (days)")

PRplot(ihp$apr,ihp$apr_lb,ihp$apr_ub,ytics=c(0.5,1,2,4),main="Adjusted\nLog-binomial model",xlab="")
PRplot(imp$apr,imp$apr_lb,imp$apr_ub,ytics=c(0.5,1,2,4),main="",xlab="")
PRplot(ind$apr,ind$apr_lb,ind$apr_ub,ytics=c(0.5,1,2,4),main="",xlab="")
PRplot(per$apr,per$apr_lb,per$apr_ub,ytics=c(0.5,1,2,4),main="",xlab="")
PRplot(sen$apr,sen$apr_lb,sen$apr_ub,ytics=c(0.5,1,2,4),main="",xlab="Recall Period (days)")

par(op)
dev.off()

#--------------------------------------
# PR % Bias
#--------------------------------------

#--------------------------------------
# Global parameters for the bias plots
#--------------------------------------
#cols <- brewer.pal(7,"Set1")[c(1:5,7)]
#cols <- c(brewer.pal(12,"Paired")[seq(2,10,by=2)],brewer.pal(9,"Greys")[8])
clabels <- c("India HP","India MP","Indonesia","Peru","Senegal")
ltys <- c(1,2,4,5,6)
ytics <- seq(0,50,by=10)


pdf("~/dropbox/manuscripts/wsp-recall/figures/diar/empirical-diar-BIAS-PRpp-adj.pdf",width=18,height=5)
op <- par(lwd=1.5,bty="l",cex.lab=1,mar=c(5,6,4,2)+0.1)
lo <- layout(mat=matrix(1:4,nrow=1,ncol=4),widths=c(0.6,1,1,1))

plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("% Bias","","Diarrhea","WAZ < -2"),bty="n",cex=1.5)


# PR Bias, Non-parametric
plot(1:14,1:14,type="n",
	main= "Unadjusted\nNon-Parametric",
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



# PR Bias, Unadjusted
plot(1:14,1:14,type="n",
	main= "Unadjusted\nLog-binomial model",
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
	lines(1:14,pctbias(ihp$pr,ihp$pr_bias),lty=ltys[1])
	lines(1:14,pctbias(imp$pr,imp$pr_bias),lty=ltys[2])
	lines(1:14,pctbias(ind$pr,ind$pr_bias),lty=ltys[3])
	lines(1:14,pctbias(per$pr,per$pr_bias),lty=ltys[4])
	lines(1:14,pctbias(sen$pr,sen$pr_bias),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")

# PR Bias, Adjusted
plot(1:14,1:14,type="n",
	main= "Adjusted\nLog-binomial model",
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
	lines(1:14,pctbias(ihp$apr,ihp$apr_bias),lty=ltys[1])
	lines(1:14,pctbias(imp$apr,imp$apr_bias),lty=ltys[2])
	lines(1:14,pctbias(ind$apr,ind$apr_bias),lty=ltys[3])
	lines(1:14,pctbias(per$apr,per$apr_bias),lty=ltys[4])
	lines(1:14,pctbias(sen$apr,sen$apr_bias),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")




par(op)

dev.off()

#--------------------------------------
# Relative efficiency for RR period prevalence
# % change in SE (day 2 = reference)
#--------------------------------------

#--------------------------------------
# Global parameters for the RE plots
#--------------------------------------
#cols <- brewer.pal(7,"Set1")[c(1:5,7)]
#cols <- c(brewer.pal(12,"Paired")[seq(2,10,by=2)],brewer.pal(9,"Greys")[8])
clabels <- c("India HP","India MP","Indonesia","Peru","Senegal")
ltys <- c(1,2,4,5,6)
ytics <- seq(-50,50,by=10)


pdf("~/dropbox/manuscripts/wsp-recall/figures/diar/empirical-diar-VAR-PRpp-adj.pdf",width=18,height=5)
op <- par(lwd=1.5,bty="l",cex.lab=1,mar=c(5,6,4,2)+0.1)
lo <- layout(mat=matrix(1:4,nrow=1,ncol=4),widths=c(0.6,1,1,1))

plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("% Change in SE\n(Day 2 = Reference)","","Diarrhea","WAZ < -2"),bty="n",cex=1.5)

# % change SE, Non-parametric
plot(1:14,1:14,type="n",
	main= "Unadjusted\nNon-Parametric",
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
	segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	lines(1:14,100*(wihp$RRppSE/wihp$RRppSE[2] - 1),lty=ltys[1])
	lines(1:14,100*(wimp$RRppSE/wimp$RRppSE[2] - 1),lty=ltys[2])
	lines(1:14,100*(wind$RRppSE/wind$RRppSE[2] - 1),lty=ltys[3])
	lines(1:14,100*(wper$RRppSE/wper$RRppSE[2] - 1),lty=ltys[4])
	lines(1:14,100*(wsen$RRppSE/wsen$RRppSE[2] - 1),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")



# PR % change SE, Unadjusted
plot(1:14,1:14,type="n",
	main= "Unadjusted\nLog-binomial model",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,labels=sprintf("%1.0f",ytics),las=1)
	# mtext("% Change SE",side=3,line=0,at=-0.5,cex=0.75)
	# segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	# segments(0,1,14,1,lwd=1)
	segments(0,ytics,14,ytics,col="gray80",lwd=1)
	segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)

	lines(1:14,100*(ihp$pr_se/ihp$pr_se[2] - 1),lty=ltys[1])
	lines(1:14,100*(imp$pr_se/imp$pr_se[2] - 1),lty=ltys[2])
	lines(1:14,100*(ind$pr_se/ind$pr_se[2] - 1),lty=ltys[3])
	lines(1:14,100*(per$pr_se/per$pr_se[2] - 1),lty=ltys[4])
	lines(1:14,100*(sen$pr_se/sen$pr_se[2] - 1),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")

# PR Bias, Adjusted
plot(1:14,1:14,type="n",
	main= "Adjusted\nLog-binomial model",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,labels=sprintf("%1.0f",ytics),las=1)
	# mtext("% Change SE",side=3,line=0,at=-0.5,cex=0.75)
	# segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	# segments(0,1,14,1,lwd=1)
	segments(0,ytics,14,ytics,col="gray80",lwd=1)
	segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	lines(1:14,100*(ihp$apr_se/ihp$apr_se[2] - 1),lty=ltys[1])
	lines(1:14,100*(imp$apr_se/imp$apr_se[2] - 1),lty=ltys[2])
	lines(1:14,100*(ind$apr_se/ind$apr_se[2] - 1),lty=ltys[3])
	lines(1:14,100*(per$apr_se/per$apr_se[2] - 1),lty=ltys[4])
	lines(1:14,100*(sen$apr_se/sen$apr_se[2] - 1),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")

par(op)

dev.off()


#--------------------------------------
# Relative MSE PR
# diarrhea, WAZ
#--------------------------------------

#--------------------------------------
# Global parameters for the MSE plots
#--------------------------------------

#cols <- brewer.pal(7,"Set1")[c(1:5,7)]
#cols <- c(brewer.pal(12,"Paired")[seq(2,10,by=2)],brewer.pal(9,"Greys")[8])
clabels <- c("India HP","India MP","Indonesia","Peru","Senegal")
ltys <- c(1,2,4,5,6)
#ytics <- seq(0.4,2.4,by=0.2)
ytics <- c(0.1,0.5,1,2,10)
refday <- 2

pdf("~/dropbox/manuscripts/wsp-recall/figures/diar/empirical-diar-MSE-PRpp-adj.pdf",width=18,height=5)
op <- par(lwd=1.5,bty="l",cex.lab=1,mar=c(5,6,4,2)+0.1,xpd=TRUE)
lo <- layout(mat=matrix(1:4,nrow=1,ncol=4),widths=c(0.6,1,1,1))


plot(1,1,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	legend("center",legend=c("Relative MSE\n(Day 2 = Reference)","","Diarrhea","WAZ < -2"),bty="n",cex=1.5)


# PR MSE, Non-parametric
plot(1:14,1:14,type="n",
	main= "Unadjusted\nNon-Parametric",
	bty="l",
	xlab="Recall period (days)",xaxt="n",
	ylab="",yaxt="n",ylim=range(ytics),ylog=TRUE,log="y",
	las=1
	)
	axis(1,at=1:14)
	axis(2,at=ytics,labels=sprintf("%1.0f",ytics),las=1)
	mtext("% Bias",side=3,line=0,at=-0.5,cex=0.75)
	# segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	# segments(0,1,14,1,lwd=1)
	segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray60",lwd=1)
	segments(0,1,14,1,lwd=1)
	lines(1:14,rMSE(wihp$bRRpp,wihp$RRppV,refday),lty=ltys[1])
	lines(1:14,rMSE(wimp$bRRpp,wimp$RRppV,refday),lty=ltys[2])
	lines(1:14,rMSE(wind$bRRpp,wind$RRppV,refday),lty=ltys[3])
	lines(1:14,rMSE(wper$bRRpp,wper$RRppV,refday),lty=ltys[4])
	lines(1:14,rMSE(wsen$bRRpp,wsen$RRppV,refday),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,box.col="white",bg="white")


# RR MSE, Unadjusted
plot(1:14,1:14,type="n",
	main= "Unadjusted",
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
	lines(1:14,rMSE(ihp$pr_bias,ihp$pr_se^2,refday),lty=ltys[1])
	lines(1:14,rMSE(imp$pr_bias,imp$pr_se^2,refday),lty=ltys[2])
	lines(1:14,rMSE(ind$pr_bias,ind$pr_se^2,refday),lty=ltys[3])
	lines(1:14,rMSE(per$pr_bias,per$pr_se^2,refday),lty=ltys[4])
	lines(1:14,rMSE(sen$pr_bias,sen$pr_se^2,refday),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,bty="n")

# RR MSE, Adjusted
plot(1:14,1:14,type="n",
	main= "Adjusted",
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
	lines(1:14,rMSE(ihp$apr_bias,ihp$apr_se^2,refday),lty=ltys[1])
	lines(1:14,rMSE(imp$apr_bias,imp$apr_se^2,refday),lty=ltys[2])
	lines(1:14,rMSE(ind$apr_bias,ind$apr_se^2,refday),lty=ltys[3])
	lines(1:14,rMSE(per$apr_bias,per$apr_se^2,refday),lty=ltys[4])
	lines(1:14,rMSE(sen$apr_bias,sen$apr_se^2,refday),lty=ltys[5])
	legend("topright",legend=clabels,lty=ltys,bty="n")
	

par(op)
dev.off()


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

pdf("~/dropbox/manuscripts/wsp-recall/figures/diar/empirical-diar-bias-var-MSE-PRpp-adj.pdf",width=17,height=20)
op <- par(lwd=1.5,bty="l",cex.lab=1,mar=c(5,6,4,2)+0.1)
lo <- layout(mat=matrix(1:20,nrow=5,ncol=4),widths=rep(c(0.4,1,1,1),5))


ihpy <- seq(0,0.5,by=0.05)
impy <- seq(0,0.07,by=0.01)
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


# Non-parametric
MSEplot(wihp$bRRpp,wihp$RRppV,main="Unadjusted\nNon-parametric",xlab="",ytics=ihpy)
MSEplot(wimp$bRRpp,wimp$RRppV,main="",xlab="",ytics=impy)
MSEplot(wind$bRRpp,wind$RRppV,main="",xlab="",ytics=indy)
MSEplot(wper$bRRpp,wper$RRppV,main="",xlab="",ytics=pery)
MSEplot(wsen$bRRpp,wsen$RRppV,main="",xlab="Recall period (days)",ytics=seny)

# Unadjusted, log-binomial model
MSEplot(ihp$pr_bias,ihp$pr_se^2,main="Unadjusted\nLog-binomial model",xlab="",ytics=ihpy)
MSEplot(imp$pr_bias,imp$pr_se^2,main="",xlab="",ytics=impy)
MSEplot(ind$pr_bias,ind$pr_se^2,main="",xlab="",ytics=indy)
MSEplot(per$pr_bias,per$pr_se^2,main="",xlab="",ytics=pery)
MSEplot(sen$pr_bias,sen$pr_se^2,main="",xlab="Recall period (days)",ytics=seny)

# Adjusted, log-binomial model
MSEplot(ihp$apr_bias,ihp$apr_se^2,main="Adjusted\nLog-binomial model",xlab="",ytics=ihpy)
MSEplot(imp$apr_bias,imp$apr_se^2,main="",xlab="",ytics=impy)
MSEplot(ind$apr_bias,ind$apr_se^2,main="",xlab="",ytics=indy)
MSEplot(per$apr_bias,per$apr_se^2,main="",xlab="",ytics=pery)
MSEplot(sen$apr_bias,sen$apr_se^2,main="",xlab="Recall period (days)",ytics=seny)



par(op)
dev.off()












