
#-------------------------------------------
# Program:   recall-empirical-plots-daily-prev.R
# Programmer: Ben Arnold 
# Date:       23 May 2012
#
# Desc:
#
# Plot daily point prevalence and relative
# daily reporting.  Code updated in May 2012
# for AJE formatting.
#-------------------------------------------

 
rm(list=ls())
library(RColorBrewer)
library(foreign)

#-------------------------------------------
# Load Data
#-------------------------------------------

d <- read.dta("~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta")

# summarize daily point prevalences
dd <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14")

# diarrhea
dpihp <- colMeans(d[d$dataset=="India HP",paste("diar",dd,sep="")])
dpimp <- colMeans(d[d$dataset=="India MP",paste("diar",dd,sep="")])
dpind <- colMeans(d[d$dataset=="Indonesia",paste("diar",dd,sep="")])
dpper <- colMeans(d[d$dataset=="Peru",paste("diar",dd,sep="")])
dpsen <- colMeans(d[d$dataset=="Senegal",paste("diar",dd,sep="")])

# cough
cpihp <- colMeans(d[d$dataset=="India HP",paste("cough",dd,sep="")])
cpimp <- colMeans(d[d$dataset=="India MP",paste("cough",dd,sep="")])
cpind <- colMeans(d[d$dataset=="Indonesia",paste("cough",dd,sep="")])
cpper <- colMeans(d[d$dataset=="Peru",paste("cough",dd,sep="")])
cpsen <- colMeans(d[d$dataset=="Senegal",paste("cough",dd,sep="")])

# fever
fpihp <- colMeans(d[d$dataset=="India HP",paste("fever",dd,sep="")])
fpimp <- colMeans(d[d$dataset=="India MP",paste("fever",dd,sep="")])
fpind <- colMeans(d[d$dataset=="Indonesia",paste("fever",dd,sep="")])
fpper <- colMeans(d[d$dataset=="Peru",paste("fever",dd,sep="")])
fpsen <- colMeans(d[d$dataset=="Senegal",paste("fever",dd,sep="")])

#-------------------------------------------
# Daily Point Prevalence Plots
#-------------------------------------------

prevplot <- function(ihp,imp,ind,per,sen,title) {
	clabels <- c("India HP","India MP","Indonesia","Peru","Senegal")
	cols <- rep("black",5)
	# pchs <- c(NA,NA,NA,"+","*")
	# ptcex <- 1.25
	# ltys <- c(1,2,4,5,6)
	pchs <- c(0,1,2,5,6)
	ptcex <- 0.9
	ltys <- c(1,2,4,5,6)
	ytic <- seq(0,24,by=2)
	op <- par(lwd=1,cex.lab=1.25,cex.axis=1)

	plot(1:14,1:14,type="n",
		xaxt="n",xlab="Day of Recall",
		yaxt="n",ylab="Daily Point Prevalence, %",ylim=range(ytic),
		bty="l",
		las=1
	)
	axis(side=1,at=1:14)
	axis(side=2,at=ytic,las=1)
	mtext(title,side=3,line=1,at=1,cex=1.25)


	lines(1:14,ihp*100,col=cols[1],lty=ltys[1])
	lines(1:14,imp*100,col=cols[2],lty=ltys[2])
	lines(1:14,ind*100,col=cols[3],lty=ltys[3])
	lines(1:14,per*100,col=cols[4],lty=ltys[4])
	lines(1:14,sen*100,col=cols[5],lty=ltys[5])

	points(1:14,ihp*100,col=cols[1],pch=pchs[1],cex=ptcex)
	points(1:14,imp*100,col=cols[2],pch=pchs[2],cex=ptcex)
	points(1:14,ind*100,col=cols[3],pch=pchs[3],cex=ptcex)
	points(1:14,per*100,col=cols[4],pch=pchs[4],cex=ptcex)
	points(1:14,sen*100,col=cols[5],pch=pchs[5],cex=ptcex)

	legend("topright",legend=clabels,col=cols,pch=pchs,lty=ltys,pt.cex=ptcex)
	par(op)
	
}

pdf(file="~/dropbox/articles/wsp-recall/figures/optrecall-daily-pp.pdf",width=4,height=12)
lo <- layout(mat=matrix(1:3,ncol=1,nrow=3))
prevplot(dpihp,dpimp,dpind,dpper,dpsen,"")
prevplot(cpihp,cpimp,cpind,cpper,cpsen,"")
prevplot(fpihp,fpimp,fpind,fpper,fpsen,"")
dev.off()



#-------------------------------------------
# Point prevalence as a % of Days 1 - 2 plots
#-------------------------------------------

# calculate relative daily reporting w/ 1-2 days as base.
rdr <- function(x) {
	mu12 <- mean(x[1:2])
	return(x/mu12)
}

# diarrhea
drdrihp <- rdr(dpihp)
drdrimp <- rdr(dpimp)
drdrind <- rdr(dpind)
drdrper <- rdr(dpper)
drdrsen <- rdr(dpsen)

# cough
crdrihp <- rdr(cpihp)
crdrimp <- rdr(cpimp)
crdrind <- rdr(cpind)
crdrper <- rdr(cpper)
crdrsen <- rdr(cpsen)

# fever
frdrihp <- rdr(fpihp)
frdrimp <- rdr(fpimp)
frdrind <- rdr(fpind)
frdrper <- rdr(fpper)
frdrsen <- rdr(fpsen)


rdrplot <- function(ihp,imp,ind,per,sen,title) {
	clabels <- c("India HP","India MP","Indonesia","Peru","Senegal")
	cols <- rep("black",5)
	pchs <- c(0,1,2,5,6)
	ptcex <- 0.9
	ltys <- c(1,2,4,5,6)
	ytic <- seq(0,1.4,by=0.2)
	op <- par(lwd=1,cex.lab=1.25,cex.axis=1)

	plot(1:14,1:14,type="n",
		xaxt="n",xlab="Day of Recall",
		yaxt="n",ylab="Relative Daily Reporting",ylim=range(ytic),
		bty="l",
		las=1
	)
	axis(side=1,at=1:14)
	axis(side=2,at=ytic,las=1)
	mtext(title,side=3,line=1,at=1,cex=1.25)

	lines(1:14,ihp,col=cols[1],lty=ltys[1])
	lines(1:14,imp,col=cols[2],lty=ltys[2])
	lines(1:14,ind,col=cols[3],lty=ltys[3])
	lines(1:14,per,col=cols[4],lty=ltys[4])
	lines(1:14,sen,col=cols[5],lty=ltys[5])

	points(1:14,ihp,col=cols[1],pch=pchs[1],cex=ptcex)
	points(1:14,imp,col=cols[2],pch=pchs[2],cex=ptcex)
	points(1:14,ind,col=cols[3],pch=pchs[3],cex=ptcex)
	points(1:14,per,col=cols[4],pch=pchs[4],cex=ptcex)
	points(1:14,sen,col=cols[5],pch=pchs[5],cex=ptcex)

	legend("topright",legend=clabels,col=cols,pch=pchs,lty=ltys,pt.cex=ptcex)
	par(op)
	
}
pdf(file="~/dropbox/articles/wsp-recall/figures/optrecall-rel-daily-rep.pdf",width=4,height=12)
lo <- layout(mat=matrix(1:3,ncol=1,nrow=3))
rdrplot(drdrihp,drdrimp,drdrind,drdrper,drdrsen,"")
rdrplot(crdrihp,crdrimp,crdrind,crdrper,crdrsen,"")
rdrplot(frdrihp,frdrimp,frdrind,frdrper,frdrsen,"")
dev.off()


