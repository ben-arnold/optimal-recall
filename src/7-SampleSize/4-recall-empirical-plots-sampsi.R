


#--------------------------------------------
# Program:     4-recall-empirical-plots-sampsi.R
# Programmer:  Ben Arnold
# Date:        24 May 2012
# Description:
#
# Calculate sample sizes required for
# different scenarios that correspond to each
# country
# Calculate N for RR = 0.8
# 
# Make plots
#
#
# Updated in May 2012 to format plots for AJE
#
#--------------------------------------------


rm(list=ls())
require(xtable)
require(foreign)

#-----------------------------------------
# binary rep meas sample size function
# (copied from ~/dropbox/code/rep-meas-sampsi-pow.R)
# Need to modify to include continuous sampsi
#
# estimate sample size for repeated binary 
# measures using the equation from Leon 2004
# (originally derived in Diggle et al. 2002)
#-----------------------------------------

bsampsi <- function(d,n,p0,r,alpha=0.05,beta=0.2) {
	# d  effect size (difference in prevalence)
	# p0 proportion in tx 0
	# n  number of post-baseline measurements per cluster
	# r  ICC for measures
	# alpha type I error
	# beta type II error
	p1 = p0+d
	za <- qnorm(1-(alpha/2))
	zb <- qnorm(1-beta)
	s0 <- p0*(1-p0)
	s1 <- p1*(1-p1)
	p  <- (p0+p1)/2
	q  <- 1-p
	q0 <- 1-p0
	q1 <- 1-p1
	
	a  <- (1+(n-1)*r)
	b  <- ( za*sqrt(2*p*q) + zb*sqrt(p0*q0+p1*q1) )^2
	c  <- n*(d^2)
	# return(  ceiling(a*b*(1/c))  )
	# take out ceiling for cleaner plots
	return(  a*b*(1/c)  )
}

# validation, row 1 in Table 1 of Leon 2004
# should be:
# 120 93 80 72 67 63 60 58 56
# mapply(bsampsi,n=2:10,MoreArgs=list(p0=0.1,d=0.1,r=0.2))

#--------------------------------------------
# function to calculate sample sizes for daily prevalence
#--------------------------------------------

dpss <- function(prev,corr) {
	# prev = daily point prevalence part of the prevalence matrix (objects dp,cp,fp)
	# corr = correlation between average daily prevalence (objects dc,cc,fc)
	require(xtable)

	#--------------------------------------------	
	# Calculate the average point prevalence over different recall windows
	avepp <- matrix(numeric(0),nrow=5,ncol=14)
	avepp[,1] <- prev[1:5,4]
	for(i in 2:14) {
		j <- i-1
		avepp[,i] <- rowMeans(prev[1:5,4:(4+j)])
		
		}
	#--------------------------------------------
	# Fixed Parameters
	# NOTE: that for *Relative Sample Size*
	# the following variables are irrelvant because they cancel out:
	# ICC
	# n (per child)
	# alpha / beta
	# RR
	rr <- 0.8
	# Observations per child
	Ns <- matrix(rep(1:14,5),nrow=5,byrow=TRUE)
	# Correlations
	Rs <- as.matrix(cbind(corr01=rep(0,5),corr[,2:14]),nrow=5)
	
	#--------------------------------------------
	# Calculate sample sizes
	p0 <- avepp
	p1 <- p0*rr
	ds <- p1-p0
	#ss   <- matrix(mapply(bsampsi,d=ds,n=n,p0=p0,r=Rs),nrow=5)
	ss <- matrix(numeric(0),nrow=5,ncol=14)
	for (i in 1:5) {
		for (j in 1:14) {
			ss[i,j] <- bsampsi(d=ds[i,j],n=Ns[i,j],p0=p0[i,j],r=Rs[i,j])
			
		}	
	}
	
	#--------------------------------------------
	# Calculate relative samples sizes
	# use 7 day recall as a basis of comparison
	refd <- 7
	rss  <- ss/ss[,refd]
	sspc <- 100*(ss-ss[,refd])/ss[,refd]
	clabels <- c("India HP","India MP","Indonesia","Peru","Senegal")
	rownames(ss) <- rownames(rss) <- rownames(sspc) <- clabels

	list(ss=ss,rss=rss,sspc=sspc)
	
	
}



#--------------------------------------------
# function to calculate sample sizes for period prevalence
#--------------------------------------------

ppss <- function(prev) {
	# prev: part of the prevalence matrix read in above (objects dp,cp,fp)
	
	#--------------------------------------------
	# Fixed Parameters
	# NOTE: that for *Relative Sample Size*
	# the following variables are irrelvant because they cancel out:
	# ICC
	# n (per child)
	# alpha / beta
	# RR
	rr <- 0.8
	# Observations per child
	n <- 1
	# ICCs
	icc <- 0

	#--------------------------------------------
	# Calculate sample sizes
	p0 <- prev[6:10,4:17]
	p1 <- p0*rr
	ds <- p1-p0
	ss   <- mapply(bsampsi,d=ds,n=n,p0=p0,r=icc)
	
	#--------------------------------------------
	# Calculate relative samples sizes
	# use 7 day recall as a basis of comparison
	refd <- 7
	rss  <- ss/ss[,refd]
	sspc <- 100*(ss-ss[,refd])/ss[,refd]
	clabels <- c("India HP","India MP","Indonesia","Peru","Senegal")
	rownames(ss) <- rownames(rss) <- rownames(sspc) <- clabels

	list(ss=ss,rss=rss,sspc=sspc)
	
}

#--------------------------------------------
# function to output sample size info to HTML
#--------------------------------------------

ssdump <- function(ppss,pprss,ppsspc,dpss,dprss,dpsspc,re,outfile) {
	# ss, rss, sspc generated from the functions dpss and ppss
	# outfile = name of file for HTML table output
	
	#--------------------------------------------
	# Output table for sample sizes and relative
	# sample sizes
	print(xtable(t(ppss),digits=0,caption="Sample Size Required (RR=0.8), period prevalence \n(File created by recall-empirical-plots-sampsi.R)"),type="html",file=outfile,caption="top")
	print(xtable(t(pprss),digits=4,caption="Relative Sample Size, period prevalence"),type="html",file=outfile,caption="top",append=TRUE)
	print(xtable(t(ppsspc),digits=2,caption="Percentage Change in Sample Size (%), period prevalence"),type="html",file=outfile,caption="top",append=TRUE)
	
	print(xtable(t(dpss),digits=0,caption="Sample Size Required (RR=0.8), daily prevalence"),type="html",file=outfile,caption="top",append=TRUE)
	print(xtable(t(dprss),digits=4,caption="Relative Sample Size, daily prevalence"),type="html",file=outfile,caption="top",append=TRUE)
	print(xtable(t(dpsspc),digits=2,caption="Percentage Change in Sample Size (%), daily prevalence"),type="html",file=outfile,caption="top",append=TRUE)
	
	print(xtable(t(re),digits=2,caption="Relative Sample Size Daily Point vs. Period Prev"),type="html",file=outfile,caption="top",append=TRUE)
	
}



#--------------------------------------------
# Retrieve Prevalence Estimates
#--------------------------------------------

d <- read.table("~/dropbox/articles/wsp-recall/data/wsp-optimal-recall-prevests.csv",sep=",",header=TRUE)

dp <- d[d$outcome=="diar",2:18]
cp <- d[d$outcome=="cough",2:18]
fp <- d[d$outcome=="fever",2:18]

#--------------------------------------------
# Retrieve Correlation in daily prevalence
#--------------------------------------------

dc <- read.dta("~/dropbox/articles/wsp-recall/tables/rawoutput/diar-daily-corr.dta")
cc <- read.dta("~/dropbox/articles/wsp-recall/tables/rawoutput/cough-daily-corr.dta")
fc <- read.dta("~/dropbox/articles/wsp-recall/tables/rawoutput/fever-daily-corr.dta")



#--------------------------------------------
# Calculate sample sizes for each outcome
#--------------------------------------------
# daily prev
ddpss <- dpss(dp,dc)
cdpss <- dpss(cp,cc)
fdpss <- dpss(fp,fc)

# period prev
dppss <- ppss(dp)
cppss <- ppss(cp)
fppss <- ppss(fp)


#--------------------------------------------
# Calculate RE of period prev vs. daily prev
#--------------------------------------------

dRE <- ddpss$ss/dppss$ss
cRE <- cdpss$ss/cppss$ss
fRE <- fdpss$ss/fppss$ss


#--------------------------------------------
# Output summary tables
#--------------------------------------------
ssdump(dppss$ss,dppss$rss,dppss$sspc,ddpss$ss,ddpss$rss,ddpss$sspc,dRE,"~/dropbox/articles/wsp-recall/tables/rawoutput/diar-sampsi-scenarios.xls")
ssdump(cppss$ss,cppss$rss,cppss$sspc,cdpss$ss,cdpss$rss,cdpss$sspc,cRE,"~/dropbox/articles/wsp-recall/tables/rawoutput/cough-sampsi-scenarios.xls")
ssdump(fppss$ss,fppss$rss,fppss$sspc,fdpss$ss,fdpss$rss,fdpss$sspc,fRE,"~/dropbox/articles/wsp-recall/tables/rawoutput/fever-sampsi-scenarios.xls")
 


#--------------------------------------------
# Plot relative sample sizes
#--------------------------------------------

rssplot <- function(rss,title) {
	
	op <- par(cex.lab=1.25)
	clabels <- c("India HP","India MP","Indonesia","Peru","Senegal")
	ytics <- seq(0.8,3.0,by=0.2)
	lwds <- 1
	ltys <- c(1,2,4,5,6)
	pchs <- c(0,1,2,5,6)
	ptcex <- 0.9
	cols <- rep("black",5)
	plot(rss[1,2:14],type="n",
		main="",
		yaxt="n",ylim=range(ytics),ylab="Relative Sample Size",
		xaxt="n",xlim=c(2,14),xlab="Recall Period, days",
		las=1,bty="l"
	
	)
	axis(1,at=2:14,las=1)
	axis(2,at=ytics,las=1)
	#segments(0,ytics,14,ytics,col="gray80")
	#segments(0,1,14,1,col="gray80")
	#segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray80")
	mtext(title,side=3,line=1,at=1,cex=1.25)
	for (i in 1:5) {
		lines(2:14,rss[i,2:14],lty=ltys[i],lwd=lwds,col=cols[i])
		points(2:14,rss[i,2:14],pch=pchs[i],cex=ptcex,col=cols[i])
	}
	legend("topright",legend=clabels,col=cols,lty=ltys,pch=pchs)
	par(op)
	
	
}

# relative sample size, period prevalence (AJE Figure 4)
pdf("~/dropbox/articles/wsp-recall/figures/optrecall-sampsi-pp.pdf",width=4,height=12)
lo <- layout(mat=matrix(1:3,ncol=1,nrow=3))
rssplot(dppss$rss,"")
rssplot(cppss$rss,"")
rssplot(fppss$rss,"")
dev.off()

#--------------------------------------------
# Plot relative efficiency of period vs.
# daily prevalence
#--------------------------------------------
REplot <- function(re,title) {
	
	op <- par(cex.lab=1.25)
	clabels <- c("India HP","India MP","Indonesia","Peru","Senegal")
	ytics <- seq(1,1.8,by=0.1)
	lwds <- 1
	ltys <- c(1,2,4,5,6)
	pchs <- c(0,1,2,5,6)
	ptcex <- 0.9
	cols <- rep("black",5)
	plot(re[1,1:7],type="n",
		main=title,
		yaxt="n",ylim=range(ytics),ylab="Relative Sample Size",
		xaxt="n",xlim=c(1,7),xlab="Recall Period, days",
		las=1,bty="l"
	
	)
	axis(1,at=1:7,las=1)
	axis(2,at=ytics,las=1)
	segments(0,ytics,14,ytics,col="gray80")
	#segments(0,1,7,1,col="gray80")
	#segments(c(2,7),min(ytics),c(2,7),max(ytics),col="gray80")
	for (i in 1:5) {
		lines(1:7,re[i,1:7],lty=ltys[i],lwd=lwds,col=cols[i])
		points(1:7,re[i,1:7],pch=pchs[i],cex=ptcex,col=cols[i])
	}
	legend("topright",legend=clabels,col=cols,lty=ltys,pch=pchs,box.col="white",bg="white",inset=0.006)
	par(op)
	
	
}


# daily prevalence vs. period prevalence (AJE Web Figure)
pdf("~/dropbox/articles/wsp-recall/figures/optrecall-sampsi-dp-v-pp.pdf",width=4,height=12)
lo <- layout(mat=matrix(1:3,ncol=1,nrow=3))
REplot(dRE,"Diarrhea\nDaily Point Prevalence vs. Period Prevalence")
REplot(cRE,"Cough\nDaily Point Prevalence vs. Period Prevalence")
REplot(fRE,"Fever\nDaily Point Prevalence vs. Period Prevalence")
dev.off()



