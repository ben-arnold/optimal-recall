

#--------------------------------------
# empirical-base-functions-v2.R
# Ben Arnold
#
# Base functions to calculate empirical
# RR and RDs based on pure empirical
# estimates and based on simulated
# scenarios for point and period
# prevalence in two groups
#
#
# Version 3.0 23 Apr 2012
#
# modified the bias calcuations so
# that the unbiased effect is assumed
# to be over a 2 day recall period
# following a suggestion from the 
# AJE reviewer 2
#
# Version 2.0 28 Oct 2011
#
# Updated the RDRR function to calculate
# bias directly from the RR or RD
# parameters averaged over 3 days. 
#
# Deleted a few functions (e.g. CTratio) not used
#
# Version 1.0 1 Sep 2011
#--------------------------------------





#--------------------------------------
# RDRR
# Function to calculate RR & RD by
# Categorical variable (C)
# Note: code assumes C is binary
#
# Also calculates bias and variance
# of RR and RD using the day 1-3
# average point prevalence as truth
#--------------------------------------
RDRR <- function(dp,pp,pmu,C,D,ID,dataset=NULL,country=NULL,iter=10,dots=TRUE) {
	# dp   : daily point prevalence indicators (wsp-recall-data.dta)
	# pp   : period prevalence indicators (wsp-recall-data.dta)
	# pmu  : mean daily point prevalence over different recall periods
	# C    : categorical variable, same n dimension as dp and pp
	# D    : average episode duration (for period prevalence calculation)
	# ID   : ID for bootstrap resampling for CIs on empirical estimates, same n dimension as dp/pp
	# dataset: Character variable that includes country strings (below)
	# country: Country dataset string (wsp-recall-data.dta)
	# iter : number of bootstrap iterations
	# dots : logical. display bootstrap iterations

	
	# Restrict to country data (if any)
	if (!is.null(country)) {
		dp  <- dp[dataset==country,]
		pp  <- pp[dataset==country,]
		pmu <- pmu[dataset==country,]
		C   <- C[dataset==country]
		ID  <- ID[dataset==country]
		
	}
	
	# calculate means in point prevalence and period prevalence by category
	mup  <- rbind(colMeans(dp[C==1,]),colMeans(dp[C==0,]))
	mudp <- rbind(colMeans(pmu[C==1,]),colMeans(pmu[C==0,]))
	mupp <- rbind(colMeans(pp[C==1,]),colMeans(pp[C==0,]))
	colnames(mup) <- colnames(mudp) <- colnames(mupp) <- paste("day",1:14)
		
	# create a dataframe to sample from
	data <- data.frame(ID,C,pp,pmu,dp)
	
	# Draw the (clustered) bootstrap samples
	bootsmp <- matrix(sample(unique(ID), length(unique(ID))*iter, replace=TRUE), ncol=iter)
	
	# matrices to collect bootstrap samples
	mp1  <- matrix(numeric(0),nrow=iter,ncol=14)
	mp0  <- matrix(numeric(0),nrow=iter,ncol=14)
	mdp1 <- matrix(numeric(0),nrow=iter,ncol=14)
	mdp0 <- matrix(numeric(0),nrow=iter,ncol=14)
	mpp1 <- matrix(numeric(0),nrow=iter,ncol=14)
	mpp0 <- matrix(numeric(0),nrow=iter,ncol=14)
	
	if(dots) cat("\n\n\nBootstrap Iterations (",iter,") \n----|--- 1 ---|--- 2 ---|--- 3 ---|--- 4 ---| --- 5 \n",sep="") 
	start.time <- Sys.time()
	
	for (bb in 1:iter) {
		
		#-------------------------------------------
		# retrieve the bootstrap sample
		#-------------------------------------------
		bd <- merge(data.frame(ID=bootsmp[,bb]),data,by="ID",all.x=TRUE)
	
		#-------------------------------------------
		# Calculate mean point & period prevalence
		# by category at each recall period
		#-------------------------------------------
		mp1[bb,]  <- colMeans(bd[bd$C==1,31:44])
		mp0[bb,]  <- colMeans(bd[bd$C==0,31:44])
		mdp1[bb,] <- colMeans(bd[bd$C==1,17:30])
		mdp0[bb,] <- colMeans(bd[bd$C==0,17:30])
		mpp1[bb,] <- colMeans(bd[bd$C==1,3:16])
		mpp0[bb,] <- colMeans(bd[bd$C==0,3:16])
		
		if(dots) cat(".",sep="")
		if(dots && bb %% 50 == 0) cat(bb,"\n")
		
	}
	
	if(dots) cat("\n Bootstrap Run Time:",round(difftime(Sys.time(),start.time,units="mins"),3)," Minutes \n\n")
	
	
	# calculate "true" RRs and RDs
	# assuming the 2 day recall is truth
	tRRdp <- rep(mudp[1,2]/mudp[2,2],14)
	tRRpp <- rep(mupp[1,2]/mupp[2,2],14)
	tRDdp <- rep(mudp[1,2]-mudp[2,2],14)
	tRDpp <- rep(mupp[1,2]-mupp[2,2],14)
	
	# calculate empirical RR and RDs
	RRdp <- mudp[1,]/mudp[2,]
	RRpp <- mupp[1,]/mupp[2,]
	RDdp <- mudp[1,]-mudp[2,]
	RDpp <- mupp[1,]-mupp[2,]
	
	# calculate bias for RR and RDs
	bRRdp <- RRdp-tRRdp
	bRRpp <- RRpp-tRRpp
	bRDdp <- RDdp-tRDdp
	bRDpp <- RDpp-tRDpp
	
	# calculate bootstrapped SEs, variance of estimators
	mRRdp <- (mdp1/mdp0)
	mRRpp <- (mpp1/mpp0)
	mRDdp <- (mdp1-mdp0)
	mRDpp <- (mpp1-mpp0)
	
	RRdpSE <- apply(mRRdp,2,sd)
	RRppSE <- apply(mRRpp,2,sd)
	RDdpSE <- apply(mRDdp,2,sd)
	RDppSE <- apply(mRDpp,2,sd)
	
	RRdpV <- apply(mRRdp,2,var)
	RRppV <- apply(mRRpp,2,var)
	RDdpV <- apply(mRDdp,2,var)
	RDppV <- apply(mRDpp,2,var)
	
	
	# return results
	list(mup=mup,mudp=mudp,mupp=mupp, mp1=mp1,mp0=mp0,mdp1=mdp1,mdp0=mdp0,mpp1=mpp1,mpp0=mpp0, RRdp=RRdp,RRpp=RRpp,RDdp=RDdp,RDpp=RDpp, RRdpSE=RRdpSE,RRppSE=RRppSE,RDdpSE=RDdpSE,RDppSE=RDppSE, RRdpV=RRdpV, RRppV=RRppV, RDdpV=RDdpV, RDppV=RDppV, tRRdp=tRRdp,tRRpp=tRRpp,tRDdp=tRDdp,tRDpp=tRDpp, bRRdp=bRRdp,bRRpp=bRRpp,bRDdp=bRDdp,bRDpp=bRDpp)

}


#--------------------------------------
# Percent bias
# given the estimate of bias (bias) and
# the true value (true)
#--------------------------------------
pctbias <- function(true,bias) { 
	abs(bias/true)*100
}

#--------------------------------------
# Percent bias in RR
# given the estimate of bias (bias) and
# the true value (true)
#
# take the log of each RR first, then
# calculate % bias
#--------------------------------------
pctbiasRR <- function(true,bias) { 
	T <- log(true)
	B <- log(bias)
	
	abs(B/T)*100
}


#--------------------------------------
# Root Mean Squared Error
# given bias (B) and variance (V) of
# an estimator
#--------------------------------------

rmse <- function(B,V) { 
	sqrt(B^2+V)
}

#--------------------------------------
# Relative Root Mean Squared Error
# given bias (B) and variance (V) of
# an estimator
# and given a reference day (R)
#--------------------------------------

rrmse <- function(B,V,R) { 
	RMSE <- sqrt(B^2+V)
	RMSE/RMSE[R]
}

#--------------------------------------
# Relative Mean Squared Error
# given bias (B) and variance (V) of
# an estimator
# and given a reference day (R)
#--------------------------------------

rMSE <- function(B,V,R) { 
	MSE <- B^2+V
	MSE/MSE[R]
}

#--------------------------------------
# Output summary statistics to HTML
# Tables (.xls files)
#--------------------------------------


empirical.tables <- function(outcome,Xvar) {
	# outcome : string argument for outcome (diar, cough, fever)
	#Xvar     : string argument for stratification variable (anemia, haz, waz)
	
require(xtable)

dir <- paste("~/dropbox/manuscripts/wsp-recall/programs/empirical/",outcome,"/",sep="")
dfile <- paste(dir,"recall-empirical-",outcome,"-",Xvar,".RData",sep="")
outfile <- paste("~/dropbox/manuscripts/wsp-recall/tables/rawoutput/empirical-tables-",outcome,"-",Xvar,".xls",sep="")

clabels <- c("India HP","India MP","Indonesia","Peru","Senegal")

load(dfile)
aihp <- ihp
aimp <- imp
aind <- ind
aper <- per
asen <- sen


#--------------------------------------
# RR period prevalence Tables
#--------------------------------------
RRpp <- matrix(c(ihp$RRpp,imp$RRpp,ind$RRpp,per$RRpp,sen$RRpp),nrow=14,ncol=5)

RRpp.se <- matrix(c(ihp$RRppSE,imp$RRppSE,ind$RRppSE,per$RRppSE,sen$RRppSE),nrow=14,ncol=5)

RRpp.bias <- matrix(c(ihp$bRRpp,imp$bRRpp,ind$bRRpp,per$bRRpp,sen$bRRpp),nrow=14,ncol=5)

RRpp.biaspct <- matrix(c(pctbias(ihp$tRRpp,ihp$bRRpp),pctbias(imp$tRRpp,imp$bRRpp),pctbias(ind$tRRpp,ind$bRRpp),pctbias(per$tRRpp,per$bRRpp),pctbias(sen$tRRpp,sen$bRRpp)),nrow=14,ncol=5)

RRpp.rmse <- matrix(c(rmse(ihp$bRRpp,ihp$RRppV), rmse(imp$bRRpp,imp$RRppV), rmse(ind$bRRpp,ind$RRppV), rmse(per$bRRpp,per$RRppV), rmse(sen$bRRpp,sen$RRppV)),nrow=14,ncol=5)

RRpp.rMSE <- matrix(c(rMSE(ihp$bRRpp,ihp$RRppV,2), rMSE(imp$bRRpp,imp$RRppV,2), rMSE(ind$bRRpp,ind$RRppV,2), rMSE(per$bRRpp,per$RRppV,2), rMSE(sen$bRRpp,sen$RRppV,2)),nrow=14,ncol=5)


# export to HTML
colnames(RRpp) <- colnames(RRpp.se) <- colnames(RRpp.bias) <- colnames(RRpp.biaspct) <- colnames(RRpp.rmse) <- colnames(RRpp.rMSE) <- clabels

print(xtable(RRpp,digits=4,caption=paste("RR period prevalence (File created by recall-empirical-tables.R / ",outcome,", ",Xvar,")",sep="")),type="html",file=outfile,caption="top")

print(xtable(RRpp.se,digits=4,caption="RR period prevalence SE"),type="html",file=outfile,caption="top",append=TRUE)

print(xtable(RRpp.bias,digits=4,caption="RR period prevalence BIAS"),type="html",file=outfile,caption="top",append=TRUE)

print(xtable(RRpp.biaspct,digits=4,caption="RR period prevalence % BIAS"),type="html",file=outfile,caption="top",append=TRUE)

print(xtable(RRpp.rmse,digits=4,caption="RR period prevalence root MSE"),type="html",file=outfile,caption="top",append=TRUE)

print(xtable(RRpp.rMSE,digits=4,caption="RR period prevalence relative MSE (day 2 ref)"),type="html",file=outfile,caption="top",append=TRUE)


#--------------------------------------
# RR daily prevalence Tables
#--------------------------------------
RRdp <- matrix(c(ihp$RRdp,imp$RRdp,ind$RRdp,per$RRdp,sen$RRdp),nrow=14,ncol=5)

RRdp.se <- matrix(c(ihp$RRdpSE,imp$RRdpSE,ind$RRdpSE,per$RRdpSE,sen$RRdpSE),nrow=14,ncol=5)

RRdp.bias <- matrix(c(ihp$bRRdp,imp$bRRdp,ind$bRRdp,per$bRRdp,sen$bRRdp),nrow=14,ncol=5)

RRdp.biaspct <- matrix(c(pctbias(ihp$tRRdp,ihp$bRRdp),pctbias(imp$tRRdp,imp$bRRdp),pctbias(ind$tRRdp,ind$bRRdp),pctbias(per$tRRdp,per$bRRdp),pctbias(sen$tRRdp,sen$bRRdp)),nrow=14,ncol=5)

RRdp.rmse <- matrix(c(rmse(ihp$bRRdp,ihp$RRdpV), rmse(imp$bRRdp,imp$RRdpV), rmse(ind$bRRdp,ind$RRdpV), rmse(per$bRRdp,per$RRdpV), rmse(sen$bRRdp,sen$RRdpV)),nrow=14,ncol=5)

RRdp.rMSE <- matrix(c(rMSE(ihp$bRRdp,ihp$RRdpV,2), rMSE(imp$bRRdp,imp$RRdpV,2), rMSE(ind$bRRdp,ind$RRdpV,2), rMSE(per$bRRdp,per$RRdpV,2), rMSE(sen$bRRdp,sen$RRdpV,2)),nrow=14,ncol=5)


# export to HTML
colnames(RRdp) <- colnames(RRdp.se) <- colnames(RRdp.bias) <- colnames(RRdp.biaspct) <- colnames(RRdp.rmse) <- colnames(RRdp.rMSE) <- clabels

print(xtable(RRdp,digits=4,caption="RR daily prevalence"),type="html",file=outfile,caption="top",append=TRUE)

print(xtable(RRdp.se,digits=4,caption="RR daily prevalence SE"),type="html",file=outfile,caption="top",append=TRUE)

print(xtable(RRdp.bias,digits=4,caption="RR daily prevalence BIAS"),type="html",file=outfile,caption="top",append=TRUE)

print(xtable(RRdp.biaspct,digits=4,caption="RR daily prevalence % BIAS"),type="html",file=outfile,caption="top",append=TRUE)

print(xtable(RRdp.rmse,digits=4,caption="RR daily prevalence root MSE"),type="html",file=outfile,caption="top",append=TRUE)

print(xtable(RRdp.rMSE,digits=4,caption="RR daily prevalence relative MSE (day 2 ref)"),type="html",file=outfile,caption="top",append=TRUE)



#--------------------------------------
# RD period prevalence Tables
#--------------------------------------
RDpp <- matrix(c(ihp$RDpp,imp$RDpp,ind$RDpp,per$RDpp,sen$RDpp),nrow=14,ncol=5)

RDpp.se <- matrix(c(ihp$RDppSE,imp$RDppSE,ind$RDppSE,per$RDppSE,sen$RDppSE),nrow=14,ncol=5)

RDpp.bias <- matrix(c(ihp$bRDpp,imp$bRDpp,ind$bRDpp,per$bRDpp,sen$bRDpp),nrow=14,ncol=5)

RDpp.biaspct <- matrix(c(pctbias(ihp$tRDpp,ihp$bRDpp),pctbias(imp$tRDpp,imp$bRDpp),pctbias(ind$tRDpp,ind$bRDpp),pctbias(per$tRDpp,per$bRDpp),pctbias(sen$tRDpp,sen$bRDpp)),nrow=14,ncol=5)

RDpp.rmse <- matrix(c(rmse(ihp$bRDpp,ihp$RDppV), rmse(imp$bRDpp,imp$RDppV), rmse(ind$bRDpp,ind$RDppV), rmse(per$bRDpp,per$RDppV), rmse(sen$bRDpp,sen$RDppV)),nrow=14,ncol=5)

RDpp.rMSE <- matrix(c(rMSE(ihp$bRDpp,ihp$RDppV,2), rMSE(imp$bRDpp,imp$RDppV,2), rMSE(ind$bRDpp,ind$RDppV,2), rMSE(per$bRDpp,per$RDppV,2), rMSE(sen$bRDpp,sen$RDppV,2)),nrow=14,ncol=5)

# export to HTML
colnames(RDpp) <- colnames(RDpp.se) <- colnames(RDpp.bias) <- colnames(RDpp.biaspct) <- colnames(RDpp.rmse) <- colnames(RDpp.rMSE) <- clabels

print(xtable(RDpp,digits=4,caption="RD period prevalence"),type="html",file=outfile,caption="top",append=TRUE)

print(xtable(RDpp.se,digits=4,caption="RD period prevalence SE"),type="html",file=outfile,caption="top",append=TRUE)

print(xtable(RDpp.bias,digits=4,caption="RD period prevalence BIAS"),type="html",file=outfile,caption="top",append=TRUE)

print(xtable(RDpp.biaspct,digits=4,caption="RD period prevalence % BIAS"),type="html",file=outfile,caption="top",append=TRUE)

print(xtable(RDpp.rmse,digits=4,caption="RD period prevalence root MSE"),type="html",file=outfile,caption="top",append=TRUE)

print(xtable(RDpp.rMSE,digits=4,caption="RD period prevalence relative MSE (day 2 ref)"),type="html",file=outfile,caption="top",append=TRUE)



#--------------------------------------
# RD daily prevalence Tables
#--------------------------------------
RDdp <- matrix(c(ihp$RDdp,imp$RDdp,ind$RDdp,per$RDdp,sen$RDdp),nrow=14,ncol=5)

RDdp.se <- matrix(c(ihp$RDdpSE,imp$RDdpSE,ind$RDdpSE,per$RDdpSE,sen$RDdpSE),nrow=14,ncol=5)

RDdp.bias <- matrix(c(ihp$bRDdp,imp$bRDdp,ind$bRDdp,per$bRDdp,sen$bRDdp),nrow=14,ncol=5)

RDdp.biaspct <- matrix(c(pctbias(ihp$tRDdp,ihp$bRDdp),pctbias(imp$tRDdp,imp$bRDdp),pctbias(ind$tRDdp,ind$bRDdp),pctbias(per$tRDdp,per$bRDdp),pctbias(sen$tRDdp,sen$bRDdp)),nrow=14,ncol=5)

RDdp.rmse <- matrix(c(rmse(ihp$bRDdp,ihp$RDdpV), rmse(imp$bRDdp,imp$RDdpV), rmse(ind$bRDdp,ind$RDdpV), rmse(per$bRDdp,per$RDdpV), rmse(sen$bRDdp,sen$RDdpV)),nrow=14,ncol=5)

RDdp.rMSE <- matrix(c(rMSE(ihp$bRDdp,ihp$RDdpV,2), rMSE(imp$bRDdp,imp$RDdpV,2), rMSE(ind$bRDdp,ind$RDdpV,2), rMSE(per$bRDdp,per$RDdpV,2), rMSE(sen$bRDdp,sen$RDdpV,2)),nrow=14,ncol=5)


# export to HTML
colnames(RDdp) <- colnames(RDdp.se) <- colnames(RDdp.bias) <- colnames(RDdp.biaspct) <- colnames(RDdp.rmse) <- colnames(RDdp.rMSE) <- clabels

print(xtable(RDdp,digits=4,caption="RD daily prevalence"),type="html",file=outfile,caption="top",append=TRUE)

print(xtable(RDdp.se,digits=4,caption="RD daily prevalence SE"),type="html",file=outfile,caption="top",append=TRUE)

print(xtable(RDdp.bias,digits=4,caption="RD daily prevalence BIAS"),type="html",file=outfile,caption="top",append=TRUE)

print(xtable(RDdp.biaspct,digits=4,caption="RD daily prevalence % BIAS"),type="html",file=outfile,caption="top",append=TRUE)

print(xtable(RDdp.rmse,digits=4,caption="RD daily prevalence root MSE"),type="html",file=outfile,caption="top",append=TRUE)

print(xtable(RDdp.rMSE,digits=4,caption="RD daily prevalence relative MSE (day 2 ref)"),type="html",file=outfile,caption="top",append=TRUE)

}


