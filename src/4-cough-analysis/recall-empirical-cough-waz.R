

#--------------------------------------
# recall-empirical-cough-waz.R
# Ben Arnold
# 20 Oct 2011
#
# Calculate the empirical risk ratio
# and risk difference for coughrhea
# over different recall periods 
# stratified by whether the child is
# below -2 WAZ
#
# Updated 24 May 2012 (finalized directories)
#--------------------------------------


library(foreign)

rm(list=ls())

#--------------------------------------
# Load base functions
#--------------------------------------
source("~/dropbox/articles/wsp-recall/programs/final/2-base-functions/empirical-base-functions-v2.R")

#--------------------------------------
# Load dataset
#--------------------------------------
d <- read.dta("~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta")

# get point and period prevalence matrices
dp <- d[,c(paste("cough0",1:9,sep=""),paste("cough1",0:4,sep="")) ]
pp <- d[,c(paste("cough0",1:9,"pp",sep=""),paste("cough1",0:4,"pp",sep="")) ]

	
# create a matrix with mean point prevalence over different recall periods
pmu <- matrix(numeric(0),nrow=nrow(dp),ncol=ncol(dp))
pmu[,1] <- dp[,1]
for (i in 2:14) {
	pmu[,i] <- rowMeans(dp[,c(1:i)])
}

# calculate average episode durations for each country
# limit calculations to episodes that terminated in the last 7 days to avoid length biased sampling
# truncate the distribution for episodes shorter than 30 days
Dihp <- mean(d$coughlen[d$dataset=="India HP" & d$coughlen<30 & d$cough00==0 & d$cough07pp==1],na.rm=TRUE)
Dimp <- mean(d$coughlen[d$dataset=="India MP" & d$coughlen<30 & d$cough00==0 & d$cough07pp==1],na.rm=TRUE)
Dind <- mean(d$coughlen[d$dataset=="Indonesia" & d$coughlen<30 & d$cough00==0 & d$cough07pp==1],na.rm=TRUE)
Dper <- mean(d$coughlen[d$dataset=="Peru" & d$coughlen<30 & d$cough00==0 & d$cough07pp==1],na.rm=TRUE)
Dsen <- mean(d$coughlen[d$dataset=="Senegal" & d$coughlen<30 & d$cough00==0 & d$cough07pp==1],na.rm=TRUE)


# Restrict to non-missing values for WAZ
dp <- dp[!is.na(d$waz2),]
pp <- pp[!is.na(d$waz2),]
pmu <- pmu[!is.na(d$waz2),]
waz2 <- d$waz2[!is.na(d$waz2)]
cluster <- d$cluster[!is.na(d$waz2)]
dataset <- d$dataset[!is.na(d$waz2)]

#--------------------------------------
# Calculate bootstrapped RR/RD
# and prevalence means + SEs
# for each country
#--------------------------------------
nboot <- 10000
set.seed(6959)
ihp <- RDRR(dp=dp,pp=pp,pmu=pmu,C=waz2,D=Dihp,ID=cluster,dataset=dataset,country="India HP",iter=nboot)
imp <- RDRR(dp=dp,pp=pp,pmu=pmu,C=waz2,D=Dimp,ID=cluster,dataset=dataset,country="India MP",iter=nboot)
ind <- RDRR(dp=dp,pp=pp,pmu=pmu,C=waz2,D=Dind,ID=cluster,dataset=dataset,country="Indonesia", iter=nboot)
per <- RDRR(dp=dp,pp=pp,pmu=pmu,C=waz2,D=Dper,ID=cluster,dataset=dataset,country="Peru",iter=nboot)
sen <- RDRR(dp=dp,pp=pp,pmu=pmu,C=waz2,D=Dsen,ID=cluster,dataset=dataset,country="Senegal",iter=nboot)



#--------------------------------------
# Save objects
#--------------------------------------
save.image("~/dropbox/articles/wsp-recall/programs/final/4-cough-analysis/recall-empirical-cough-waz.RData")



