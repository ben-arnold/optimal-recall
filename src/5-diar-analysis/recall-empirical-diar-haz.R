

#--------------------------------------
# recall-empirical-diar-haz.R
# Ben Arnold
# 23 Sep 2011
#
# Calculate the empirical risk ratio
# and risk difference for diarrhea
# over different recall periods 
# stratified by whether the child is
# below -2 HAZ
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
dp <- d[,c(paste("diar0",1:9,sep=""),paste("diar1",0:4,sep="")) ]
pp <- d[,c(paste("diar0",1:9,"pp",sep=""),paste("diar1",0:4,"pp",sep="")) ]

	
# create a matrix with mean point prevalence over different recall periods
pmu <- matrix(numeric(0),nrow=nrow(dp),ncol=ncol(dp))
pmu[,1] <- dp[,1]
for (i in 2:14) {
	pmu[,i] <- rowMeans(dp[,c(1:i)])
}

# calculate average episode durations for each country
# limit calculations to episodes that terminated in the last 7 days to avoid length biased sampling
# truncate the distribution for episodes shorter than 30 days
Dihp <- mean(d$diarlen[d$dataset=="India HP" & d$diarlen<30 & d$diar00==0 & d$diar07pp==1],na.rm=TRUE)
Dimp <- mean(d$diarlen[d$dataset=="India MP" & d$diarlen<30 & d$diar00==0 & d$diar07pp==1],na.rm=TRUE)
Dind <- mean(d$diarlen[d$dataset=="Indonesia" & d$diarlen<30 & d$diar00==0 & d$diar07pp==1],na.rm=TRUE)
Dper <- mean(d$diarlen[d$dataset=="Peru" & d$diarlen<30 & d$diar00==0 & d$diar07pp==1],na.rm=TRUE)
Dsen <- mean(d$diarlen[d$dataset=="Senegal" & d$diarlen<30 & d$diar00==0 & d$diar07pp==1],na.rm=TRUE)


# Restrict to non-missing values for HAZ
dp <- dp[!is.na(d$haz2),]
pp <- pp[!is.na(d$haz2),]
pmu <- pmu[!is.na(d$haz2),]
haz2 <- d$haz2[!is.na(d$haz2)]
cluster <- d$cluster[!is.na(d$haz2)]
dataset <- d$dataset[!is.na(d$haz2)]

#--------------------------------------
# Calculate bootstrapped RR/RD
# and prevalence means + SEs
# for each country
#--------------------------------------
nboot <- 10000
set.seed(345634)
ihp <- RDRR(dp=dp,pp=pp,pmu=pmu,C=haz2,D=Dihp,ID=cluster,dataset=dataset,country="India HP",iter=nboot)
imp <- RDRR(dp=dp,pp=pp,pmu=pmu,C=haz2,D=Dimp,ID=cluster,dataset=dataset,country="India MP",iter=nboot)
ind <- RDRR(dp=dp,pp=pp,pmu=pmu,C=haz2,D=Dind,ID=cluster,dataset=dataset,country="Indonesia",iter=nboot)
per <- RDRR(dp=dp,pp=pp,pmu=pmu,C=haz2,D=Dper,ID=cluster,dataset=dataset,country="Peru",iter=nboot)
sen <- RDRR(dp=dp,pp=pp,pmu=pmu,C=haz2,D=Dsen,ID=cluster,dataset=dataset,country="Senegal",iter=nboot)



#--------------------------------------
# Save objects
#--------------------------------------
save.image("~/dropbox/articles/wsp-recall/programs/final/5-diar-analysis/recall-empirical-diar-haz.RData")



