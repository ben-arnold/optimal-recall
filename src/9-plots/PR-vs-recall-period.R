



#---------------------------------------
# Prevalence ratio and risk difference
# parameter estimates based on period 
# prevalence with different recall periods
# Example used in the appendix of the article:
# Optimal recall period for caregiver-reported illness in risk factor and intervention studies: a multicountry study
#---------------------------------------

library(xtable)
library(RColorBrewer)

# incidence
I1 <- 4/365
I2 <- 5/365

I1
I2

# duration
D1 <- 3
D2 <- 3

Pfn <- function(I,D) {
	I*D/(1+I*D)
}

PTfn <- function(T,P,I) {
	P + I*(T-1)
}

# prevalence
P1 <- Pfn(I1,D1)
P2 <- Pfn(I2,D2)

P1
P2

# time period
T <- 1:14

PT1 <- sapply(T,PTfn,P=P1,I=I1)
PT2 <- sapply(T,PTfn,P=P2,I=I2)

# risk difference, risk ratio (or prevalence ratio)
RD <- PT1 - PT2
RR <- PT1 / PT2

RD

RR


plot(RR)
abline(I1/I2,0)



tab <- matrix(sprintf("%1.3f",c(PT2,PT1,RD,RR)),nrow=14,ncol=4)

colnames(tab) <- c("Control","Intervention","RD","RR")
rownames(tab) <- 1:14

xtable(tab)

#---------------------------------------
# 13 Apr 2012 -
# in response to AJE review, generalize the trend for different baseline incidence
#--------------------------------------

RRs <- matrix(numeric(),nrow=14,ncol=5)
j <- 1
for (i in seq(10,2,by=-2)) {
	I2 <- i/365
	I1 <- I2*0.8
	D <- 3
	P1 <- Pfn(I1,D)
	P2 <- Pfn(I2,D)
	T <- 1:14
	PT1 <- sapply(T,PTfn,P=P1,I=I1)
	PT2 <- sapply(T,PTfn,P=P2,I=I2)
	RRs[,j] <- PT1/PT2
	j <- j+1
	
}

cols <- brewer.pal(5,"Set1")

pdf("~/dropbox/articles/wsp-recall/figures/PR-by-recall-period.pdf")
matplot(RRs,type="l",
	col="black",
	ylim=c(0.80,0.815),
	ylab="Prevalence Ratio",
	xaxt="n",xlab="Period Prevalence Recall Period (days)",
	las=1,bty="l"
	)
axis(1,at=1:14)
abline(0.8,0,lwd=2)
legend("top",title="Episodes per year",legend=paste(seq(10,2,by=-2)),lty=1:5,bty="n")
dev.off()


