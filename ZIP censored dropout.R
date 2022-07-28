##Libraries

library(runjags)
library(rjags)
library(gplots)

##Setting working directory



##Data
data=read.table("data.txt",header=T,sep="\t",dec=",")
attach(data)

GI<-scale(GI)
OM<-scale(OM)	
Age<-scale(Age)
EnrolmentFee<-scale(EnrolmentFeeInCOP)	
GeneralTestScore<-scale(GeneralTestScore)		
Biology<-scale(Biology)	
HistoryAndGeography<-scale(HistoryAndGeography)	
Philosophy<-scale(Philosophy)	
Phisics<-scale(Phisics)	
English<-scale(English)	
Language<-scale(Language)	
Math<-scale(Math)	
Chemistry<-scale(Chemistry)

X<-cbind(1,GI,OM,Gender,Age,TypeOfHighSchool,GeneralTestScore,Biology,HistoryAndGeography,Philosophy,Phisics,English,Language,Math,Chemistry,EnrolmentFee,PlaceOfOrigin)
n=nrow(X)
cutoff<-NumberOfCoursesPassed
n.Chains=3
sampleSize=20000
response<-ifelse(Censored==0,cutoff,NA)

##Model 1: censored ZIP: mu and p with covariates 

model.zip.cen<-'model{
	for(j in 1:n){
		Censored[j]~dinterval(response[j],cutoff[j])
		response[j] ~ dpois(mu[j])
		mu[j]<-g[j]*lambda[j]
		g[j]~dbern(1-p[j])
		p[j]<-max(0.00000000001,min(0.99999999999,Q1[j]))
		logit(Q1[j])<- gamma%*%X[j,]
		log(lambda[j])<- beta%*%X[j,]
}

for (i in 1:17){
	beta[i]~dnorm(0,0.01)
	gamma[i]~dnorm(0,0.01)
	}
}
'
##Data

datas <- list ("X"=X,"n"=n,"response"=response,"Censored"=Censored,"cutoff"=cutoff)

##Initial values

inits <- function(){list(beta=rep(0,17),gamma=rep(0,17),g=rep(1,n),response=ifelse(Censored==1,max(cutoff)+1,NA))} # )}#

##Parameters

param<-c("beta","gamma")

##Bayesian approach

model.out<-run.jags(model=model.zip.cen,monitor= param,data= datas,inits= inits,n.chains=3,sample=20000,adapt=5000,burnin=5000,thin=5,modules='runjags',jags.refresh=40)

print(model.out)


             