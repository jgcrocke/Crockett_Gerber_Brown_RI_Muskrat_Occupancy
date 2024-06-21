#####
#Loading in data
#####
rm(list=ls())
setwd("C:/Users/John Crockett/Documents/GitHub/Crockett_Gerber_Brown_RI_Muskrat_Occupancy")
MuskData=read.csv("Analyses and R Files/TTDAllYrs_cleaned.csv")
surveyed=read_sf("Site Shapefiles/Surveyed_NLCD.shp")
#####
#Get down to just muskrats
#####
MuskData$Spp[which(MuskData$Spp=="Muskrat")]="muskrat"
MuskData$TTD[MuskData$Spp!="muskrat"]=NA
MuskData=MuskData[,c(3:6,11:29)]
MuskData=MuskData[!duplicated(MuskData[,-15]),]

# Make sure that the times are valid (TTD!=0,YMax!<TTD)
min(MuskData$TTD,na.rm=T)
MuskData$TTD[MuskData$TTD<1]=0.1
min(MuskData$YMax)
min(MuskData$YMax- MuskData$TTD,na.rm=T)
hist(MuskData$TTD);hist(MuskData$YMax)

#Making an index so we're certain the sites are right
UNQ=unique(MuskData[,1:2])
MuskData$Index=NA
for (i in 1:length(UNQ$Watershed)){
  MuskData$Index[MuskData$Watershed==UNQ$Watershed[i]&
                   MuskData$Site.ID==UNQ$Site.ID[i]]=i
  }

surveyed$keep=0
for (i in 1:277){
  if(sum(UNQ$Site.ID==surveyed$Site_ID[i],na.rm = T)>0){
    surveyed$keep[i]=1
  }
}
surveyed=surveyed[surveyed$keep==1,]
#####
#Standardizing Covariates
#####
COVS=st_drop_geometry(surveyed[,c(1:2,4:12,22:23)])
COVS$Index=1:276
meanimp=mean(COVS$N19_Urb)
sdimp=sqrt(var(COVS$N19_Urb))
COVS$Bimp=(COVS$N19_Urb-meanimp)/sdimp

meanforest=mean(COVS$N19_FWt)
sdforest=sqrt(var(COVS$N19_FWt))
COVS$Bforest=(COVS$N19_FWt-meanforest)/sdforest

meanswamp=mean(COVS$N19_OWt)
sdswamp=sqrt(var(COVS$N19_OWt))
COVS$Bswamp=(COVS$N19_OWt-meanswamp)/sdswamp



meanh2o=mean(COVS$N19_Wtr)
sdh2o=sqrt(var(COVS$N19_Wtr))
COVS$Bh2o=(COVS$N19_Wtr-meanh2o)/sdh2o

COVS$BStrm=ifelse(COVS$StrmOrd>1,1,0)
COVS$BStrm[is.na(COVS$BStrm)]=0
ocovs=MuskData[,8:16]
for (i in 1:9){
  ocovs[,i]=(MuskData[,i+7]-mean(MuskData[,i+7]))/
    sqrt(var(MuskData[,i+7]))
}




#####
#Making the Effect Coding for Observer
#####
wshed=as.factor(COVS$Watrshd)
levels(wshed)

unique(MuskData$Observer)
MuskData$Observer[MuskData$Observer=="Emerson Paton"|
                    MuskData$Observer=="Emma Paton"|
                    MuskData$Observer=="Charlie Brown"|
                    MuskData$Observer=="Morgan Lucot"|
                    MuskData$Observer=="Richard Mercer"]="Other Observer"
obs=as.factor(MuskData$Observer)
levels(obs)
OBS=model.matrix(~obs,contrasts = list(obs = contr.sum))
colnames(OBS)=c("Int",levels(obs)[1:6])
#####
#Bits and bobs to give the model
#####
d <- as.numeric(is.na(MuskData$TTD)) # Censoring indicator
M=length(UNQ$Watershed)
nobs=length(MuskData$TTD)
TTDF=vector(length = nobs)
for (i in 1:nobs){
  TTDF[i]=which(COVS$Index==MuskData$Index[i])
}
max(TTDF)
#####
#Packages
#####
library(rjags)
library(runjags)
library(mcmcplots)
library(MCMCvis)
## Set model
model_string <-"model
{
  
# Likelihood
for (m in 1:M){ # Model for occurrence at site level
z[m] ~ dbern(psi[m])
logit(psi[m])=psi.int+beta1*imp[m]+beta2*forest[m]+beta3*swamp[m]+
        beta4*salt[m]+beta5*fresh[m]+beta6*Strm[m]+betaw[WSHED[m]]
}
for (w in 1:7){
betaw[w]~dnorm(0,psi.tau)
}
for (i in 1:nobs){ # Observation model at observation level
# Weibull model for time to detection ignoring censoring
ttd[i] ~ dweib(shape,lambda[i])
log(lambda[i]) <- log(lambda.int)+alpha1*OBS[i,2]+alpha2*OBS[i,3]+
                  alpha3*OBS[i,4]+alpha4*OBS[i,5]+alpha5*OBS[i,6]+
                  alpha6*OBS[i,7]+alpha7*water[TTDF[i]]+
                  alpha8*built[TTDF[i]]+alpha9*wetland[TTDF[i]]+
                  alpha10*temp[TTDF[i]]+alpha11*cloud[TTDF[i]]+
                  alpha12*precip[TTDF[i]]+alpha13*kayak[TTDF[i]]

# Model for censoring due to species absence or ttd>Tmax
d[i] ~ dbern(theta[i])
theta[i] <- z[TTDF[i]] * step(ttd[i] - Tmax[i]) + (1 - z[TTDF[i]])
}
# Priors
psi.int ~ dlogis(0,1) # Occupancy intercept
beta1~dlogis(0,1)
beta2~dlogis(0,1)
beta3~dlogis(0,1)
beta4~dlogis(0,1)
beta5~dlogis(0,1)
beta6~dlogis(0,1)
psi.tau~dgamma(1,1)
lambda.int ~ dgamma(.001,.001) 
alpha1~dnorm(0, 0.3)
alpha2~dnorm(0, 0.3)
alpha3~dnorm(0, 0.3)
alpha4~dnorm(0, 0.3)
alpha5~dnorm(0, 0.3)
alpha6~dnorm(0,0.3)
alpha7~dnorm(0,0.3)
alpha8~dnorm(0,0.3)
alpha9~dnorm(0,0.3)
alpha10~dnorm(0,0.3)
alpha11~dnorm(0,0.3)
alpha12~dnorm(0,0.3)
alpha13~dnorm(0,0.3)
shape~dgamma(1,1)

psim=mean(psi)
}
"
data <- list(M=M,
             ttd = MuskData$TTD,
             TTDF=TTDF,
             d=d,nobs=nobs,
             Tmax=MuskData$YMax,
             forest=COVS$Bforest,
             imp=COVS$Bimp,
             swamp=COVS$Bswamp,
             salt=COVS$Salt,
             fresh=COVS$Bh2o,
             WSHED=as.numeric(wshed),
             OBS=OBS,
             Strm=COVS$BStrm,
             built=ocovs$Built,
             wetland=ocovs$Wetland,
             water=ocovs$Water,
             temp=ocovs$Temp,
             cloud=ocovs$cloudcover,
             precip=ocovs$Precip,
             kayak=as.numeric(as.factor(MuskData$Mode))-1)
zst <- rep(1, data$M)
ttdst <-rep(data$Tmax+1)
ttdst[data$d == 0] <- NA
inits=function(){list(z =zst, ttd = ttdst,
                      psi.int=runif(1),
                      lambda.int=runif(1),
                      alpha1=rnorm(1),
                      alpha2=rnorm(1),
                      alpha3=rnorm(1),
                      alpha4=rnorm(1),
                      alpha5=rnorm(1),
                      alpha6=rnorm(1),
                      alpha7 =rnorm(1),
                      alpha8 =rnorm(1),
                      alpha9 =rnorm(1),
                      alpha10=rnorm(1),
                      alpha11=rnorm(1),
                      alpha12=rnorm(1),
                      alpha13=rnorm(1),
                      beta1=rnorm(1),
                      beta2=rnorm(1),
                      beta3=rnorm(1),
                      beta4=rnorm(1),
                      beta5=rnorm(1),
                      beta6=rnorm(1),
                      betaw=rnorm(7),
                      psi.tau=runif(1))}
Muskrat.TTD=run.jags(model=model_string,
                     monitor= c("psim","psi.int","alpha1","alpha2",
                                "alpha3","alpha4","alpha5","alpha6",
                                "alpha7","alpha8","alpha9","alpha10",
                                "alpha11","alpha12","alpha13",
                                "beta1","beta2","beta3",
                                "beta4","beta5","beta6","betaw","psi.tau",
                                "lambda.int",
                                "shape"),
                     burnin=10000, sample=20000,
                     data=data, n.chains=4, method="rjags", inits=inits)

save(Muskrat.TTD,file="MuskratTTD_nlcd.Rdata")
save.image(file = "MuskratTTD_For_Graphs.Rdata")
